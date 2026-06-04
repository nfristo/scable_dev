# =========================
# SCRIPT LAND N°3 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif d'ajouter les caractéristiques des chantiers terrestres aux données.
# =========================

# =========================
# 1. PACKAGES
# =========================
library(readr)
library(dplyr)
library(sf)
library(ggplot2)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
commandes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP_terrestres.csv")
terre_tarif_presta_complet <- read_csv ("~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_tarif_presta_complet.csv")
grumes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/GRUMES_terrestres.csv")
contour_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO_terrestres.csv")
tige_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/TIGES_terrestres.csv")


# =========================
# 3. CARACTÉRISTIQUES DES GRUMES DEBARDÉES 
# =========================

# Essence majoritaire des grumes marquées pour le débardage
ess_maj <- tige_terre |>
  filter(!is.na(ESS), !is.na(NB_TIGES)) |>
  group_by(ID_FB, ESS) |>
  summarise(nb = sum(NB_TIGES), .groups = "drop") |>
  group_by(ID_FB) |>
  slice_max(nb, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ID_FB, ESSENCE_MAJ = ESS)

# Indicateurs principaux caractérisant les tiges marquées
stats <- tige_terre |>
  mutate(
    vol_unitaire = ifelse(NB_TIGES > 1,
                          VOL_TIGE / NB_TIGES,
                          VOL_TIGE)
  ) |>
  group_by(ID_FB) |>
  summarise(
    NB_GRUMES = sum(NB_TIGES, na.rm = TRUE), # Nombre de tiges marquées par chantier
    DIAM_MOY = sum(DIAM * NB_TIGES, na.rm = TRUE) / sum(NB_TIGES, na.rm = TRUE),  #Moyenne du diamètre des grumes marquées par chantier 
    VOL_MOYEN_GRUME = sum(vol_unitaire * NB_TIGES, na.rm = TRUE) / sum(NB_TIGES, na.rm = TRUE), #Moyenne du volume des grumes marquées par chantier
    G_TOT_CUT = sum(G, na.rm = TRUE), #Surface terrière totale coupée ATTENTION ce n'est pas la surface terrière du peuplemen
    
    .groups = "drop"
  )

gr_terre_stats <- left_join(stats, ess_maj, by = "ID_FB")

prix_chantier <- grumes_terre |>
  group_by(ID_FB) |>
  summarise(
    PRIX_VENTE_TOT = if(all(is.na(PRIX_VENTE))) NA_real_ # Prix de vente total des grumes par chantier
    else sum(PRIX_VENTE, na.rm = TRUE),
    .groups = "drop"
  )

gr_terre_stats <- gr_terre_stats |>
  left_join(prix_chantier, by = "ID_FB")
gr_terre_stats <- gr_terre_stats |>
  mutate(
    PRIX_VENTE_GRUME_TERRE = PRIX_VENTE_TOT / NB_GRUMES #Moyenne du prix de vente par grumes par chantier
  )


# =========================
# 4. CARACTÉRISTIQUES DES PEUPLEMENTS
# =========================

peupl_terre_stats <- contour_terre |>
  group_by(ID_FB) |>
  summarise(
    VOL_HA_MOY = mean(VOL_HA, na.rm = TRUE), #Volume moyen à l'hectare du peuplement  
    HA_DES_TOT = sum(HA_DES, na.rm = TRUE), #Surface du peuplement en hectare désigné par le TFT  
  )

peupl_terre_stats2 <- contour_terre |>
  group_by(ID_FB) |>
  summarise(
    FAMILLE_COUPE = names(sort(table(FAMILLE_COUPE), decreasing = TRUE))[1], #Famille de coupe de la coupe effectuée
    PEUPLEMENT_STR = names(sort(table(PEUPLEMENT_MODE), decreasing = TRUE))[1]) #Structure du peuplement

# =========================
# 5. ASSEMBLAGE DE TOUS LES TABLEURS PRÉCÉDENTS AVEC ID CHANTIERS 
# ET CALCUL DE VARIABLES SUPPLÉMENTAIRES
# =========================

commandes_terre_id <- commandes_terre |>
  group_by(ID_FB) |>
  summarise(
    NUM_ETF = first(NUM_ETF),
    CENTRE_PROFIT = first(CENTRE_PROFIT),
    .groups = "drop"
  )

df_terre_inter <- commandes_terre_id |>
  left_join(terre_tarif_presta_complet, by = "ID_FB") |>
  left_join(gr_terre_stats, by = "ID_FB") |>
  left_join(peupl_terre_stats, by = "ID_FB") |>
  left_join(peupl_terre_stats2, by = "ID_FB")

#Pour pouvoir calculer des données spatiales :
# On ajoute les polygones de chaque chantier dans le df_terre, en les regroupant par chantier :

contour_sf_terre <- contour_terre |>
  st_as_sf(wkt = "CONTOUR", crs = 4326) |>
  select(ID_FB, CONTOUR)
contour_terre2 <- contour_sf_terre |>
  group_by(ID_FB) |>
  summarise(CONTOUR = st_union(CONTOUR))

df_terre_inter <- df_terre_inter |>
  left_join(contour_terre2, by = "ID_FB")

#Les coordonnées des polygones sont extraites pour pouvoir être utilisées en analyse statistiques.
df_terre_inter <- df_terre_inter |>
  st_as_sf() |>
  st_transform(2154) #transformation des données WGS 84 en Lambert 93 pour les calculs suivants
df_terre_inter <- df_terre_inter |>
  filter(!st_is_empty(CONTOUR))

# =========================
# 6.CALCUL DE LA PENTE ET DE L'ALTTIUDE MOYENNE 
# (des parcelles débardées en utlisant la BD ALTI 25m de l'IGN)
# =========================

library(exactextractr)
library(terra)

dir_base <- "~/Documents/S10_BETA/scable_dev/resources/public/BDALTI/"
files <- list.files(
  dir_base,
  pattern = "\\.[a][s][c]$",  # permet de sélectionner tous les fichiers .asc présent dans le fichier donné en dir_base
  full.names = TRUE,
  recursive = TRUE )

print(length(files)) # On vérifie que les fichiers .asc ont été sélectionnés et que le files n'est pas vide

mnt <- terra::vrt(files) #chargement de la BDALTI dans le Formal class SpatRaster mnt
# Attention certaines étape peut prendre plusieurs minutes, il ne faut pas les stopper

crs(mnt) <- "EPSG:2154" # définir le CRS des rasters pour qu'il soit identique à celui des polygones (IGN BD ALTI = Lambert 93 2154)
v <- terra::vect(df_terre_inter) # On converti en vecteur terra les données spatiales des polygones 
mnt_terre <- crop(mnt, v) # On réduit le Raster uniquement aux polygones pour l'étude de pente et d'altitude
slope <- terrain(mnt_terre, v = "slope", unit = "degrees") # on créé un Raster contenant uniquement la pente de chaque polygone
df_terre_inter$ALTITUDE_MOY <- exact_extract(mnt_terre, df_terre_inter, 'mean') #Ajout de l'altitude moyenne pondérée par chaque polygone dans le tableau
df_terre_inter$PENTE_MOY <- exact_extract(slope, df_terre_inter, 'mean') #Ajout de la pente moyenne pondérée par chaque polygone dans le tableau


################### On verra si le calcul d'un centroide sert vrmt ou si je prends l'altitude moyenne du polygon
# calcul de l'latitude du centroïde
df_terre_inter$CENTROIDE <- st_centroid(df_terre_inter$CONTOUR) # Création du centroide de chaque polygone
centroids_vect <- vect(st_centroid(df_terre_inter$CONTOUR))
df_terre_inter$ALTITUDE_CENTROIDE <- terra::extract(mnt, centroids_vect)[,2]

# On classe les chantiers en classe plaine / montagne selon leur altitude.
df_terre_inter$TYPE_CHANTIER <- ifelse(df_terre_inter$ALTITUDE_MOY > 600, "Montagne", "Plaine")

summary(df_terre_inter$ALTITUDE_MOY)
summary(df_terre_inter$PENTE_MOY)

# =========================
# 14. ENREGISTREMENT DU TABLEUR FINAL
# =========================
df_export <- df_terre_inter |>
  sf::st_drop_geometry() |> # permet d'enlever les données spatiales du csv et l'exporter proprement
  select(-CENTROIDE) # les coordonées des centroides sont également enlevées car elles faussent le tableau 

write.csv(df_export, "~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_alti_slope_complet.csv", row.names = FALSE)


