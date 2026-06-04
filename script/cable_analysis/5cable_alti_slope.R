# =========================
# SCRIPT N°5 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif d'ajouter les caractéristiques des chantiers aux données de prix.
# =========================

# =========================
# 1. PACKAGES
# =========================
library(readr)
library(dplyr)
library(sf)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CHANTIERS.csv") |>
  filter(ETAT_CHANTIER %in% c("Terminé", "En cours")) #On prend en compte uniquement les chantiers terminés ou en cours
ID_ok <- chantiers$ID_CHANTIER
df_inter <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/intermediate/df_inter.csv")
contour <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO.csv") |>
  filter(ID_CHANTIER %in% ID_ok)#idem
grumes <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/GRUMES.csv")|>
  filter(ID_CHANTIER %in% ID_ok)#idem
tiges <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/TIGES.csv")|>
  filter(ID_CHANTIER %in% ID_ok)#idem

# =========================
# 3. CARACTÉRISTIQUES DES GRUMES DEBARDÉES 
# =========================

# Essence majoritaire des grumes marquées pour le débardage
ess_maj <- tiges |>
  filter(!is.na(ESS), !is.na(NB_TIGES)) |>
  group_by(ID_CHANTIER, ACCORD_CADRE, ESS) |>
  summarise(nb = sum(NB_TIGES), .groups = "drop") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  slice_max(nb, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ID_CHANTIER, ACCORD_CADRE, ESSENCE_MAJ = ESS)

# Indicateurs principaux caractérisant les tiges marquées
stats <- tiges |>
  mutate(
    vol_unitaire = ifelse(NB_TIGES > 1,
                          VOL_TIGE / NB_TIGES,
                          VOL_TIGE)
  ) |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    NB_GRUMES = sum(NB_TIGES, na.rm = TRUE), # Nombre de tiges marquées par chantier
    DIAM_MOY = sum(DIAM * NB_TIGES, na.rm = TRUE) / sum(NB_TIGES, na.rm = TRUE),  #Moyenne du diamètre des grumes marquées par chantier 
    VOL_MOYEN_GRUME = sum(vol_unitaire * NB_TIGES, na.rm = TRUE) / sum(NB_TIGES, na.rm = TRUE), #Moyenne du volume des grumes marquées par chantier
    G_TOT_CUT = sum(G, na.rm = TRUE), #Surface terrière totale coupée ATTENTION ce n'est pas la surface terrière du peuplemen
    
    .groups = "drop"
  )

gr_stats <- left_join(stats, ess_maj,
                       by = c("ID_CHANTIER", "ACCORD_CADRE"))

prix_chantier <- grumes |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    PRIX_VENTE_TOT = if(all(is.na(PRIX_VENTE))) NA_real_
    else sum(PRIX_VENTE, na.rm = TRUE),
    .groups = "drop"
  )

gr_stats <- gr_stats |>
  left_join(prix_chantier, by = c("ID_CHANTIER", "ACCORD_CADRE"))
gr_stats <- gr_stats |>
  mutate(
    PRIX_GRUME = PRIX_VENTE_TOT / NB_GRUMES #Moyenne du prix de vente par grumes par chantier
  )

# =========================
# 4. CARACTÉRISTIQUES DES PEUPLEMENTS #A REVENIR DESSUS ABSOLUMNT CAR DONNES FAUSSES
# =========================

peupl_stats <- contour |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    VOL_HA_MOY = mean(VOL_HA, na.rm = TRUE), #Volume moyen à l'hectare du peuplement  
    HA_DES_TOT = sum(HA_DES, na.rm = TRUE), #Surface du peuplement en hectare désigné par le TFT  
  )

peupl_stats2 <- contour |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    FAMILLE_COUPE = names(sort(table(FAMILLE_COUPE), decreasing = TRUE))[1], #Famille de coupe de la coupe effectuée
    PEUPLEMENT_STR = names(sort(table(PEUPLEMENT_MODE), decreasing = TRUE))[1]) #Structure du peuplement

# =========================
# 5. ASSEMBLAGE DE TOUS LES TABLEURS PRÉCÉDENTS AVEC ID CHANTIERS 
# ET CALCUL DE VARIABLES SUPPLÉMENTAIRES
# =========================

df_inter2 <- chantiers |>
  select(ID_CHANTIER, ACCORD_CADRE, DEP, FRT, NUM_ETF, DEBUT_CHANTIER,FIN_CHANTIER, VOL_PREVU, NB_LIGNES,
         LONGUEUR_MIN, LONGUEUR_MAX) |>
  left_join(df_inter, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  left_join(gr_stats, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  left_join(peupl_stats, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  left_join(peupl_stats2, by = c("ID_CHANTIER", "ACCORD_CADRE"))

df_inter2 <- df_inter2 |>
  mutate(LONGUEUR_MOY = (LONGUEUR_MIN + LONGUEUR_MAX) / 2) |> #Longueur moyenne d'une ligne de câble
 relocate(LONGUEUR_MOY, .after = LONGUEUR_MAX)

# Pour pouvoir calculer des données spatiales :
# On ajoute les polygones de chaque chantier dans le df, en les regroupant par chantier :
contour_sf <- contour |>
  st_as_sf(wkt = "CONTOUR", crs = 4326)
contour_chantier <- contour_sf |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(CONTOUR = st_union(CONTOUR))

df_inter2 <- df_inter2 |>
  left_join(contour_chantier, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  st_as_sf(sf_column_name = "CONTOUR", crs = 4326) |>
  st_transform(2154)

#Les coordonnées des polygones sont extraites pour pouvoir être utilisées en analyse statistiques.
df_inter2 <- df_inter2 |>
  st_as_sf() |>
  st_transform(2154) #transformation des données WGS 84 en Lambert 93 pour les calculs suivants
df_inter2 <- df_inter2 |>
  filter(!st_is_empty(CONTOUR)) #Supprime le chantier 44 qui ne contient pas de POlygon


# =========================
# 6. Calcul de la pente et l'altitude moyenne des parcelles débardées en utlisant la BD ALTI 25m de l'IGN
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
v <- vect(df_inter2) # On converti en vecteur terra les données spatiales des polygones 
mnt_cable <- crop(mnt, v) # On réduit le Raster uniquement aux polygones pour l'étude de pente et d'altitude
slope <- terrain(mnt_cable, v = "slope", unit = "degrees") # on créé un Raster contenant uniquement la pente de chaque polygone
df_inter2$ALTITUDE_MOY <- exact_extract(mnt_cable, df_inter2, 'mean') #Ajout de l'altitude moyenne pondérée par chaque polygone dans le tableau
df_inter2$PENTE_MOY <- exact_extract(slope, df_inter2, 'mean') #Ajout de la pente moyenne pondérée par chaque polygone dans le tableau

################### On verra si le calcul d'un centroide sert vrmt ou si je prends l'altitude moyenne du polygon
# calcul de l'latitude du centroïde
df_inter2$CENTROIDE <- st_centroid(df_inter2$CONTOUR) # Création du centroide de chaque polygone
centroids_vect <- vect(st_centroid(df_inter2$CONTOUR))
df_inter2$ALTITUDE_CENTROIDE <- terra::extract(mnt, centroids_vect)[,2] 

# On classe les chantiers en classe plaine / montagne selon leur altitude.
df_inter2$TYPE_CHANTIER <- ifelse(df_inter2$ALTITUDE_MOY > 600, "Montagne", "Plaine")

summary(df_inter2$ALTITUDE_MOY)
summary(df_inter2$PENTE_MOY)

# =========================
# 14. ENREGISTREMENT DU TABLEUR FINAL
# =========================
df_export <- df_inter2 |>
  sf::st_drop_geometry() |> # permet d'enlever les données spatiales du csv et l'exporter proprement
  select(-CENTROIDE) # les coordonées des centroides sont également enlevées car elles faussent le tableau 

write.csv(df_export, "~/Documents/S10_BETA/scable_dev/results/cable_df_inter_alti_slope.csv", row.names = FALSE)

