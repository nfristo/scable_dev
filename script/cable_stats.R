# =========================
# SCRIPT réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif de prestation (au m3) facturé à l'ONF du débardage par câble aérien. 
# Il est séparé en plusieurs parties, comprenant le calcul du prix de l'abattage/façonnage, le calcul du prix du débardage,
# le calcul de la mise en place du câble ainsi que les frais de déplacements des entreprises. 
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
chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CHANTIERS.csv") %>%
  filter(ETAT_CHANTIER %in% c("Terminé", "En cours")) #On prend en compte uniquement les chantiers terminés ou en cours
ID_ok <- chantiers$ID_CHANTIER
commandes <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP.csv") %>%
  filter(ID_CHANTIER %in% ID_ok) #idem
contour <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO.csv") %>%
  filter(ID_CHANTIER %in% ID_ok)#idem
grumes <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/GRUMES.csv")

# =========================
# 3. COUT D’ABATTAGE (€/m3)
# =========================

# Filtre uniquement les lignes d'abattage dans le tableur commandes
abattage <- commandes %>%
  filter(grepl("battage|façonnage", LIBELLE_ARTICLE, ignore.case = TRUE),
         UNITE == "M3") #On filtre pour le moment par M3 pour éviter de fausser les données voir avec valentin ou martin

# Calcul du coût moyen par chantier
cout_aba <- abattage %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    COUT_ABA_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )

# =====================================
# 4. COUT DU DÉBARDAGE PAR CÂBLE (€/m3)
# =====================================

# Filtre uniquement les lignes de débardage par câble dans le tableur commandes (=hors coûts de déplacements et de mise en place)
debardage <- commandes %>%
  filter(grepl("Débardag|Câblage", LIBELLE_ARTICLE, ignore.case = TRUE),
         UNITE == "M3")#Je filtre pour le moment par M3 pour éviter de fausser les données voir avec valentin ou martin

# Calcul du coût moyen par chantier
cout_deb <- debardage %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    COUT_DEB_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )

# =========================
# 5. VOLUME RÉEL DÉBARDÉ PAR L'ETF ET FACTURÉ À L'ONF
# =========================

vol_reel <- commandes %>%
  filter(UNITE == "M3") %>% #Filtre par m3 pour éviter d'inclure les coûts fixes
  group_by(ID_CHANTIER) %>% #Regroupe la sélection des bois débardés par chantier
  summarise(VOL_REEL_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE)) #Somme la quantité de bois réceptionné par l'ONF par chantier


# =====================================
# 6. COUT DU MONTAGE/DÉMONTAGE DES LIGNES DE DÉBARDAGE  (€/m3)
# =====================================

# Filtre uniquement les coûts de mise en place du câble dans le tableur commandes
montage <- commandes %>%
  filter(grepl("ontage/démontag", LIBELLE_ARTICLE, ignore.case = TRUE),
         UNITE == "U" | UNITE == "/FO")

# Calcul du coût moyen par chantier
cout_montage <- montage %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    COUT_MONTAGE_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE) #Calcul du tarif facturé à l'ONF pour le montage du câble par chantier 
  ) %>%
  left_join(vol_reel, by = "ID_CHANTIER") %>%
  mutate(
    COUT_MONTAGE_M3 =COUT_MONTAGE_TOTAL / VOL_REEL_M3, #Tarif ramené au m3 débardé par chantier
  )

# =====================================
# 7. COUT DU TRANSFERT MATÉRIEL ETF  (€/m3)
# =====================================

# Filtre uniquement les coûts de transfert du matériel câble et ETF dans le tableur commandes
transfert <- commandes %>%
  filter(grepl("ransfert", LIBELLE_ARTICLE, ignore.case = TRUE),
         UNITE == "U" | UNITE == "/FO")

# Calcul du coût moyen par chantier
cout_transfert <- transfert %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    COUT_TRANSFERT_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE) #Calcul du tarif facturé à l'ONF pour le transfert du matériel par chantier 
  ) %>%
  left_join(vol_reel, by = "ID_CHANTIER") %>%
  mutate(
    COUT_TRANSFERT_M3 = COUT_TRANSFERT_TOTAL / VOL_REEL_M3 #Tarif ramené au m3 débardé par chantier
  )

# =========================
# 8. CARACTÉRISTIQUES DES GRUMES DEBARDÉES 
# =========================

gr_stats <- grumes %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    DIAM_MOY = mean(DIAM_SUR, na.rm = TRUE), #Moyenne du diamètre des grumes débardées par chantier
    VOL_MOYEN_GRUME = mean(VOL_SUR, na.rm = TRUE), #Moyenne du volume des grumes débardées par chantier
    PRIX_VENTE_MOY = mean(PRIX_VENTE, na.rm = TRUE), #Moyenne des prix de vente des grumes par chantier
    NB_GRUMES = n()
  )

# =========================
# 9. CARACTÉRISTIQUES DES PEUPLEMENTS
# =========================

peupl_stats <- contour %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    G_TOT_MOY = mean(G_TOT, na.rm = TRUE), #Surface terrière moyenne à l'hectare 
    VOL_HA_MOY = mean(VOL_HA, na.rm = TRUE), #Volume moyen à l'hectare 
    HA_DES_TOT = sum(HA_DES, na.rm = TRUE), #Surface du peuplement en hectare désigné par le TFT  
    NB_TIGES_CONT = sum(NB_TIGES, na.rm = TRUE) #Nombre de tiges à l'hectare
  )

# ESSENCE DOMINANTE PAR PEUPLEMENT
essence <- contour %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    ESSENCE_DOM = names(sort(table(PEUPLEMENT_COMPO), decreasing = TRUE))[1],
    PEUPLEMENT_MODE = names(sort(table(PEUPLEMENT_MODE), decreasing = TRUE))[1])


# =========================
# 10. ASSEMBLAGE DE TOUS LES TABLEURS PRÉCÉDENTS AVEC ID CHANTIERS 
# ET CALCUL DE VARIABLES SUPPLÉMENTAIRES
# =========================

df <- chantiers %>%
  select(ID_CHANTIER, DEP, FRT, VOL_PREVU, NB_LIGNES,
         LONGUEUR_MIN, LONGUEUR_MAX) %>%
  left_join(vol_reel, by = "ID_CHANTIER") %>%
  left_join(cout_deb, by = "ID_CHANTIER") %>%
  left_join(cout_aba, by = "ID_CHANTIER") %>%
  left_join(cout_montage, by = "ID_CHANTIER") %>%
  left_join(cout_transfert, by = "ID_CHANTIER") %>%
  left_join(gr_stats, by = "ID_CHANTIER") %>%
  left_join(peupl_stats, by = "ID_CHANTIER") %>%
  left_join(essence, by = "ID_CHANTIER")

#On retire les colonnes volume réel qui sont ajoutés plusieurs fois en doublon lors des jointures
df <- df %>%
  mutate(VOL_REEL_M3 = VOL_REEL_M3.x) %>%
  select(-VOL_REEL_M3.x, -VOL_REEL_M3.y) 

# Variables supplémentaires facilement calculables
df <- df %>%
  mutate(
    COUT_VARIABLES_M3 = COUT_DEB_M3 + COUT_ABA_M3,  #Tarif facturé à l'ONF pour l'abattage et le débardage uniquement 
    COUT_FIXES_M3 = COUT_TRANSFERT_M3 + COUT_MONTAGE_M3, #Tarif facturé à l'ONF pour le montage/démontage du câble et les frais de transferts
    TARIF_PRESTA_M3 = COUT_VARIABLES_M3 + COUT_FIXES_M3, #Tarif de prestation facturé à l'ONF pour le chantier tout compris
    LONGUEUR_MOY = (LONGUEUR_MIN + LONGUEUR_MAX) / 2  #Longueur moyenne d'une ligne de câble
  )

# Pour pouvoir calculer des données spatiales :
# On ajoute les polygones de chaque chantier dans le df, en les regroupant par chantier :
contour_sf <- contour %>% 
  st_as_sf(wkt = "CONTOUR", crs = 4326)
contour_chantier <- contour_sf %>%
  group_by(ID_CHANTIER) %>%
  summarise(CONTOUR = st_union(CONTOUR))
df <- df %>%
  left_join(contour_chantier, by = "ID_CHANTIER")

#Les coordonnées des polygones sont extraites pour pouvoir être utilisées en analyse statistiques.
df <- df %>%
  st_as_sf() %>%
  st_transform(2154) #transformation des données WGS 84 en Lambert 93 pour les calculs suivants

# =========================
# 11. Calcul de la pente et l'altitude moyenne des parcelles débardées en utlisant la BD ALTI 25m de l'IGN
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

mnt <- vrt(files) #chargement de la BDALTI dans le Formal class SpatRaster mnt
# Attention cette étape peut prendre plusieurs minutes, il ne faut pas la stopper

crs(mnt) <- "EPSG:2154" # définir le CRS des rasters pour qu'il soit identique à celui des polygones (IGN BD ALTI = Lambert 93 2154)
v <- vect(df) # On converti en vecteur terra les données spatiales des polygones 
mnt <- crop(mnt, v) # On réduit le Raster uniquement aux polygones pour l'étude de pente et d'altitude
slope <- terrain(mnt, v = "slope", unit = "degrees") # on créé un Raster contenant uniquement la pente de chaque polygone
df$altitude_moy <- exact_extract(mnt, df, 'mean') #Ajout de l'altitude moyenne pour chaque polygone dans le tableau
df$pente_moy <- exact_extract(slope, df, 'mean') #Ajout de la pente moyenne pour chaque polygone dans le tableau

################### On verra si le calcul d'un centroide sert vrmt ou si je prends l'altitude moyenne du polygon
# calcul de l'latitude du centroïde
df$centroide <- st_centroid(df$CONTOUR) # Création du centroide de chaque polygone
centroids_vect <- vect(st_as_sf(df$centroide)) 
df$altitude_centroide <- extract(mnt, centroids_vect)[,2]
#mediane_chantier <- df %>% # permet de créer une altitude médiane des centroides des différents polygones d'un chantier
  st_drop_geometry() %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    altitude_med = median(altitude_centroide, na.rm = TRUE)
  )

# On classe les chantiers en classe plaine / montagne selon leur altitude.
df$type_chantier <- ifelse(df$altitude_moy > 600, "Montagne", "Plaine")

summary(df$altitude_moy)
summary(df$pente_moy)


# =========================
# TEST ENGORGEMENT #problème = résolution km de la carte et_2014
# =========================

et <- rast("~/Documents/S10_BETA/scable_dev/resources/public/et_2014.tif") #Chargement carte engorgement temporaire france
crs(et) <- "EPSG:2154"
v <- vect(df) 
et <- crop(et, v) # On adapte à l'emprise des polygones des chantiers

df$prop_engorge <- exact_extract(et, df, function(values, coverage_fraction) { #Approche surfacique, superposition des données des pixels
  mean(values > 0.025, na.rm = TRUE) # Définition du seuil d'engorgement de la carte
})

df$zone_humide <- ifelse(df$prop_engorge >= 0.70, "Engorgé", "Non engorgé") # Parcelle engorgée si au moins 20% de sa surface est sur une surface engorgée de la carte ET

df_plaine <- df %>% #On garde uniquement les chantiers de plaine et ceux avec une pente inférieure à 20% 
  filter(type_chantier == "Plaine", pente_moy < 11.3) #11.3° équivaut à environ pente de 20%

#Résumé de l'engorgement par chantier (plaine uniquement) 
resume_plaine <- df_plaine %>%
  st_drop_geometry() %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    prop_engorge = mean(prop_engorge, na.rm = TRUE),
    classe_engorgement = ifelse(prop_engorge >= 0.70, "Engorgé", "OK")
  )

table(df_plaine$zone_humide)


# =========================
# 12. CALCUL DU NOMBRE DE JOURS EFFECTIFS PAR CHANTIER
# =========================

jours_chantier <- df %>%
  group_by(ID_CHANTIER) %>%
  summarise(
    DEBUT_CHANTIER = ifelse(all(is.na(DEBUT_CHANTIER)), NA, min(DEBUT_CHANTIER, na.rm = TRUE)), # Chargement des dates de début et fin de chantier 
    FIN_CHANTIER   = ifelse(all(is.na(FIN_CHANTIER)), NA, max(FIN_CHANTIER, na.rm = TRUE)),
    NB_LIGNES_TOTAL = sum(NB_LIGNES, na.rm = TRUE), # Chargement du nombre de lignes totlaes par chantier
    LONGUEUR_MAX = max(LONGUEUR_MAX, na.rm = TRUE), # Chargement de la longueur maximum des lignes par chantier
    type_chantier = first(type_chantier) # chargement du type de chantier Plaine ou Montagne
  ) %>%
  mutate(
    JOURS_TOTAL = as.numeric(FIN_CHANTIER - DEBUT_CHANTIER + 2),
    NB_TRANCHES_200 = floor(pmax(0, LONGUEUR_MAX - 200) / 200), # permet de ne pas prendre en compte les premiers 200 premiers mètres de câble
    CORRECTION_LIGNES = ifelse(
      type_chantier == "plaine",
      NB_LIGNES_TOTAL * ( 2 + (0.25 * NB_TRANCHES_200)), # Modalité du calcul pour les chantiers Plaine
      -2 * NB_LIGNES_TOTAL # Modalité du calcul pour les chantiers Montagne
    ),
    JOURS_EFFECTIFS = (JOURS_TOTAL + CORRECTION_LIGNES) * 5/7 #calcul final pour trouver le nb de jours effectifs de chantier
  )

df <- df %>%
  st_drop_geometry() %>% # On mets la géométrie des données à la même frome que df pour faire la jointure
  left_join(
    jours_chantier %>% st_drop_geometry() %>% select(ID_CHANTIER, JOURS_EFFECTIFS), #on ajoute Jours effectifs au df par ID chantier
    by = "ID_CHANTIER"
  )

#=========================
# 13. ESTIMATION DE LA VITESSE DE DÉBARDAGE (m3/jour effectif) : 2 méthodes différentes
# =========================

# 1ère estimation en utilisant les cinétiques de débardage intra-chantier grâce aux dates de réception : 

df_vitesse <- commandes %>%
  filter(UNITE == "M3") %>%  # Pour prendre en compte uniquement les dates de reception concernant le bois
  arrange(ID_CHANTIER, DATE_RECEPTION) %>%
  group_by(ID_CHANTIER) %>%
  mutate(
    date_prec = lag(DATE_RECEPTION), #initialisation de la première date de réception
    jours_calendaires = as.numeric(DATE_RECEPTION - date_prec), #Calcul de nombre de jours entre les deux dates de réception
    jours_travailles = jours_calendaires * (5/7), #on précise qu'on utilise 5 jours de travail effectif par semaine
    vitesse_m3_j = QUANTITE_RECEPTION / jours_travailles #on divise la quantité réceptionnée en m3 par le nombre de jours effectifs travaillés
  ) %>%
  filter(jours_calendaires > 2 & jours_calendaires <= 90) %>% # Pour éviter d'avoir les lignes n'ayant pas de cinétique (1ère date, 1 seule de réception, etc) ou de prendre en compte les réceptions de BE
  ungroup()

df_vitesse %>%
  group_by(ID_CHANTIER) %>%
  summarise(vitesse_moy = mean(vitesse_m3_j, na.rm = TRUE)) # affiche la vitesse moyenne par chantier en moyennant les différentes vitesses

df_vitesse %>%
  summarise(vitesse_moy = mean(vitesse_m3_j, na.rm = TRUE)) # affiche la vitessse moyenne de débardage tout chnatier confondu

# 2e estimation en utilisant la cinétique globale sur le nombre de jours effectifs et le volume réel débardé : 

df <- df %>% 
  mutate(
    VITESSE_DEBARDAGE_M3_JOUR = ifelse(
      JOURS_EFFECTIFS > 2 & JOURS_EFFECTIFS <= 90 ,
      VOL_REEL_M3 / JOURS_EFFECTIFS, #Volume réel divisé par le nombre de jours effectifs par chantier (voir partie #12)
      NA  # On met NA si JOURS_EFFECTIFS <= 0 pour éviter les divisions par zéro
    )
  )

vitesse_moyenne_debardage <- df %>%
  summarise(
    VITESSE_MOYENNE_M3_JOUR = mean(VITESSE_DEBARDAGE_M3_JOUR, na.rm = TRUE)
  ) %>%
  pull(VITESSE_MOYENNE_M3_JOUR)

vitesse_moyenne_debardage #affiche la vitesse moyenne de débardage 


#Tentative de créer détection une pause sur chantier lors du calcul de cinétique 
#Fonctionne pas comme je veux pour l'instant A enlever quand c'est OK
df_phases_actives <- commandes %>%
  filter(UNITE == "M3") %>%
  arrange(ID_CHANTIER, DATE_RECEPTION) %>%
  group_by(ID_CHANTIER) %>%
  mutate(
    date_prec = lag(DATE_RECEPTION),
    gap = as.numeric(DATE_RECEPTION - date_prec),
    
    # Nouvelle phase si trou > 60 jours (à ajuster)
    new_periode = ifelse(is.na(gap) | gap > 60, 1, 0),
    
    # ID de cluster cumulatif
    cluster_id = cumsum(new_cluster)
  ) %>%
  ungroup()


# =========================
# 14. ENREGISTREMENT DU TABLEUR FINAL
# =========================

df_export <- df %>%
  sf::st_drop_geometry() %>% # permet d'enlever les données spatiales du csv et l'exporter proprement
  select(-centroide) # les coordonées des centroides sont également enlevées car elles faussent le tableau 

write.csv(df_export, "~/Documents/S10_BETA/scable_dev/results/cable_dataset.csv", row.names = FALSE)


#===============================================================================================================================================================================
#===============================================================================================================================================================================

# =========================
# ANALYSE des données du tableur df
# =========================
library(dplyr)
library(ggplot2)
library(scales)
# =========================
# 1ere étape : tester toutes les variables non liées à COUT_VARIABLE_M3 qui ont une influence sur elle
# =========================
# Toutes les variables explicatives non liées sont : 
vars <- c("DEP", "FRT", "VOL_PREVU", "NB_LIGNES", "LONGUEUR_MIN", "LONGUEUR_MAX",
          "VOL_REEL_M3", "DIAM_MOY","VOL_MOYEN_GRUME", "PRIX_VENTE_MOY", "NB_GRUMES", 
          "G_TOT_MOY", "VOL_HA_MOY","HA_DES_TOT", "NB_TIGES_CONT",
          "LONGUEUR_MOY", "LON", "LAT", "ESSENCE_DOM", "PEUPLEMENT_MODE")

# Il est possible de tester chaque variable individuellement avec la ligne : 
#summary(lm(COUT_VARIABLES_M3 ~ NB_LIGNES, data = df)) en changeant NB_LIGNES par n'importe quelle variable souhaitant être testée
#Pour interpréter tous les résultats des tests rapidement, nous allons tous les tester et afficher directement les résultats.
#Cration d'un df pour stocker tous les résultats de la boucle for

results <- data.frame(
  variable = character(),
  estimate = numeric(),
  p_value = numeric(),
  r_squared = numeric(),
  stringsAsFactors = FALSE
)

for (v in vars) { #Pour chaque variable, on teste son effet individuel sur COUT_VARIABLES_M3
  formula <- as.formula(paste("COUT_VARIABLES_M3 ~", v))
  model <- lm(formula, data = df)
  
  est <- coef(summary(model))[2, "Estimate"]
  pval <- coef(summary(model))[2, "Pr(>|t|)"]
  r2 <- summary(model)$r.squared
  
  results <- rbind(results, data.frame( #Chaque résultat de la boucle pour un variable est stockée dans le df
    variable = v,
    estimate = est,
    p_value = pval,
    r_squared = r2
  ))
}

# On ajoute la colonne des étoiles de significativité
results <- results %>%
  mutate(signif = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01  ~ "**",
    p_value <= 0.05  ~ "*",
    p_value <= 0.1   ~ ".",
    TRUE             ~ ""
  ))

# Les résultats sont triés par ordre croissant de p-value
results <- results %>%
  arrange(p_value) 
head(results, 20) # Affiche les 20 premiers résultats par p-value (à modifier en fonction du nombre de variables significatives par exemple)

write_csv(results, "~/Documents/S10_BETA/scable_dev/results/global/Cout_variable_relation.csv")

# =========================
# On dispose alors d'une liste de variables ayant une influence significative
# On utilise ces variables ayant un effet sur COUT_VARIABLES_M3 et on teste leur effet combiné
# Régression linéaire
# =========================
modele_variable <- lm(COUT_VARIABLES_M3 ~ DIAM_MOY + VOL_HA_MOY + VOL_MOYEN_GRUME, data = df)
summary(modele_variable) 

modele_variable2 <- lm(COUT_VARIABLES_M3 ~ DIAM_MOY + VOL_HA_MOY + LON, data = df)
summary(modele_variable2)

modele_fixes <- lm(COUT_FIXES_M3 ~ VOL_REEL_M3 + LAT + G_TOT_MOY, data = df)
summary(modele_fixes)

modele_tarif <- lm(TARIF_PRESTA_M3 ~ DIAM_MOY + VOL_HA_MOY + LON, data = df)
summary(modele_tarif)

modele_tarif <- lm(TARIF_PRESTA_M3 ~ DIAM_MOY + VOL_HA_MOY + LON * LAT, data = df)
summary(modele_tarif)

# =========================
# Test corrélation de Pearson
# =========================
cor(df$COUT_VARIABLES_M3, df$DIAM_MOY, use = "complete.obs")
cor(df$COUT_VARIABLES_M3, df$VOL_HA_MOY, use = "complete.obs")
cor(df$COUT_VARIABLES_M3, df$VOL_MOYEN_GRUME, use = "complete.obs")

# =========================
#Relation entre longitude et tarif de prestation (câble)
# =========================

df_plot <- df %>%
  filter(TARIF_PRESTA_M3 <= 45,) #Filtre les valeurs aberrantes au dessus de 60 €/m3

ggplot(df_plot, aes(x = LON, y = TARIF_PRESTA_M3)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relation entre longitude et tarif de prestation (câble)",
    x = "Longitude (Ouest → Est)",
    y = "Tarif de prestation (€/m³)"
  ) +
  scale_x_reverse() +  #permet d'inverser l'axe pour avoir l'ouest à gauche et l'est à droite
  theme_minimal()

# =========================
# Calcul de la part représentée par les coûts fixes dans le tarif de prestation :
# =========================
df_plot <- df_plot %>%
  mutate(part_fixe = COUT_FIXES_M3 / TARIF_PRESTA_M3 * 100)

cat("\nPart coûts fixes dans tarif câble (médiane):", median(df_plot$part_fixe, na.rm=TRUE), "%\n")

#============================
#Répartition des coûts fixes des chantiers par câbles
#============================

repartition <- data.frame(
  Type_cout = c("Montage", "Transfert"),
  Valeur = c(df$Montage, df$Transfert)
) %>%
  mutate(
    pct = Valeur / sum(Valeur),
    label = paste0(Type_cout, "\n", percent(pct, accuracy = 0.1))
  )

ggplot(repartition, aes(x = "", y = Valeur, fill = Type_cout)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 5) +
  scale_fill_manual(values = c("Montage" = "#1f77b4",   
                               "Transfert" = "#ff7f0e")) + 
  labs(title = "Répartition des coûts fixes câble") +
  theme_void()


#============================
#tests relation vitesse de débardage vs contraintes mais là y a mahcine learning/modele c'est sur 
#============================
modele_lin <- lm(
  VITESSE_DEBARDAGE_M3_JOUR ~ VOL_HA_MOY + NB_GRUMES + DIAM_MOY + LONGUEUR_MOY + NB_LIGNES_TOTAL,
  data = df
)
summary(modele_lin)

modele_log <- lm(log(VITESSE_DEBARDAGE_M3_JOUR) ~ VOL_HA_MOY + NB_GRUMES + DIAM_MOY, data = df)
summary(modele_log)
