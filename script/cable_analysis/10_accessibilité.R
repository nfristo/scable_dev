#=========================
# SCRIPT N°10 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer l'accessibilité moyenne des parcelles débardées par câble
# =========================

# =========================
# 1. PACKAGES
# =========================
install.packages("readr")
library(readr)
library(dplyr)
library(sf)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
chantiers <- read_csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/resources/inhouse/CHANTIERS.csv") |>
  filter(ETAT_CHANTIER %in% c("Terminé", "En cours")) #On prend en compte uniquement les chantiers terminés ou en cours
ID_ok <- chantiers$ID_CHANTIER
df_inter <- read_csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/results/global/intermediate/df_inter.csv")
contour <- read_csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/resources/inhouse/CONTOUR_GEO.csv") |>
  filter(ID_CHANTIER %in% ID_ok)#idem

# Pour pouvoir calculer des données spatiales :
# On ajoute les polygones de chaque chantier dans le df, en les regroupant par chantier :
contour_sf <- contour |>
  st_as_sf(wkt = "CONTOUR", crs = 4326)
contour_chantier <- contour_sf |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(CONTOUR = st_union(CONTOUR))

df_inter2 <- df_inter |>
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
# Chargement de la carte d'accessibilité
# =========================

emprise <- st_union(df_inter2) |>
  st_buffer(5)      # 5 m de marge
access <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/Accessfor_2025.gpkg",
  layer = "Porteur",
  wkt_filter = st_as_text(emprise),
  quiet = TRUE
)


# Intersection
inter <- st_intersection(
  df_inter2,
  access
)

# Surface de chaque morceau intersecté
inter$surf <- as.numeric(st_area(inter))

# Moyenne pondérée par la surface mais ça n'a pas vrmt de sens ?
access_chantier_p <- inter |>
  st_drop_geometry() |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    access_moy = weighted.mean(Class, surf),
    .groups = "drop"
  )


access1 <- access |>
  mutate(dist_m = case_when(
    Cat == "Accessible - Classe de débardage 1 : 0 - 250 m" ~ 125,
    Cat == "Accessible - Classe de débardage 2 : 250 - 500 m" ~ 375,
    Cat == "Accessible - Classe de débardage 3 : 500 - 1000 m" ~ 750,
    Cat == "Accessible - Classe de débardage 4 : 1000 - 1500 m" ~ 1250,
    Cat == "Accessible - Classe de débardage 5 : 1500 - 2000 m" ~ 1750,
    Cat == "Accessible - Classe de débardage 6 : > 2000 m" ~ 2250,
    TRUE ~ NA_real_
  ))

inter <- st_intersection(df_inter2, access1)

inter$surf <- as.numeric(st_area(inter))

access_chantier_p <- inter |>
  mutate(surf = as.numeric(st_area())) |>
  st_drop_geometry() |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    access_moy_m = weighted.mean(dist_m, surf, na.rm = TRUE),
    
    prop_inaccessible = sum(surf[Cat == "Inaccessible"], na.rm = TRUE) / sum(surf),
    
    prop_non_exploitable = sum(surf[Cat == "Zone non exploitable (pente trop élevée)"], na.rm = TRUE) / sum(surf),
    
    .groups = "drop"
  )

##################################################
######################@##########################

# =========================
# Chargement de la carte d'accessibilité
# =========================

# Définition de l'emprise des chantiers
emprise <- st_union(df_inter2) |>
  st_buffer(5)      # 5 m de marge

# Lecture de la couche Accessfor
access <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/Accessfor_2025.gpkg",
  layer = "Porteur",
  wkt_filter = st_as_text(emprise),
  quiet = TRUE
)

# =========================
# Calcul des pourcentages de surface par catégorie
# =========================

# Intersection entre les chantiers et la carte d'accessibilité
inter <- st_intersection(df_inter2, access)

# Calcul de la surface (m²) de chaque morceau
inter$surf <- as.numeric(st_area(inter))

# Surface totale de chaque chantier
surf_totale <- inter |>
  st_drop_geometry() |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    surf_totale = sum(surf),
    .groups = "drop"
  )

# Surface par classe d'accessibilité
access_chantier <- inter |>
  st_drop_geometry() |>
  group_by(ID_CHANTIER, ACCORD_CADRE, Class, Cat) |>
  summarise(
    surf_cat = sum(surf),
    .groups = "drop"
  ) |>
  left_join(
    surf_totale,
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  ) |>
  mutate(
    pourcentage = 100 * surf_cat / surf_totale
  )


# =========================
# Classe majoritaire (classe ayant la plus grande surface)
# =========================

classe_majoritaire <- access_chantier |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  slice_max(
    order_by = pourcentage,
    n = 1,
    with_ties = FALSE
  ) |>
  select(
    ID_CHANTIER,
    ACCORD_CADRE,
    classe_majoritaire = Class
  )

# =========================
# Mise en forme : une colonne par catégorie
# =========================

library(tidyr)

access_chantier <- access_chantier |>
  select(
    ID_CHANTIER,
    ACCORD_CADRE,
    Cat,
    pourcentage
  ) |>
  pivot_wider(
    names_from = Cat,
    values_from = pourcentage,
    values_fill = 0,
    names_repair = "unique"
  ) |>
  select(
    ID_CHANTIER,
    ACCORD_CADRE,
    `Accessible - Classe de débardage 1 : 0 - 250 m`,
    `Accessible - Classe de débardage 2 : 250 - 500 m`,
    `Accessible - Classe de débardage 3 : 500 - 1000 m`,
    `Accessible - Classe de débardage 4 : 1000 - 1500 m`,
    `Accessible - Classe de débardage 5 : 1500 - 2000 m`,
    `Accessible - Classe de débardage 6 : > 2000 m`,
    `Zone non exploitable (pente trop élevée)`,
    Inaccessible
  ) |>
  left_join(
    classe_majoritaire,
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  ) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 1))
  )


# =========================
# Affichage
# =========================

print(access_chantier)

write.csv(
  access_chantier,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/results/global/intermediate/access_chantier.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
