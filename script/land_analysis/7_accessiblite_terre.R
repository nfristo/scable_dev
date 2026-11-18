#=========================
# SCRIPT N°7 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer l'accessibilité moyenne des parcelles débardées par débardage terrestre
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
df_terre_inter <- read_csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/results/global/intermediate/3terre_alti_slope_complet.csv")
contour_terre <- read_csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/resources/inhouse/CONTOUR_GEO_terrestres.csv") 

# Pour pouvoir calculer des données spatiales :
# On ajoute les polygones de chaque chantier dans le df, en les regroupant par chantier :
contour_sf <- contour_terre |>
  st_as_sf(
    wkt = "CONTOUR",
    crs = 4326
  )

contour_chantier <- contour_sf |>
  group_by(ID_FB) |>
  summarise(
    CONTOUR = st_union(CONTOUR)
  )
#Les coordonnées des polygones sont extraites pour pouvoir être utilisées en analyse statistiques.
df_terre_inter <- df_terre_inter |>
  left_join(
    contour_chantier,
    by = "ID_FB"
  ) |>
  st_as_sf(
    sf_column_name = "CONTOUR",
    crs = 4326
  ) |>
  st_transform(2154) #transformation des données WGS 84 en Lambert 93 pour les calculs suivants

df_terre_inter <- df_terre_inter |>
  filter(!st_is_empty(CONTOUR)) #Supprime le chantier qui ne contient pas de POlygon

# =========================
# 3. Chargement de la carte d'accessibilité
# =========================

# Définition de l'emprise des chantiers
emprise <- st_union(df_terre_inter) |>
  st_buffer(5)

# Lecture de la couche Accessfor (débardage terrestre = couche Porteur)
access <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/Accessfor_2025.gpkg",
  layer = "Porteur",
  wkt_filter = st_as_text(emprise),
  quiet = TRUE
)

# =========================
# 4. INTERSECTION
# =========================

inter <- st_intersection(df_terre_inter, access)
inter$surf <- as.numeric(st_area(inter))

# =========================
# 5. SURFACE TOTALE
# =========================

surf_totale <- inter |>
  st_drop_geometry() |>
  group_by(ID_FB) |>
  summarise(
    surf_totale = sum(surf),
    .groups = "drop"
  )

# =========================
# 6. SURFACE PAR CLASSE
# =========================

access_chantier <- inter |>
  st_drop_geometry() |>
  group_by(ID_FB, Class, Cat) |>
  summarise(
    surf_cat = sum(surf),
    .groups = "drop"
  ) |>
  left_join(
    surf_totale,
    by = "ID_FB"
  ) |>
  mutate(
    pourcentage = 100 * surf_cat / surf_totale
  )

# =========================
# 7. CLASSE MAJORITAIRE
# =========================

classe_majoritaire <- access_chantier |>
  group_by(ID_FB) |>
  slice_max(
    order_by = pourcentage,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    ID_FB,
    classe_majoritaire = Class
  )

# =========================
# Classe minoritaire
# (plus faible classe présente, pourcentage > 0)
# =========================

classe_minoritaire <- access_chantier |>
  filter(pourcentage > 0) |>
  group_by(ID_FB) |>
  slice_min(
    order_by = pourcentage,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    ID_FB,
    classe_minoritaire = Class
  )


access_chantier <- access_chantier |>
  select(ID_FB, Cat, pourcentage ) |>
  pivot_wider(
    names_from = Cat,
    values_from = pourcentage,
    values_fill = 0,
    names_repair = "unique"
  ) |>
  left_join(classe_majoritaire, by = "ID_FB") |>
  left_join(classe_minoritaire, by = "ID_FB") |>
  mutate(
    across(where(is.numeric), ~ round(.x, 1))
  )


df_terre_inter <- df_terre_inter |>
  left_join(classe_majoritaire, by = "ID_FB") |>
  left_join(classe_minoritaire, by = "ID_FB")


print(access_chantier)

# =========================
# 8. EXPORT
# =========================

write_csv(
  st_drop_geometry(df_terre_inter),
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/results/global/intermediate/df_terre_inter_access.csv"
)
