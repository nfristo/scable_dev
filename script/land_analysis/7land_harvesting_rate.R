# =========================
# SCRIPT N°7 TERRE réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer la distribution du taux de prélèvement par chantier terrestre
# =========================

library(readr)
library(dplyr)
library(sf)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev/")
data_chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/data_chantiers_terre_output") |>
  mutate(
    G_CUT_HA = if_else(G_TOT_CUT / HA_DES_TOT > 1,
                       G_TOT_CUT / HA_DES_TOT,
                       NA_real_)
  )

G_SER_IFN <- read_csv2("~/Documents/S10_BETA/scable_dev/resources/public/G_SER_IFN.csv")
contours_chantiers <- read.csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO_terrestres.csv")
ser <- st_read("~/Documents/S10_BETA/scable_dev/carto/ser_l93/ser_l93.shp")

#Assigner à chaque chantier sa SER correspondante :

contours_chantiers <- st_as_sf( # Transformer CONTOUR (WKT) en géométrie
  contours_chantiers,
  wkt = "CONTOUR",
  crs = 4326
)

contours_chantiers <- st_transform(contours_chantiers, 2154)
st_bbox(contours_chantiers) # Transformer les coordonnées vers Lambert 93 car SER en Lambert 93

chantier_ser <- st_intersection( # Intersection chantier-SER
  contours_chantiers,
  ser
)

chantier_ser_majoritaire <- chantier_ser |>
  mutate(surface_SER = as.numeric(st_area(CONTOUR)))|>
  st_drop_geometry() |>
  group_by(ID_FB, codeser) |>
  summarise(
    surface_SER = sum(surface_SER),
    .groups = "drop"
  ) |>
  group_by(ID_FB) |>
  slice_max(surface_SER, n = 1) 

data_chantiers <- data_chantiers |> # Ajouter la colonne de la SER correspondante à chaque chantier
  left_join(
    chantier_ser_majoritaire |>
      select(ID_FB, codeser),
    by = "ID_FB"
  )

G_SER_IFN <- G_SER_IFN |>
  rename( G_MOY_SER = `Moyenne hectare GTOT (m2/ha)`)
G_SER_IFN <- G_SER_IFN |>
  rename( codeser = `Code  SER_86`)

data_chantiers <- data_chantiers |>
  left_join(
    G_SER_IFN |>
      select(codeser, G_MOY_SER),
    by = "codeser"
  ) 

data_chantiers <- data_chantiers |>
  mutate(
    G_MOY_SER = as.numeric(gsub(",", ".", G_MOY_SER)),
    Tx_prelevement = G_CUT_HA / G_MOY_SER
  ) 

hist(data_chantiers$Tx_prelevement)
summary(data_chantiers$Tx_prelevement)

write_csv(data_chantiers, "~/Documents/S10_BETA/scable_dev/results/global/output/data_chantiers_terre_output_carto")


data_chantiers |>
  group_by(FAMILLE_COUPE) |>
  summarise(
    Tx_moy = mean(Tx_prelevement, na.rm = TRUE),
    Tx_med = median(Tx_prelevement, na.rm = TRUE),
    Q1 = quantile(Tx_prelevement, 0.25, na.rm = TRUE),
    Q3 = quantile(Tx_prelevement, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

data_chantiers |>
  group_by(TYPE_ESSENCE) |>
  summarise(
    Tx_moy = mean(Tx_prelevement, na.rm = TRUE),
    Tx_med = median(Tx_prelevement, na.rm = TRUE),
    Q1 = quantile(Tx_prelevement, 0.25, na.rm = TRUE),
    Q3 = quantile(Tx_prelevement, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


