# =========================
# SCRIPT N°11 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer la distribution du taux de prélèvement par chantier
# =========================

library(readr)
library(dplyr)
library(sf)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev/")
data_chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/output/data_chantiers_output") |>
  mutate(
    G_CUT_HA = if_else(G_TOT_CUT / HA_DES_TOT > 1,
                       G_TOT_CUT / HA_DES_TOT,
                       NA_real_)
  )

G_SER_IFN <- read_csv2("~/Documents/S10_BETA/scable_dev/resources/public/G_SER_IFN.csv")
contours_chantiers <- read.csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO.csv")
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
  group_by(ID_CHANTIER, codeser) |>
  summarise(
    surface_SER = sum(surface_SER),
    .groups = "drop"
  ) |>
  group_by(ID_CHANTIER) |>
  slice_max(surface_SER, n = 1) 

data_chantiers <- data_chantiers |> # Ajouter la colonne de la SER correspondante à chaque chantier
  left_join(
    chantier_ser_majoritaire |>
      select(ID_CHANTIER, codeser),
    by = "ID_CHANTIER"
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

write_csv(data_chantiers, "~/Documents/S10_BETA/scable_dev/results/global/output/data_chantiers_output_carto")


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


library(ggplot2)

ggplot(data_chantiers, aes(
  x = FAMILLE_COUPE,
  y = Tx_prelevement
)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Famille de coupe",
    y = "Taux de prélèvement"
  )

kruskal.test(
  Tx_prelevement ~ FAMILLE_COUPE,
  data = data_chantiers
)

ggplot(data_chantiers, aes(
  x = PEUPLEMENT_STR,
  y = Tx_prelevement
)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = " PEUPLEMENT_STR coupe",
    y = "Taux de prélèvement"
  )

kruskal.test(
  Tx_prelevement ~ PEUPLEMENT_STR,
  data = data_chantiers
)

ggplot(data_chantiers, aes(
  x = TYPE_ESSENCE,
  y = Tx_prelevement
)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c(
      "FEUILLUS" = "HARDWOOD",
      "RESINEUX" = "SOFTWOOD"
    )
  ) +
  theme_bw() +
  labs(
    x = "Tree species group",
    y = "Harvesting rate"
  )

kruskal.test(
  Tx_prelevement ~ TYPE_ESSENCE,
  data = data_chantiers
)

ggplot(data_chantiers, aes(
  x = TYPE_ESSENCE,
  y = Tx_prelevement,
  fill = FAMILLE_COUPE
)) +
  geom_boxplot() +
  theme_bw()

wilcox.test(
  Tx_prelevement ~ TYPE_ESSENCE,
  data = data_chantiers
)

data_chantiers |>
  wilcox_effsize(
    Tx_prelevement ~ TYPE_ESSENCE
  )

modele1 <- lm(
  Tx_prelevement ~ TYPE_ESSENCE,
  data = data_chantiers
)

modele2 <- lm(
  Tx_prelevement ~ TYPE_ESSENCE + FAMILLE_COUPE,
  data = data_chantiers
)

anova(modele1, modele2)

modele3 <- lm(
  Tx_prelevement ~ FAMILLE_COUPE,
  data = data_chantiers
)

modele4 <- lm(
  Tx_prelevement ~ FAMILLE_COUPE + TYPE_ESSENCE,
  data = data_chantiers
)

anova(modele3, modele4)

modele_inter <- lm(
  Tx_prelevement ~ FAMILLE_COUPE * TYPE_ESSENCE,
  data = data_chantiers
)

anova(modele2, modele_inter)

summary(modele2)


install.packages("emmeans")
library(emmeans)

emmeans(modele2, ~ TYPE_ESSENCE + FAMILLE_COUPE)

modele_log <- lm(
  log(Tx_prelevement) ~ TYPE_ESSENCE + FAMILLE_COUPE,
  data = data_chantiers
)
emmeans(modele_log, ~ TYPE_ESSENCE + FAMILLE_COUPE) |>
  summary(type = "response")
