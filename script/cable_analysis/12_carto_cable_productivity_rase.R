#=========================
# SCRIPT N°12 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de créer la cartographie spatialisée de la dynamique de production
# =========================

# Modèle pris en compte :
# VITESSE_MOY_JOUR_M3 ~ G_TOT_CUT

# =========================
# 1. PACKAGES
# =========================

install.packages("terra")
install.packages("sf")
install.packages("brms")
install.packages("dplyr")

library(terra)
library(sf)
library(brms)
library(dplyr)
library(stringr)
library(readr)
library(posterior)

terraOptions(
  memfrac = 0.5,
  todisk = TRUE,
  progress = 10
)

# =========================
# 2. DONNEES
# =========================

mod_vitesse <- readRDS(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/mod_vitesse_intra_cable.rds"
)

ser <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/ser_l93/ser_l93.shp",
  quiet = TRUE
)

G_SER <- read_csv2(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/G_SER_IFN.csv"
)

G_SER <- G_SER |>
  rename(
    G_MOY_SER = `Moyenne hectare GTOT (m2/ha)`,
    codeser = `Code  SER_86`
  ) |>
  mutate(
    G_MOY_SER = ifelse(G_MOY_SER == "n.s.", "0", G_MOY_SER),
    G_MOY_SER = str_replace(G_MOY_SER, ",", "."),
    G_MOY_SER = as.numeric(G_MOY_SER)
  )

ser <- ser |>
  left_join(
    G_SER |>
      select(codeser, G_MOY_SER),
    by = "codeser"
  )

foret <- vect(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/masque_foret2.gpkg"
)

template <- rast(
  ext(foret),
  resolution = 25,
  crs = crs(foret)
)

G_r <- rasterize(
  vect(ser),
  template,
  field = "G_MOY_SER"
)

writeRaster(
  G_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/G_SER.tif",
  overwrite = TRUE
)

# =========================
# 3. GRILLE DE PREDICTION
# =========================

g_min <- 0

g_max <- global(
  G_r,
  "max",
  na.rm = TRUE
)[1,1]

g_seq <- seq(
  g_min,
  g_max,
  length.out = 10000
)

# =========================
# 4. PREDICTION DU MODELE
# =========================

cat("Calcul de la courbe du modèle...\n")

mu <- posterior_linpred(
  mod_vitesse,
  newdata = data.frame(
    G_TOT_CUT = g_seq
  )
)

draws <- as_draws_df(mod_vitesse)
sigma <- draws$sigma

pred_mean <- apply(
  exp(
    sweep(
      mu,
      1,
      sigma^2 / 2,
      "+"
    )
  ),
  2,
  mean
)

lookup <- data.frame(
  G = g_seq,
  vitesse = pred_mean
)

rm(
  mu,
  pred_mean,
  sigma,
  draws
)

gc()

# =========================
# 5. FONCTION D'INTERPOLATION
# =========================

interp_fun <- function(x){
  
  y <- approx(
    x = lookup$G,
    y = lookup$vitesse,
    xout = x,
    rule = 2
  )$y
  
  y[is.na(x)] <- NA_real_
  
  y
  
}

# =========================
# 6. APPLICATION DU MODELE
# =========================

cat("Calcul de la carte de vitesse...\n")

# Hypothèse : coupe rase
# Toute la surface terrière est récoltée

G_coupe <- G_r

cat(
  "G_TOT_CUT :",
  global(G_coupe, "min", na.rm = TRUE)[1,1],
  "-",
  global(G_coupe, "max", na.rm = TRUE)[1,1],
  "\n"
)

vitesse_r <- terra::app(
  G_coupe,
  fun = interp_fun
)

writeRaster(
  vitesse_r,
  filename = "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_cable_coupe_rase.tif",
  overwrite = TRUE
)

# =========================
# 18. MASQUE FORESTIER
# =========================

foret <- vect(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/masque_foret2.gpkg"
)

mask(
  vitesse_r,
  foret,
  filename =
    "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_cable_coupe_rase_foret.tif",
  overwrite = TRUE
)

rm(
  G_coupe,
  vitesse_r
)

gc()
