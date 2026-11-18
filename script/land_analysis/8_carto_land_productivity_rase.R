# =========================
# SCRIPT N°13 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA
# Cartographie de la dynamique de production du débardage terrestre
# Hypothèse : coupe rase (100 % de la surface terrière récoltée)
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
library(posterior)

# =========================
# OPTIONS MEMOIRE
# =========================

terraOptions(
  memfrac = 0.5,
  todisk = TRUE,
  progress = 10
)

# =========================
# 2. DONNEES
# =========================

mod_vitesse_terre <- readRDS(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/mod_vitesse_intra_terre.rds"
)

G_SER <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/G_SER.tif"
)

# =========================
# 3. GRILLE DE PREDICTION
# =========================

g_min <- 0

g_max <- global(
  G_SER,
  "max",
  na.rm = TRUE
)[1,1]

g_seq <- seq( 
  g_min,
  g_max,
  length.out = 10000 #seulement 10 000 predictions pour éviter des milions de prédictions pour chaque pixels
)

# =========================
# 4. PREDICTION DU MODELE
# =========================

cat("Calcul de la courbe du modèle...\n")

# Prédicteur linéaire (échelle log)

mu <- posterior_linpred(
  mod_vitesse_terre,
  newdata = data.frame(
    G_TOT_CUT = g_seq
  )
)

# Paramètre sigma

draws <- as_draws_df(mod_vitesse_terre)
sigma <- draws$sigma

# Moyenne de la loi lognormale

pred_mean <- apply( #pour revenir au valeurs réelles
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

lookup <- data.frame( # contient la courbe de dsitribution complète du modèle
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

G_coupe <- G_SER

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
  filename = "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_terre_coupe_rase.tif",
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
    "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_terre_coupe_rase_foret.tif",
  overwrite = TRUE
)


rm(
  G_coupe,
  vitesse_r
)

gc()

# =========================
# 7. VERIFICATION DU MODELE
# =========================

summary(mod_vitesse_terre)
fixef(mod_vitesse_terre)