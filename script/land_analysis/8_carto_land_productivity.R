#=========================
# SCRIPT N°8 TERRE réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de créer la cartographie spatialisée de la dynamique de production des chantiers terrestres
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

# OPTIONS MEMOIRE

terraOptions(
  memfrac = 0.5,
  todisk = TRUE,
  progress = 10
)

mod_vitesse_terre <- readRDS(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/mod_vitesse_intra_terre.rds")

ser <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/ser_l93/ser_l93.shp",
  quiet = TRUE
)

G_SER <- terra::rast("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/G_SER.tif")
 
scenarios <- data.frame(
    classe = c(
      "Faible coupe",
      "Coupe modérée",
      "Coupe forte",
      "Forte coupe peuplements à GB_TGB capitalisés"
    ),
    taux = c(
      0.12,  # Q1 
      0.16,  # médiane du Tx prélèvement 
      0.22,  # Q3 
      1.05   #Forte coupe peuplements à GB_TGB capitalisés
    )
  )
# =========================
# 7. PREDICTION PAR SCENARIO
# =========================

# Maximum de G_TOT_CUT possible parmi tous les scénarios
g_min <- 0

g_max <- global(
  G_SER * max(scenarios$taux),
  "max",
  na.rm = TRUE
)[1,1]

# Grille de prédiction du modèle
g_seq <- seq(
  g_min,
  g_max,
  length.out = 10000
)

# =========================
# PREDICTION DU MODELE
# MOYENNE LOGNORMALE
# =========================

cat("Calcul de la courbe du modèle...\n")

# prédicteur linéaire (échelle log)
mu <- posterior_linpred(
  mod_vitesse_terre,
  newdata = data.frame(
    G_TOT_CUT = g_seq
  )
)

# récupération de sigma du modèle lognormal
draws <- as_draws_df(mod_vitesse_terre)
sigma <- draws$sigma

# moyenne lognormale pour chaque simulation
pred_draws <- exp(
  sweep(mu, 1, sigma^2 / 2, "+")
)

# moyenne des simulations postérieures
pred_mean <- apply(
  pred_draws,
  2,
  mean
)

# table de correspondance
lookup_terre <- data.frame(
  G = g_seq,
  vitesse = pred_mean
)


rm(mu, pred_mean, sigma, draws, pred_draws)
gc()

# =========================
# FONCTION D'INTERPOLATION
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
# BOUCLE SCENARIOS
# =========================

for(i in seq_len(nrow(scenarios))){
  
  cat(
    "Scenario :",
    scenarios$classe[i],
    "\n"
  )
  
  # Surface terrière coupée selon scénario
  G_coupe <- G_SER * scenarios$taux[i]
  
  
  # Affichage des plages de G coupée
  cat(
    "G_TOT_CUT :",
    global(G_coupe, "min", na.rm = TRUE)[1,1],
    "-",
    global(G_coupe, "max", na.rm = TRUE)[1,1],
    "\n"
  )
  
  
  # Application de la relation vitesse = f(G_TOT_CUT)
  vitesse_r <- terra::app(
    G_coupe,
    fun = interp_fun
  )
  
  
  # Sauvegarde
  writeRaster(
    vitesse_r,
    filename = paste0(
      "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_terre_",
      scenarios$classe[i],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  rm(G_coupe,vitesse_r )
  gc()
}


summary(mod_vitesse_terre)
fixef(mod_vitesse_terre)

