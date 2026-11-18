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

# OPTIONS MEMOIRE

terraOptions(
  memfrac = 0.5,
  todisk = TRUE,
  progress = 10
)

mod_vitesse <- readRDS(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/mod_vitesse_intra_cable.rds")

ser <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/ser_l93/ser_l93.shp",
  quiet = TRUE
)

G_SER <- read_csv2("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/G_SER_IFN.csv")

G_SER <- G_SER|>
  rename( G_MOY_SER = `Moyenne hectare GTOT (m2/ha)`)
G_SER<- G_SER |>
  rename( codeser = `Code  SER_86`)

G_SER<- G_SER |>
  mutate(
  G_MOY_SER = ifelse(G_MOY_SER == "n.s.", "0", G_MOY_SER),
  G_MOY_SER = str_replace(G_MOY_SER, ",", "."),
  G_MOY_SER = as.numeric(G_MOY_SER)
)

ser <- ser |>
  left_join(
    G_SER |> 
      select(codeser, G_MOY_SER),
    by="codeser"
  )

foret <- vect("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/masque_foret2.gpkg")

template <- rast(
  ext(foret),
  resolution = 25,
  crs = crs(foret)
)

G_r <- rasterize(
  vect(ser),
  template,
  field="G_MOY_SER"
)

writeRaster(
  G_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/G_SER.tif",
  overwrite=TRUE
)

scenarios <- data.frame(
  classe = c(
    "Faible coupe",
    "Coupe modérée",
    "Coupe forte",
    "Forte coupe peuplements à GB_TGB capitalisés"
  ),
  taux = c(
    0.165, # médiane du entre 0 et mediane Tx prelevement # Faible coupe
    0.33,  # médiane du Tx prélèvement et > Q3 Tx prelevement Hardwood # Coupe modérée
    0.595,  # médiane des 0.33-0.86 car Q3 TX_prelevement = 0.86 # Coupe forte 
    1.05    #  >0.86 car Q3 et =Q3 du Tx prevelement pour Softwood # Forte coupe peuplement à GB/TGB capitalisés
  )
)

# =========================
# 7. PREDICTION PAR SCENARIO
# =========================

# Maximum de G_TOT_CUT possible parmi tous les scénarios
g_min <- 0

g_max <- global(
  G_r * max(scenarios$taux),
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

# Prédiction sur l'échelle log
mu <- posterior_linpred(
  mod_vitesse,
  newdata = data.frame(
    G_TOT_CUT = g_seq
  )
)

# Récupération de sigma
draws <- as_draws_df(mod_vitesse)
sigma <- draws$sigma

# Retour à l'échelle m3/j :
# moyenne lognormale = exp(mu + sigma²/2)

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


# Table de correspondance
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
  G_coupe <- G_r * scenarios$taux[i]
  
  
  # Vérification des plages de G coupée
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
      "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_cable_",
      scenarios$classe[i],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  
  rm(
    G_coupe,
    vitesse_r
  )
  
  gc()
}


# =========================
# VERIFICATION MODELE
# =========================

summary(mod_vitesse)
fixef(mod_vitesse)

