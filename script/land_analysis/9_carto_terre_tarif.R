# =========================
# CARTOGRAPHIE DU TARIF DE PRESTATION TERRESTRE
#
# Modèle :
# TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE + ALTITUDE_MOY
# =========================


# =========================
# 1. PACKAGES
# =========================

library(terra)
library(sf)
library(brms)
library(dplyr)

terraOptions(
  memfrac = 0.5,
  todisk = TRUE,
  progress = 10
)


# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================

ser <- st_read(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/ser_l93/ser_l93.shp",
  quiet = TRUE
)

# Raster du type d'essence majoritaire
type_essence_r <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/TYPE_ESSENCE_MAJORAITAIRE_FRANCE_25m.tif"
)


# =========================
# 3. QUANTILES D'ALTITUDE
# =========================

altitude <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/altitude.tif"
)

alt_breaks <- as.numeric(
  terra::global(
    altitude,
    fun = quantile,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  )[1, ]
)

rcl_altitude <- cbind(
  alt_breaks[-length(alt_breaks)],
  alt_breaks[-1],
  1:5
)

alti_cl <- classify(
  altitude,
  rcl_altitude,
  include.lowest = TRUE
)

writeRaster(
  alti_cl,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/alti_cl.tif",
  overwrite = TRUE
)

rm(altitude)
gc()


# =========================
# 4. CHARGEMENT DU MODÈLE
# =========================

mod_tarif_presta_terre <- readRDS(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/mod_tarif_presta_terre.rds"
)


# =========================
# 5. TYPES D'ESSENCE
# =========================

# Le modèle utilise deux modalités :
# 1 = FEUILLUS
# 2 = RESINEUX

type_essence_levels <- c(
  "FEUILLUS",
  "RESINEUX"
)


# =========================
# 6. GRILLE DE PRÉDICTION
# =========================

# 5 classes d'altitude × 2 types d'essence
#
# => 10 combinaisons

grid_pred <- expand.grid(
  alti_cl = 1:5,
  TYPE_ESSENCE = type_essence_levels
)


# =========================
# 7. VALEURS RÉELLES D'ALTITUDE
# =========================

grid_pred$ALTITUDE_MOY <- sapply(
  grid_pred$alti_cl,
  function(i)
    mean(
      alt_breaks[c(i, i + 1)]
    )
)


# =========================
# 8. PREDICTIONS
# =========================

pred <- posterior_epred(
  mod_tarif_presta_terre,
  newdata = grid_pred
)

grid_pred$tarif_mean <- apply(
  pred,
  2,
  mean
)

grid_pred$tarif_q05 <- apply(
  pred,
  2,
  quantile,
  0.05
)

grid_pred$tarif_q95 <- apply(
  pred,
  2,
  quantile,
  0.95
)


# =========================
# 9. DELTA TARIF
# =========================

data_train <- read.csv(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/data_chantiers_terre_output_carto"
)

# Référence :
# altitude médiane des données d'apprentissage
# + type d'essence FEUILLUS

ref <- data.frame(
  TYPE_ESSENCE = "FEUILLUS",
  ALTITUDE_MOY = median(
    data_train$ALTITUDE_MOY,
    na.rm = TRUE
  )
)

pred_ref <- posterior_epred(
  mod_tarif_presta_terre,
  newdata = ref
)

delta <- sweep(
  pred,
  1,
  pred_ref[, 1],
  "-"
)

grid_pred$delta_tarif <- apply(
  delta,
  2,
  mean
)

rm(pred)
rm(delta)
gc()


# =========================
# 10. ALIGNEMENT DU RASTER D'ESSENCE
# =========================

template <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/alti_cl.tif"
)

# Le raster d'essence est déjà en Lambert-93 et à une résolution d'environ 25 m.
# On le rééchantillonne sur la grille du MNT avec la méthode "near" car il s'agit d'un raster catégoriel.

type_essence_r <- resample(
  type_essence_r,
  template,
  method = "near"
)


# =========================
# 11. CRÉATION DE L'INDICE
# =========================

# Codes :
#
# TYPE_ESSENCE
# 1 = FEUILLUS
# 2 = RESINEUX
#
# ALTITUDE
# 1 à 5
#
# Indices :
#
# 101 = altitude 1 + FEUILLUS
# 102 = altitude 1 + RESINEUX
# 201 = altitude 2 + FEUILLUS
# 202 = altitude 2 + RESINEUX
# ...
# 501 = altitude 5 + FEUILLUS
# 502 = altitude 5 + RESINEUX

indice_r <- alti_cl * 100 + type_essence_r


# =========================
# 12. INDICE POUR LA GRILLE
# =========================

grid_pred$type_cl <- ifelse(
  grid_pred$TYPE_ESSENCE == "FEUILLUS",
  1,
  2
)

grid_pred$INDICE <- (
  grid_pred$alti_cl * 100 +
    grid_pred$type_cl
)


# =========================
# 13. TABLE DE CORRESPONDANCE
# =========================

lookup <- data.frame(
  INDICE = grid_pred$INDICE,
  TARIF = grid_pred$tarif_mean,
  DELTA = grid_pred$delta_tarif
)

print(lookup)


# =========================
# 14. ATTRIBUTION DES TARIFS
# =========================

vals <- values(indice_r)

tarif_vals <- lookup$TARIF[
  match(
    vals,
    lookup$INDICE
  )
]

delta_vals <- lookup$DELTA[
  match(
    vals,
    lookup$INDICE
  )
]


# =========================
# 15. RASTER DU TARIF
# =========================

tarif_r <- setValues(
  indice_r,
  tarif_vals
)


# =========================
# 16. RASTER DU DELTA
# =========================

delta_r <- setValues(
  indice_r,
  delta_vals
)


# =========================
# 17.  MASQUE FORESTIER ET SAUVEGARDE
# =========================

foret <- vect(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/masque_foret2.gpkg"
)


writeRaster(
  tarif_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_terre.tif",
  overwrite = TRUE
)

writeRaster(
  delta_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/delta_tarif_terre.tif",
  overwrite = TRUE
)

writeRaster(
  indice_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/indice_terre.tif",
  overwrite = TRUE
)


# =========================
# 18. MASQUE FORESTIER
# =========================

foret <- vect(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/masque_foret2.gpkg"
)

mask(
  tarif_r,
  foret,
  filename =
    "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_terre_foret.tif",
  overwrite = TRUE
)

mask(
  delta_r,
  foret,
  filename =
    "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/delta_tarif_terre.tif",
  overwrite = TRUE
)


# =========================
# 19. NETTOYAGE
# =========================

rm(
  ser,
  type_essence_r,
  alti_cl,
  indice_r,
  tarif_r,
  delta_r,
  template,
  lookup,
  grid_pred
)

gc()
