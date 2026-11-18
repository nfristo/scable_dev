#=========================
# SCRIPT N°9 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de créer la cartographie spatialisée du tarif de prestation
# =========================

# Modèle pris en compte :

# TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY 

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

# OPTIONS MEMOIRE

terraOptions(
  memfrac = 0.5,
  todisk = TRUE,
  progress = 10
)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================

getwd()
list.files(recursive = TRUE)
list.files(recursive = TRUE, pattern = "\\.shp$")

ser <- st_read("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/ser_l93/ser_l93.shp",
               quiet = TRUE
)

table_diam_ser <- read.csv2("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/table_SER_diametres.csv")
names(table_diam_ser)[names(table_diam_ser) == "CODE_SER"] <- "codeser"

ser <- left_join(
  ser,
  table_diam_ser,
  by = "codeser"
)

SER_volume_bois_2024 <- read.csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/SER_volume_bois_2024.csv")
names(SER_volume_bois_2024)[names(SER_volume_bois_2024) == "code_territoire"] <- "codeser"

ser <- ser|>
  left_join(
    SER_volume_bois_2024|>
      select(codeser, moy_ha_stand,
             ic_moy_ha_stand),
    by = c("codeser")
  )

ser[, 3] <- NULL

# =========================
# 3. QUANTILES DE PENTE
# =========================

slope <- rast("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/slope.tif") # importation du fichier comprenant la pente

p_breaks <- as.numeric(
  terra::global(
    slope,
    fun = quantile,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  )[1, ]
) #calcul les seuils des quantiles de pente 
  
rcl_pente <- cbind(
  p_breaks[-length(p_breaks)],
  p_breaks[-1],
  1:5
) # permet de créer la table de classement des quantiles, avec les valeurs obtenues pour les quantiles correspondants,
#les bornes inf et supp et le numéro du quantile

pente_cl <- classify(
  slope,
  rcl_pente,
  include.lowest = TRUE
) # permet de modifier les valeurs de pente du raster pour leur assigner la valeur de quantiles correspondant
# classe 1 = terrain plat, classe 5 = terrain très pentu 
#permet de transformer la variable continue de pente en variable discrète de classe, plus de lisibilité cartographique 

writeRaster(
  pente_cl,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/pente_cl.tif",
  overwrite = TRUE
)

rm(slope)
gc()

# =========================
# 4. QUANTILES DE VOLUME
# =========================

vol_breaks <- quantile(
  ser$moy_ha_stand,
  probs = seq(0,1,0.2),
  na.rm = TRUE
) #calcul les seuils des quantiles de volume

ser$VOL_CL <- cut(
  ser$moy_ha_stand,
  breaks = vol_breaks,
  include.lowest = TRUE,
  labels = 1:5
) # permet de créer la table de classement des quantiles, avec les valeurs obtenues pour les quantiles correspondants,
#la forme est ici un facteur donc il faut repasser en nombre

ser$VOL_CL <- as.numeric(
  as.character(
    ser$VOL_CL
  )
) # as numeric + as character parce que as numeric seule ne garderait pas les valeurs


# =========================
# 5. RASTERISATION DU VOLUME
# =========================

template <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/pente_cl.tif"
) # Modèle sur lequel on calque 

vol_r <- rasterize(
  vect(ser),
  template,
  field = "VOL_CL"
)  # on rasterise les données vectorielles du volume des SER

writeRaster(
  vol_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/vol_cl.tif",
  overwrite = TRUE
)

rm(template)
gc()

# =========================
# 6. EXPAND.GRID
# =========================

grid_pred <- expand.grid(
  pente_cl = 1:5,
  vol_cl = 1:5
) # permet de créer toutes les combinaisons possibles entre classes de pente et classes de volume


# =========================
# 7. PASSAGE AUX VALEURS REELLES DANS GRID
# =========================

grid_pred$PENTE_MOY <- sapply(
  grid_pred$pente_cl,
  function(i)
    mean(
      p_breaks[c(i,i+1)]
    )
) # On associe ici le numéro de classe de la pente dans expand grid à la valeur réelle correspondante 

grid_pred$VOL_HA_MOY <- sapply(
  grid_pred$vol_cl,
  function(i)
    mean(
      vol_breaks[c(i,i+1)]
    )
) 
# On associe ici le numéro de classe de volume dans expand grid à la valeur réelle correspondante 

#grid_pred$LONGUEUR_MOY <- long_ref


# =========================
# 8. PREDICTIONS BAYESIENNES
# =========================
mod_tarif_presta_cable <- readRDS("CloudFiles/beta-lorraine-sync/Documents/scable_dev/mod_tarif_presta_cable.rds")

pred <- posterior_epred(
  mod_tarif_presta_cable,
  newdata = grid_pred
) #On utilise le modèle bayésien pour prédire le tarif de débardage pour chacune des 25 combinaisons pente × volume

grid_pred$tarif_mean <- apply(pred, 2, mean)
grid_pred$tarif_q05 <- apply(pred, 2, quantile, 0.05)
grid_pred$tarif_q95 <- apply(pred, 2, quantile, 0.95)

# =========================
# 9. DELTA TARIF
# =========================

# On calcule ici un tarif relatif (ou écart au cas moyen) pour chaque combinaison pente × volume,

data_train <- read.csv("CloudFiles/beta-lorraine-sync/Documents/scable_dev/data_train_cable_tarif")

ref <- data.frame(
  PENTE_MOY = median(
    data_train$PENTE_MOY),
  VOL_HA_MOY = median(
    data_train$VOL_HA_MOY)
) #création du point de ref à partir de médianes des variables

pred_ref <- posterior_epred(
  mod_tarif_presta_cable,
  newdata = ref
) #prédiction du prix sur ce point référence

delta <- sweep(
  pred,
  1,
  pred_ref[,1],
  "-"
) # calcul des écarts de prix au point référence

grid_pred$delta_tarif <- apply(delta, 2, mean) # résumé des deltas

rm(pred)
rm(delta)
gc()


# =========================
# 10. CREATION DE L'INDICE RASTER
# =========================

pente_cl <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/pente_cl.tif"
)

vol_r <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/vol_cl.tif"
)

indice_r <- pente_cl * 10 + vol_r # permet de créer un indice combinant la valeur de pente et de volume
# exemple : classe de pente 5 = 50 + classe de volume 3 = 3 = 50+3= 53 
# chaque pixel comporte une combinaison permettant d'identifier immédiatement les valeurs de pente et volume 
# 34 = classe de pente 3 et classe de volume 4

grid_pred$INDICE <- grid_pred$pente_cl * 10 + grid_pred$vol_cl

writeRaster(
  indice_r,
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/indice.tif",
  overwrite = TRUE
)

# =========================
# 11. TABLE DE CORRESPONDANCE
# =========================

lookup <- data.frame(
  INDICE = grid_pred$INDICE,
  TARIF = grid_pred$tarif_mean,
  DELTA = grid_pred$delta_tarif
) # on créé ici une table de correspondance associant un indice avec un tarif de presta et un delta au prix moyen

# =========================
# 12. ATTRIBUTION DES TARIFS
# =========================

vals <- values(indice_r)
tarif_vals <- lookup$TARIF[
  match(vals, lookup$INDICE)
] # on transforme valeurs du raster en vecteur
delta_vals <- lookup$DELTA[
  match(vals, lookup$INDICE)
]
tarif_r <- setValues(indice_r, tarif_vals) # création des rasters finaux
delta_r <- setValues(indice_r, delta_vals)

writeRaster(tarif_r, "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation.tif", overwrite = TRUE)
writeRaster(delta_r, "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/delta_tarif.tif", overwrite = TRUE)


foret <- vect("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/masque_foret2.gpkg")

mask(
  tarif_r,
  foret,
  filename = "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_foret.tif",
  overwrite = TRUE
)

