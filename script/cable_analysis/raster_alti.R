# =========================
# CREATION DU RASTER D'ALTITUDE A PARTIR DE LA BD ALTI 25m IGN
# =========================

library(terra)


# =========================
# 1. CHARGEMENT DES FICHIERS BD ALTI
# =========================

dir_base <- "~/Documents/S10_BETA/scable_dev/resources/public/BDALTI/"

files <- list.files(
  dir_base,
  pattern = "\\.[a][s][c]$",
  full.names = TRUE,
  recursive = TRUE
)

print(length(files))   # vérification du nombre de dalles trouvées


# =========================
# 2. CREATION DU MOSAIQUAGE VIRTUEL
# =========================

mnt <- terra::vrt(files)

# définition du système de coordonnées
crs(mnt) <- "EPSG:2154"


# =========================
# 3. EXPORT DU RASTER D'ALTITUDE
# =========================

writeRaster(
  mnt,
  "~/Documents/S10_BETA/scable_dev/resources/public/altitude.tif",
  overwrite = TRUE
)


# =========================
# 4. VERIFICATION
# =========================

altitude <- rast(
  "~/Documents/S10_BETA/scable_dev/resources/public/altitude.tif"
)

print(altitude)

plot(altitude)




