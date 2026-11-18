# =========================
# DIFFERENCE DE PRODUCTIVITE
# CABLE - TERRESTRE
# HYPOTHESE : COUPE RASE
# =========================

library(terra)
library(RColorBrewer)

# =========================
# DOSSIER
# =========================

dossier <- "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/"

# =========================
# CHARGEMENT DES CARTES
# =========================

vitesse_terre <- rast(
  paste0(
    dossier,
    "VITESSE_terre_coupe_rase_foret.tif"
  )
)

vitesse_cable <- rast(
  paste0(
    dossier,
    "VITESSE_cable_coupe_rase_foret.tif"
  )
)

# =========================
# VERIFICATION
# =========================

if (!compareGeom(
  vitesse_terre,
  vitesse_cable,
  stopOnError = FALSE
)) {
  stop("Les rasters câble et terrestre ne correspondent pas.")
}

# =========================
# DIFFERENCE DE PRODUCTIVITE
# =========================

# positif = câble plus rapide
# négatif = terrestre plus rapide

diff_vitesse <- vitesse_cable - vitesse_terre

# =========================
# RESUME
# =========================

cat(
  "Différence min-max :",
  global(diff_vitesse, "min", na.rm = TRUE)[1,1],
  "-",
  global(diff_vitesse, "max", na.rm = TRUE)[1,1],
  "m3/j\n"
)

cat(
  "Différence moyenne :",
  global(diff_vitesse, "mean", na.rm = TRUE)[1,1],
  "m3/j\n"
)

# =========================
# SAUVEGARDE
# =========================

writeRaster(
  diff_vitesse,
  filename = paste0(
    dossier,
    "DIFF_VITESSE_cable_terre_coupe_rase.tif"
  ),
  overwrite = TRUE
)

# =========================
# AFFICHAGE
# =========================

plot(
  diff_vitesse,
  col = rev(RColorBrewer::brewer.pal(11, "YlOrRd")),
  main = "Différence de productivité\n(Câble - Terrestre)"
)

# =========================
# NETTOYAGE
# =========================

rm(
  vitesse_terre,
  vitesse_cable,
  diff_vitesse
)

gc()