# =========================
# PROSPECTIVE SCENARIOS
# =========================

library(terra)

# =========================
# 1. FICHIERS
# =========================

tarif_terre <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_terre_foret.tif"
)

tarif_cable <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_foret.tif"
)

# Fichier de sortie
scenario_tarif <- paste0(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/",
  "carto_scenario_tarif.tif"
)

# =========================
# 2. VÉRIFICATION DES RASTERS
# =========================

print(tarif_terre)
print(tarif_cable)


# =========================
# 3. APPLICATION DES SCÉNARIOS
# =========================

# Tarif terrestre : +20 %
tarif_terre_scenario <- tarif_terre * 1.20

# Tarif câble : -30 %
tarif_cable_scenario <- tarif_cable * 0.70

# =========================
# 4. CALCUL DE LA DIFFÉRENCE
# =========================

# Différence :
# tarif câble scénarisé - tarif terrestre scénarisé

difference_scenario <- tarif_cable_scenario - tarif_terre_scenario 

# =========================
# 5. SAUVEGARDE
# =========================

writeRaster(
  difference_scenario,
  scenario_tarif,
  overwrite = TRUE
)

print(difference_scenario)

cat("\nRésumé de la différence :\n")
print(global(
  difference_scenario,
  c("min", "mean", "max"),
  na.rm = TRUE
))

