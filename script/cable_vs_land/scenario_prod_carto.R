# =========================
# SCENARIO DIFFERENCE DE PRODUCTIVITE ANNUELLE
# CABLE - TERRESTRE
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
# 3. APPLICATION DES SCÉNARIOS
# =========================

# Productivité terrestre : passage annuel avec 66 jours en moins
prod_terre_scenario_3mois <- vitesse_terre * 143
# Productivité terrestre : passage annuel avec 100 jours en moins
prod_terre_scenario_5mois <- vitesse_terre * 109

# Productivité câble : passage annuel
prod_cable_scenario <- vitesse_cable * 255

# =========================
# DIFFERENCE DE PRODUCTIVITE
# =========================

# positif = câble plus rapide
# négatif = terrestre plus rapide

diff_scenario_3 <- prod_cable_scenario - prod_terre_scenario_3mois
diff_scenario_5 <- prod_cable_scenario - prod_terre_scenario_5mois

# =========================
# 5. SAUVEGARDE
# =========================

writeRaster(
  diff_scenario_3,
  filename = paste0(
    dossier,
    "prod_diff_scenario_3.tif"
  ),
  overwrite = TRUE
)

writeRaster(
  diff_scenario_5,
  filename = paste0(
    dossier,
    "prod_diff_scenario_5.tif"
  ),
  overwrite = TRUE
)

print(diff_scenario_3)
print(diff_scenario_5)

cat("\nRésumé de la différence :\n")
print(global(
  diff_scenario_3,
  c("min", "mean", "max"),
  na.rm = TRUE
))

cat("\nRésumé de la différence :\n")
print(global(
  diff_scenario_5,
  c("min", "mean", "max"),
  na.rm = TRUE
))

plot(diff_scenario_3)
plot(diff_scenario_5)
