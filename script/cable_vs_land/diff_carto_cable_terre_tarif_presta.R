# =========================
# DIFFERENCE ENTRE DEUX CARTES DE TARIF DE PRESTATION
# =========================

library(terra)

# =========================
# 1. FICHIERS
# =========================

tarif_terre <- rast("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_terre_foret.tif")

tarif_cable <- rast("CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_foret.tif")

# Fichier de sortie
presta_difference <- "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/carto_diff_tarif_prestation.tif"


print(tarif_terre)
print(tarif_cable)

# Vérification de l'emprise
print(ext(tarif_terre))
print(ext(tarif_cable))


# =========================
# 5. CALCUL DE LA DIFFERENCE
# =========================

# Différence :
# TARIF CABLE - TARIF TERRESTRE

difference <- tarif_cable - tarif_terre 


# =========================
# 6. INFORMATIONS SUR LE RESULTAT
# =========================

print(difference)

cat("\nRésumé de la différence :\n")
print(global(
  difference,
  fun = c("min", "max", "mean", "sd"),
  na.rm = TRUE
))


# =========================
# 7. EXPORT
# =========================

writeRaster(
  difference,
  presta_difference,
  overwrite = TRUE
)

# =========================
# 8. AFFICHAGE
# =========================

plot(
  difference,
  main = "Différence de tarif de prestation\n Cable - Ground based",
  col = hcl.colors(100, "RdBu", rev = TRUE)
)
