# =========================
# DIFFERENCE DE PRODUCTIVITE
# CABLE - TERRESTRE
# =========================

library(terra)

# Dossier contenant les cartes
dossier <- "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/"


# Scénarios
scenarios <- data.frame(
  classe = c(
    "Faible coupe",
    "Coupe modérée",
    "Coupe forte",
    "Forte coupe peuplements à GB_TGB capitalisés"
  )
)


# =========================
# BOUCLE DIFFERENCE
# =========================

for(i in seq_len(nrow(scenarios))){
  
  scenario <- scenarios$classe[i]
  
  cat(
    "\nCalcul différence :",
    scenario,
    "\n"
  )
  
  
  # Chemins des deux cartes
  fichier_terre <- paste0(
    dossier,
    "VITESSE_terre_",
    scenario,
    ".tif"
  )
  
  
  fichier_cable <- paste0(
    dossier,
    "VITESSE_cable_",
    scenario,
    ".tif"
  )
  
  
  # Chargement
  vitesse_terre <- rast(fichier_terre)
  vitesse_cable <- rast(fichier_cable)
  
  
  # Vérification projection/résolution
  if(!compareGeom(
    vitesse_terre,
    vitesse_cable,
    stopOnError = FALSE
  )){
    stop(
      paste(
        "Les rasters câble et terrestre ne correspondent pas pour",
        scenario
      )
    )
  }
  
  
  # =========================
  # DIFFERENCE
  # =========================
  
  # positif = câble plus rapide
  # négatif = terrestre plus rapide
  
  diff_vitesse <- vitesse_cable - vitesse_terre
  
  
  # Résumé
  cat(
    "Différence min-max :",
    global(diff_vitesse, "min", na.rm = TRUE)[1,1],
    "-",
    global(diff_vitesse, "max", na.rm = TRUE)[1,1],
    "m3/j\n"
  )
  
  
  # Sauvegarde
  
  writeRaster(
    diff_vitesse,
    filename = paste0(
      dossier,
      "DIFF_VITESSE_cable_terre_",
      scenario,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  
  rm(
    vitesse_terre,
    vitesse_cable,
    diff_vitesse
  )
  
  gc()
}

plot(
  rast(
    paste0(
      dossier,
      "DIFF_VITESSE_cable_terre_Forte coupe peuplements à GB_TGB capitalisés.tif"
    )
  ),
  col = rev(RColorBrewer::brewer.pal(11,"RdBu"))
)
