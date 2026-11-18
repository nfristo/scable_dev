# ============================================================
# BD FORET IGN V2
# RASTER ESSENCE MAJORITAIRE - FRANCE 25 m
# ============================================================
#
# Classes finales :
#   1 = FEUILLUS
#   2 = RESINEUX
#
# AUCUNE classe MIXTE.
#
# Pour les peuplements mélangés :
#   - feuillus prépondérants -> FEUILLUS
#   - conifères prépondérants -> RESINEUX
#
# Les formations sans information permettant de déterminer
# une majorité sont laissées en NA.
#
# Projection : Lambert 93 EPSG:2154
# Résolution : 25 m
# ============================================================


# ============================================================
# 0. LIBRAIRIES
# ============================================================

library(sf)
library(terra)
library(dplyr)


# ============================================================
# 1. DOSSIERS
# ============================================================

dir_base <- "~/Documents/S10_BETA/scable_dev/BDFORET"

dir_raster <- file.path(
  dir_base,
  "rasters_25m"
)

dir.create(
  dir_raster,
  showWarnings = FALSE,
  recursive = TRUE
)


# ============================================================
# 2. RECHERCHE DES SHAPEFILES
# ============================================================

message("Recherche des shapefiles BD Forêt...")

shp_files <- list.files(
  dir_base,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

message(
  "Nombre de shapefiles trouvés : ",
  length(shp_files)
)

if(length(shp_files) == 0){
  
  stop(
    "Aucun shapefile trouvé dans : ",
    dir_base
  )
  
}


# ============================================================
# 3. TRAITEMENT DE CHAQUE DEPARTEMENT
# ============================================================

rasters <- character(0)


for(i in seq_along(shp_files)){
  
  shp <- shp_files[i]
  
  message("\n")
  message("================================================")
  message(
    "DEPARTEMENT ",
    i,
    " / ",
    length(shp_files)
  )
  message(
    "Fichier : ",
    basename(shp)
  )
  message("================================================")
  
   
# ==========================================================
# 3.1 LECTURE
# ==========================================================
  
  foret <- st_read(
    shp,
    quiet = TRUE
  )
  
  
# ==========================================================
# 3.2 PROJECTION
# ==========================================================
  
foret <- st_transform(
    foret,
    2154
  )
  
  
# ==========================================================
# 3.3 CLASSIFICATION DU TYPE D'ESSENCE
# ==========================================================
  
  foret <- foret %>%
    mutate(
      TYPE_ESSENCE = case_when(
        
        # ----------------------------------------------------
        # FEUILLUS
        # ----------------------------------------------------
        
        ESSENCE %in% c(
          "Feuillus",
          "Châtaignier",
          "Chênes décidus",
          "Hêtre",
          "Peuplier",
          "Robinier"
        ) ~ "FEUILLUS",
        
        # ----------------------------------------------------
        # RESINEUX
        # ----------------------------------------------------
        
        ESSENCE %in% c(
          "Conifères",
          "Douglas",
          "Mélèze",
          "Pin autre",
          "Pin laricio, pin noir",
          "Pin sylvestre",
          "Pins mélangés",
          "Sapin, épicéa"
        ) ~ "RESINEUX",
        
        # ----------------------------------------------------
        # MELANGE AVEC RESINEUX PREPONDERANTS
        # ----------------------------------------------------
        
        TFV %in% c(
          "Forêt fermée à mélange de conifères prépondérants et feuillus"
        ) ~ "RESINEUX",
        
        # ----------------------------------------------------
        # MELANGE AVEC FEUILLUS PREPONDERANTS
        # ----------------------------------------------------
        
        TFV %in% c(
          "Forêt fermée à mélange de feuillus prépondérants et conifères"
        ) ~ "FEUILLUS",
        
        # ----------------------------------------------------
        # CAS NON DETERMINABLE
        # ----------------------------------------------------
        
        TRUE ~ NA_character_
      )
    )
  
  
# ==========================================================
# 3.4 CONTROLE
# ==========================================================
  
  message("\nRépartition :")
  
  print(
    table(
      foret$TYPE_ESSENCE,
      useNA = "ifany"
    )
  )
  
  
  # ==========================================================
  # 3.5 CODAGE
  # ==========================================================
  
  foret$CODE_ESSENCE <- case_when(
    foret$TYPE_ESSENCE == "FEUILLUS" ~ 1,
    foret$TYPE_ESSENCE == "RESINEUX" ~ 2,
    TRUE ~ NA_real_
  )
  
  
  # ==========================================================
  # 3.6 RASTER 25 m
  # ==========================================================
  
  emprise <- ext(vect(foret))
  
  r <- rast(
    emprise,
    resolution = 25,
    crs = "EPSG:2154"
  )
  
  
  # ==========================================================
  # 3.7 RASTERISATION
  # ==========================================================
  
  message("Rasterisation 25 m...")
  
  r_essence <- rasterize(
    vect(foret),
    r,
    field = "CODE_ESSENCE"
  )
  
  
  # ==========================================================
  # 3.8 NOM DU DEPARTEMENT
  # ==========================================================
  
  # Le shapefile s'appelle apparemment FORMATION_VEGETALE.shp
  # donc basename(shp) ne permet pas d'identifier le département.
  #
  # On récupère le nom du dossier contenant le shapefile.
  
  nom_dep <- basename(dirname(shp))
  
  # Nettoyage éventuel du nom
  
  nom_dep <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    nom_dep
  )
  
  
  # ==========================================================
  # 3.9 NOM DU RASTER
  # ==========================================================
  
  fichier_raster <- file.path(
    dir_raster,
    paste0(
      "TYPE_ESSENCE_25m_",
      nom_dep,
      ".tif"
    )
  )
  
  
  # ==========================================================
  # 3.10 ECRITURE
  # ==========================================================
  
  message(
    "Ecriture : ",
    basename(fichier_raster)
  )
  
  writeRaster(
    r_essence,
    fichier_raster,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT1U",
      gdal = c(
        "COMPRESS=DEFLATE",
        "PREDICTOR=2"
      )
    )
  )
  
  
  # Ajouter le fichier à la liste des rasters
  
  rasters <- c(
    rasters,
    fichier_raster
  )
  
  
  # ==========================================================
  # 3.11 CONTROLE DU FICHIER CREE
  # ==========================================================
  
  if (file.exists(fichier_raster)) {
    
    message(
      "Raster créé : ",
      basename(fichier_raster)
    )
    
  } else {
    
    warning(
      "ERREUR : le raster n'a pas été créé : ",
      fichier_raster
    )
  }
  
  
  # ==========================================================
  # 3.12 NETTOYAGE
  # ==========================================================
  
  rm(
    foret,
    r,
    r_essence
  )
  
  gc()
  
  message(
    "Département terminé : ",
    nom_dep
  )

}   
  
# ============================================================
# 4. MOSAIQUE FRANCE
# ============================================================

message("\n")
message("================================================")
message("CREATION DE LA MOSAIQUE FRANCE")
message("================================================")


rasters <- list.files(
  dir_raster,
  pattern = "\\.tif$",
  full.names = TRUE
)


message(
  "Nombre de rasters départementaux : ",
  length(rasters)
)


r_list <- lapply(
  rasters,
  rast
)


message(
  "Mosaïque en cours..."
)


r_france <- do.call(
  mosaic,
  c(
    r_list,
    list(
      fun = "first"
    )
  )
)


# ============================================================
# 5. TABLE DE CATEGORIES
# ============================================================

levels(r_france) <- data.frame(
  
  ID = c(
    1,
    2
  ),
  
  TYPE_ESSENCE = c(
    "FEUILLUS",
    "RESINEUX"
  )
  
)


# ============================================================
# 6. EXPORT FINAL
# ============================================================

sortie_finale <- file.path(
  dir_base,
  "TYPE_ESSENCE_MAJORITAIRE_FRANCE_25m.tif"
)


message(
  "Ecriture du raster final..."
)


writeRaster(
  r_france,
  sortie_finale,
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c(
      "COMPRESS=DEFLATE",
      "PREDICTOR=2"
    )
  )
)


# ============================================================
# 7. VERIFICATION
# ============================================================

message("\n")
message("================================================")
message("RESULTAT FINAL")
message("================================================")

print(r_france)

message(
  "\nRaster final :\n",
  sortie_finale
)

message(
  "\nRésolution : ",
  paste(
    res(r_france),
    collapse = " x "
  ),
  " m"
)

message(
  "\nCRS : ",
  crs(r_france)
)


# ============================================================
# 8. FREQUENCES
# ============================================================

message(
  "\nFréquence des classes :"
)

print(
  freq(r_france)
)


# ============================================================
# 9. APERCU
# ============================================================

plot(
  r_france,
  main = "Essence majoritaire - France métropolitaine - 25 m"
)
