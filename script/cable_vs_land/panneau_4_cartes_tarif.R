# ============================================================
# FIGURE 4 PANNEAUX : TARIFS DE PRESTATION
# ============================================================
install.packages("patchwork")
library(terra)
library(tmap)
library(sf)
library(patchwork)

# ============================================================
# 1. RASTERS
# ============================================================

tarif_terre <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_terre_foret.tif"
)

tarif_cable <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/tarif_prestation_foret.tif"
)

presta_difference <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/carto_diff_tarif_prestation.tif"
)

scenario_tarif <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/carto_scenario_tarif.tif"
)


# ============================================================
# 2. MODE PLOT
# ============================================================

tmap_mode("plot")

# ============================================================
# 3. LIMITES COMMUNES
# ============================================================

# Même échelle pour les deux cartes de tarif
range_tarif <- range(
  c(
    global(tarif_terre, "min", na.rm = TRUE)[1,1],
    global(tarif_terre, "max", na.rm = TRUE)[1,1],
    global(tarif_cable, "min", na.rm = TRUE)[1,1],
    global(tarif_cable, "max", na.rm = TRUE)[1,1]
  )
)

# Breaks communs
breaks_tarif <- pretty(range_tarif, n = 6)

# On s'assure que les breaks couvrent bien toute la plage
breaks_tarif <- c(
  min(range_tarif),
  breaks_tarif[breaks_tarif > min(range_tarif) &
                 breaks_tarif < max(range_tarif)],
  max(range_tarif)
)

breaks_tarif <- sort(unique(breaks_tarif))

# 5 classes de largeur égale
breaks_tarif <- seq(
  range_tarif[1],
  range_tarif[2],
  length.out = 6
)

print(breaks_tarif)


# Même échelle pour les deux cartes de différence
range_diff <- range(
  c(
    global(presta_difference, "min", na.rm = TRUE)[1,1],
    global(presta_difference, "max", na.rm = TRUE)[1,1],
    global(scenario_tarif, "min", na.rm = TRUE)[1,1],
    global(scenario_tarif, "max", na.rm = TRUE)[1,1]
  )
)

# Échelle symétrique autour de 0
max_diff <- max(abs(range_diff))


# ============================================================
# 4. CARTE (A) — TERRESTRE
# ============================================================

p1 <- tm_shape(tarif_terre) +
  tm_raster(
    col.scale = tm_scale_continuous(
      values = "viridis",
      limits = range_tarif
    ),
    col.legend = tm_legend(
      title = "(EUR/m³)"
    )
  ) +
  tm_layout(
    title = "(a)",
    title.position = c("left", "top"),
    frame = FALSE
  )

# ============================================================
# 5. CARTE (B) — CÂBLE
# ============================================================

p2 <- tm_shape(tarif_cable) +
  tm_raster(
    col.scale = tm_scale_continuous(
      values = "viridis",
      limits = range_tarif
    ),
    col.legend = tm_legend(
      title = "(EUR/m³)"
    )
  ) +
  tm_layout(
    title = "(b)",
    title.position = c("left", "top"),
    frame = FALSE
  )

# ============================================================
# 6. CARTE (C) — DIFFÉRENCE
# ============================================================

p3 <- tm_shape(presta_difference) +
  tm_raster(
    palette = c(
      "#2166AC",
      "#67A9CF",
      "#BDBDBD",
      "#EF8A62",
      "#B2182B"
    ),
    limits = range_diff,
    title = "(EUR/m³)",
    legend.format = list(
      digits = 0, text.separator = ";"
    )
  ) +
  tm_layout(
    title = "(c)",
    title.position = c("left", "top"),
    frame = FALSE
  )
# ============================================================
# 7. CARTE (D) — SCÉNARIO
# ============================================================

p4 <- tm_shape(scenario_tarif) +
  tm_raster(
    palette = c(
      "#2166AC",
      "#67A9CF",
      "#BDBDBD",
      "#EF8A62",
      "#B2182B"
    ),
    limits = range_diff,
    title = "(EUR/m³)",
    legend.format = list(
      digits = 0, text.separator = ";"
    )
  ) +
  tm_layout(
    title = "(d)",
    title.position = c("left", "top"),
    frame = FALSE
  )

# ============================================================
# 8. ASSEMBLAGE
# ============================================================

tmap_arrange(
  p1, p2,
  ncol = 2
)

tmap_arrange(
  p3, p4,
  ncol = 2
)



# ============================================================
# PLAGE COMMUNE AUX DEUX RASTERS
# ============================================================

vals_communs <- c(
  terra::values(presta_difference),
  terra::values(scenario_tarif)
)

vals_communs <- vals_communs[!is.na(vals_communs)]

breaks_communs <- quantile(
  vals_communs,
  probs = seq(0, 1, length.out = 6),
  na.rm = TRUE
)

breaks_communs <- unique(breaks_communs)

tarif_comparaison <- c(presta_difference, scenario_tarif)

names(tarif_comparaison) <- c("Diff_er", "Scenario")

# ============================================================
# 3. CARTES — SANS AUCUNE LÉGENDE
# ============================================================

cols <- c(
  "#2166AC",
  "#67A9CF",
  "#BDBDBD",
  "#EF8A62",
  "#B2182B"
)

fig_maps <- tm_shape(tarif_comparaison) +
  tm_raster(
    col = tm_vars(),
    col.scale = tm_scale_intervals(
      breaks = breaks_communs,
      values = cols
    ),
    col.legend = tm_legend_hide(),
    col.free = FALSE
  ) +
  tm_facets(
    ncol = 2
  ) +
  tm_layout(
    panel.labels = c("Current difference (a)", "Predicitive scenario (b)"),
    panel.label.size = 1,
    panel.label.bg = FALSE,
    panel.label.frame = FALSE,
    frame = FALSE
  )


# ============================================================
# 4. COULEURS DE LA LÉGENDE
# ============================================================

n_classes <- length(breaks_communs) - 1

cols <- cols[seq_len(n_classes)]

# ============================================================
# 5. LABELS DES CLASSES
# ============================================================

labels <- c(
  paste0(
    "< ",
    formatC(
      breaks_communs[2],
      format = "f",
      digits = 1
    )
  ),
  paste0(
    "< ",
    formatC(
      breaks_communs[3],
      format = "f",
      digits = 1
    )
  ),
  paste0(
    "< ",
    formatC(
      breaks_communs[4],
      format = "f",
      digits = 1
    )
  ),
  paste0(
    "< ",
    formatC(
      breaks_communs[5],
      format = "f",
      digits = 1
    )
  ),
  paste0(
    "≥ ",
    formatC(
      breaks_communs[5],
      format = "f",
      digits = 1
    )
  )
)


# ============================================================
# 6. EXTRACTION DES CARTES
# ============================================================

maps_grob <- tmap_grob(fig_maps)


# ============================================================
# 7. LÉGENDE MANUELLE
# ============================================================

legend_grob <- grobTree(
  
  # Titre
  textGrob(
    "Extraction rate (EUR/m³)",
    x = 0.5,
    y = 0.95,
    just = "center",
    gp = gpar(
      fontsize = 11
    )
  ),
  
  # Rectangles de couleur
  rectGrob(
    x = seq(
      0.30,
      0.70,
      length.out = n_classes
    ),
    y = rep(0.52, n_classes),
    width = 0.10,
    height = 0.28,
    gp = gpar(
      fill = cols,
      col = NA
    )
  ),
  
  # Labels
  textGrob(
    labels,
    x = seq(
      0.30,
      0.70,
      length.out = n_classes
    ),
    y = rep(0.20, n_classes),
    just = "center",
    gp = gpar(
      fontsize = 10
    )
  )
)


# ============================================================
# 8. ASSEMBLAGE
# ============================================================

final_fig1 <- grid.arrange(
  maps_grob,
  legend_grob,
  ncol = 1,
  heights = c(1, 0.13)
)


# ============================================================
# 9. AFFICHAGE
# ============================================================

grid.draw(final_fig1)


