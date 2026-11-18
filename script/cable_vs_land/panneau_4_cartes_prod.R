# ============================================================
# FIGURE 4 PANNEAUX : PRODUCTIVITE
# ============================================================
install.packages("patchwork")
library(terra)
library(tmap)
library(sf)
library(patchwork)
library(ggplot2)
library(grid)
library(gridExtra)
library(viridisLite)

# ============================================================
# 1. RASTERS
# ============================================================

prod_terre <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_terre_coupe_rase_foret.tif"
)

prod_cable <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/VITESSE_cable_coupe_rase_foret.tif"
)

prod_difference <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/DIFF_VITESSE_cable_terre_coupe_rase.tif"
)

scenario_prod <- rast(
  "CloudFiles/beta-lorraine-sync/Documents/scable_dev/carto/prod_diff_scenario_3.tif"
)


# ============================================================
# 2. MODE PLOT
# ============================================================

tmap_mode("plot")

# ============================================================
# PLAGE COMMUNE AUX DEUX RASTERS
# ============================================================

vals_communs <- c(
  terra::values(prod_terre),
  terra::values(prod_cable)
)

vals_communs <- vals_communs[!is.na(vals_communs)]

breaks_communs <- quantile(
  vals_communs,
  probs = seq(0, 1, length.out = 6),
  na.rm = TRUE
)

breaks_communs <- unique(breaks_communs)

prod_comparaison <- c(prod_terre, prod_cable)

names(prod_comparaison) <- c("Terrestre", "Câble")

# ============================================================
# 3. CARTES — SANS AUCUNE LÉGENDE
# ============================================================

fig_maps <- tm_shape(prod_comparaison) +
  tm_raster(
    col = tm_vars(),
    col.scale = tm_scale_intervals(
      breaks = breaks_communs,
      values = "viridis"
    ),
    col.legend = tm_legend_hide(),
    col.free = FALSE
  ) +
  tm_facets(
    ncol = 2
  ) +
  tm_layout(
    panel.labels = c("Ground-based (a)", "Cable (b)"),
    panel.label.size = 1,
    panel.label.bg = FALSE,
    panel.label.frame = FALSE,
    frame = FALSE
  )


# ============================================================
# 4. COULEURS DE LA LÉGENDE
# ============================================================

n_classes <- length(breaks_communs) - 1

cols <- viridisLite::viridis(
  n_classes,
  direction = 1
)


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
    "Productivity (m³/day)",
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



# ============================================================
# 10. CARTE (C) — DIFFÉRENCE
# ============================================================

p3 <- tm_shape(prod_difference) +
  tm_raster(
    palette = "YlOrRd",
    style = "quantile",
    n = 5,
    title = "(m³/year)",
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
# 11. CARTE (D) — SCÉNARIO
# ============================================================

p4 <- tm_shape(scenario_prod) +
  tm_raster(
    palette = "YlOrRd",
    style = "quantile",
    n = 5,
    title = "(m³/year)",
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
# 12. ASSEMBLAGE
# ============================================================

fig2 <- tmap_arrange(
  p3, p4,
  ncol = 2
)

fig2

# ============================================================
# PLAGE COMMUNE AUX DEUX RASTERS
# ============================================================

vals_communs <- c(
  terra::values(prod_difference),
  terra::values(scenario_prod)
)

vals_communs <- vals_communs[!is.na(vals_communs)]

breaks_communs <- quantile(
  vals_communs,
  probs = seq(0, 1, length.out = 6),
  na.rm = TRUE
)

breaks_communs <- unique(breaks_communs)

prod_comparaison <- c(
  prod_difference,
  scenario_prod
)

names(prod_comparaison) <- c(
  "Diff_er",
  "Scenario"
)


# ============================================================
# 3. CARTES — SANS AUCUNE LÉGENDE
# ============================================================

cols <- rev(c(
  "#FFFFB2",
  "#FECC5C",
  "#FD8D3C",
  "#F03B20",
  "#BD0026"
))

fig_maps <- tm_shape(prod_comparaison) +
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
    panel.labels = c(
      "Current difference (c)",
      "Predictive scenario (d)"
    ),
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
      digits = 0
    )
  ),
  paste0(
    "< ",
    formatC(
      breaks_communs[3],
      format = "f",
      digits = 0
    )
  ),
  paste0(
    "< ",
    formatC(
      breaks_communs[4],
      format = "f",
      digits = 0
    )
  ),
  paste0(
    "< ",
    formatC(
      breaks_communs[5],
      format = "f",
      digits = 0
    )
  ),
  paste0(
    ">= ",
    formatC(
      breaks_communs[5],
      format = "f",
      digits = 0
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
    "Production difference (m³/year)",
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

final_fig2 <- grid.arrange(
  maps_grob,
  legend_grob,
  ncol = 1,
  heights = c(1, 0.13)
)


# ============================================================
# 9. AFFICHAGE
# ============================================================

grid.newpage()
grid.draw(final_fig2)

