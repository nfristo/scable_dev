###############################################################
# RIDGELINE DES DISTRIBUTIONS PREDICTIVES
# PRODUCTIVITE CABLE VS TERRESTRE
# ECHELLE ORIGINALE m3/jour
###############################################################

library(brms)
library(ggridges)
library(ggplot2)
library(dplyr)


###############################################################
# CHARGEMENT DES MODELES BAYESIENS
###############################################################

mod_vitesse_intra_cable <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_vitesse_intra_cable.rds"
)

mod_vitesse_intra_terre <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_vitesse_intra_terre.rds"
)

###############################################################
# 1. LISTE DES MODELES
###############################################################

model <- list(
  
  "Cable - productivity" =
    mod_vitesse_intra_cable,
  
  "Ground-based - productivity" =
    mod_vitesse_intra_terre
  
)

###############################################################
# 1. GENERATION DES PREDICTIONS POSTERIEURES
###############################################################

G_values <- c(
  10,
  50,
  100,
  200
)

posterior_predictions <- function(model,
                                  system,
                                  G_values){
  
  
  newdata <- data.frame(
    G_TOT_CUT = G_values
  )
  
  
  pred <- posterior_epred(
    model,
    newdata = newdata,
    re_formula = NA
  )
  
  
  data.frame(
    
    Productivity = as.vector(pred),
    
    System = system
    
  )
  
}



###############################################################
# 2. PREDICTIONS DES MODELES
###############################################################


pred_cable <-
  
  posterior_predictions(
    
    mod_vitesse_intra_cable,
    
    "Cable",
    
    G_values
    
  )



pred_terre <-
  
  posterior_predictions(
    
    mod_vitesse_intra_terre,
    
    "Ground-based",
    
    G_values
    
  )



pred_all <-
  
  bind_rows(
    pred_cable,
    pred_terre
  )


###############################################################
# 3. RIDGELINE UNIQUE
###############################################################

figure_ridge_productivity <-
  
  ggplot(
    
    pred_all,
    
    aes(
      
      x = Productivity,
      
      y = System,
      
      fill = System
      
    )
    
  )+
  
  
  geom_density_ridges(
    
    alpha = 0.65,
    
    scale = 1.2,
    
    rel_min_height = 0.01
    
  )+
  
  scale_fill_manual(
    values = c(
      "Cable" = "#0057B8",
      "Ground-based" = "#D71920"
    )
  ) +
  
  
  labs(
    
    x = "Predicted productivity (m³/working day)",
    
    y = NULL,
    
    fill = "System"
    
  )+
  
  
  theme_ridges(
    
    font_size = 12
    
  )+
  
  
  theme(
    
    legend.position = "bottom"
    
  )



figure_ridge_productivity



###############################################################
# 4. EXPORT
###############################################################

ggsave(
  
  "Ridgeline_productivity_cable_vs_ground.png",
  
  figure_ridge_productivity,
  
  width = 9,
  
  height = 5,
  
  dpi = 300
  
)


###############################################################
# FIN
###############################################################
###############################################################
# RIDGELINE DES DISTRIBUTIONS PREDICTIVES
# PRODUCTIVITE CABLE VS TERRESTRE
# ECHELLE ORIGINALE m3/jour
# ALTERNATIVE EN UTILISANT LES PRÉDICTIONS DU MODELE SUR CHAQUE VALEUR DE G TOT CUT ET NON G VALUES
###############################################################

library(brms)
library(ggridges)
library(ggplot2)
library(dplyr)

###############################################################
# CHARGEMENT DES MODELES BAYESIENS
###############################################################

mod_vitesse_intra_cable <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_vitesse_intra_cable.rds"
)

mod_vitesse_intra_terre <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_vitesse_intra_terre.rds"
)

###############################################################
# 1. FONCTION PREDICTIONS
###############################################################

posterior_predictions_productivity <- function(model, system){
  
  # récupération automatique des données du modèle
  data_model <- model$data
  
  # prédictions sur l'échelle réelle m3/jour
  pred <- posterior_epred(
    model,
    newdata = data_model,
    re_formula = NA
  )
  
  data.frame(
    Productivity = as.vector(pred),
    System = system
  )
}

###############################################################
# 2. PREDICTIONS DES DEUX MODELES
###############################################################

pred_cable_productivity <-
  posterior_predictions_productivity(
    mod_vitesse_intra_cable,
    "Cable"
  )

pred_terre_productivity <-
  posterior_predictions_productivity(
    mod_vitesse_intra_terre,
    "Ground-based"
  )

pred_productivity_all <-
  bind_rows(
    pred_cable_productivity,
    pred_terre_productivity
  )

###############################################################
# 3. NETTOYAGE
###############################################################

pred_productivity_all <- pred_productivity_all %>%
  filter(
    !is.na(Productivity),
    !is.na(System)
  )

###############################################################
# 4. RIDGELINE
###############################################################

figure_ridge_productivity <-
  ggplot(
    pred_productivity_all,
    aes(
      x = Productivity,
      y = System,
      fill = System
    )
  ) +
  
  geom_density_ridges(
    alpha = 0.65,
    scale = 1.2,
    rel_min_height = 0.01
  ) +
  
  scale_fill_manual(
    values = c(
      "Cable" = "#0057B8",
      "Ground-based" = "#D71920"
    )
  ) +
  
  labs(
    x = "Predicted productivity (m³/working day)",
    y = NULL,
    fill = "System"
  ) +
  
  coord_cartesian(
    xlim = c(0, 200)
  ) +
  
  theme_ridges(
    font_size = 12
  ) +
  
  theme(
    legend.position = "bottom"
  )

figure_ridge_productivity

pred_all %>%
  group_by(System) %>%
  summarise(
    mean = mean(Productivity),
    median = median(Productivity),
    from = min (Productivity),
    to = max(Productivity),
    lower_95 = quantile(Productivity, 0.025),
    upper_95 = quantile(Productivity, 0.975),
    lower_IQR = quantile(Productivity, 0.25),
    upper_IQR = quantile(Productivity, 0.75),
    .groups = "drop"
  )
