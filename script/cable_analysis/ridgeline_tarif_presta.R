###############################################################
# RIDGELINE DES DISTRIBUTIONS PREDICTIVES
# TARIF DE PRESTATION CABLE VS TERRESTRE
# ECHELLE ORIGINALE €/m3
###############################################################

library(brms)
library(ggridges)
library(ggplot2)
library(dplyr)

###############################################################
# CHARGEMENT DES MODELES BAYESIENS
###############################################################

mod_tarif_presta_cable <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_tarif_presta_cable.rds"
)

mod_tarif_presta_terre <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_tarif_presta_terre.rds"
)

###############################################################
# 1. LISTE DES MODELES
###############################################################

model <- list(
  
  "Cable - harvesting service rate" =
    mod_tarif_presta_cable,
  
  "Ground-based - service rate" =
    mod_tarif_presta_terre

)

###############################################################
# 1. FONCTION PREDICTIONS
###############################################################

posterior_predictions_tarif <- function(model,
                                        system){
  
  
  # récupération automatique des données du modèle
  
  data_model <- model$data
  
  
  # prédictions sur l'échelle réelle €/m3
  
  pred <- posterior_epred(
    model,
    newdata = data_model,
    re_formula = NA
  )
  
  
  data.frame(
    
    Tarif = as.vector(pred),
    
    System = system
    
  )
  
}



###############################################################
# 2. PREDICTIONS DES DEUX MODELES
###############################################################

pred_cable_tarif <-
  
  posterior_predictions_tarif(
    
    mod_tarif_presta_cable,
    
    "Cable"
    
  )



pred_terre_tarif <-
  
  posterior_predictions_tarif(
    
    mod_tarif_presta_terre,
    
    "Ground-based"
    
  )



pred_tarif_all <-
  
  bind_rows(
    
    pred_cable_tarif,
    
    pred_terre_tarif
    
  )



###############################################################
# 3. NETTOYAGE
###############################################################

pred_tarif_all <- pred_tarif_all %>%
  
  filter(
    !is.na(Tarif),
    !is.na(System)
  )



###############################################################
# 4. RIDGELINE
###############################################################

figure_ridge_tarif <-
  
  ggplot(
    
    pred_tarif_all,
    
    aes(
      
      x = Tarif,
      
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
    
    x = "Predicted harvesting extraction rate (EUR/m³)",
    
    y = NULL,
    
    fill = "System"
    
  )+
  
  
  theme_ridges(
    
    font_size = 12
    
  )+
  
  
  theme(
    
    legend.position = "bottom"
    
  )



figure_ridge_tarif



###############################################################
# 5. EXPORT
###############################################################

ggsave(
  
  "Ridgeline_harvesting_cost_cable_vs_ground.png",
  
  figure_ridge_tarif,
  
  width = 9,
  
  height = 5,
  
  dpi = 300
  
)

pred_tarif_all %>%
  group_by(System) %>%
  summarise(
    mean = mean(Tarif),
    median = median(Tarif),
    from = min (Tarif),
    to = max(Tarif),
    lower_95 = quantile(Tarif, 0.025),
    upper_95 = quantile(Tarif, 0.975),
    lower_IQR = quantile(Tarif, 0.25),
    upper_IQR = quantile(Tarif, 0.75),
    .groups = "drop"
  )

###############################################################
# FIN
###############################################################