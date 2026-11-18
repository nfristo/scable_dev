###############################################################
# PRESENTATION ARTICLE - MODELES BAYESIENS
# BLOC 1/2 : TABLEAUX DE RESULTATS
###############################################################

library(brms)
library(loo)
library(dplyr)
library(purrr)
library(tibble)
library(knitr)

###############################################################
# CHARGEMENT DES MODELES BAYESIENS
###############################################################

mod_tarif_presta_cable <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_tarif_presta_cable.rds"
)

mod_tarif_presta_terre <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_tarif_presta_terre.rds"
)

mod_vitesse_intra_cable <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_vitesse_intra_cable.rds"
)

mod_vitesse_intra_terre <- readRDS(
  "~/Documents/S10_BETA/scable_dev/results/models/mod_vitesse_intra_terre.rds"
)
###############################################################
# 1. LISTE DES MODELES
###############################################################

models <- list(
  
  "Cable - harvesting service rate" =
    mod_tarif_presta_cable,
  
  "Ground-based - service rate" =
    mod_tarif_presta_terre,
  
  "Cable - productivity" =
    mod_vitesse_intra_cable,
  
  "Ground-based - productivity" =
    mod_vitesse_intra_terre
  
)


###############################################################
# 2. TABLEAU DES COEFFICIENTS POSTERIEURS
###############################################################

table_coefficients <-
  
  imap_dfr(
    models,
    function(model, name){
      
      
      coef <- fixef(model) %>%
        as.data.frame() %>%
        rownames_to_column("Parameter")
      
      
      coef %>%
        
        rename(
          Estimate = Estimate,
          SE = Est.Error,
          CI_2.5 = Q2.5,
          CI_97.5 = Q97.5
        ) %>%
        
        mutate(
          Model = name,
          .before = Parameter
        )
      
    }
  )


# affichage
print(table_coefficients)


# export
write.csv(
  table_coefficients,
  "Table_coefficients_Bayes.csv",
  row.names = FALSE
)


# tableau format article

kable(
  table_coefficients,
  digits = 3,
  caption =
    "Posterior estimates of Bayesian model parameters"
)



###############################################################
# 3. TABLEAU PERFORMANCE DES MODELES
###############################################################

table_performance <-
  
  imap_dfr(
    models,
    function(model,name){
      
      
      # LOO
      loo_result <- loo(model)
      
      
      # R2 bayesien
      R2 <- bayes_R2(model)
      
      
      # sigma
      sigma <- posterior_summary(
        model,
        pars="sigma"
      )[1]
      
      
      data.frame(
        
        Model = name,
        
        Bayesian_R2 =
          round(R2[1],3),
        
        R2_low =
          round(R2[3],3),
        
        R2_high =
          round(R2[4],3),
        
        Sigma =
          round(sigma,3),
        
        ELPD_LOO =
          round(
            loo_result$estimates[
              "elpd_loo",
              "Estimate"
            ],
            1
          ),
        
        LOOIC =
          round(
            loo_result$estimates[
              "looic",
              "Estimate"
            ],
            1
          )
        
      )
      
    }
  )


print(table_performance)


write.csv(
  table_performance,
  "Table_performance_Bayes.csv",
  row.names = FALSE
)


kable(
  table_performance,
  digits=3,
  caption =
    "Predictive performance of Bayesian models"
)



###############################################################
# 4. TABLEAU FINAL POUR ARTICLE
###############################################################

table_article <-
  
  table_coefficients %>%
  
  left_join(
    table_performance,
    by="Model"
  )


write.csv(
  table_article,
  "Table_article_models_Bayes.csv",
  row.names = FALSE
)


###############################################################
# 5. VERIFICATION CONVERGENCE
###############################################################

table_Rhat <-
  
  imap_dfr(
    models,
    function(model,name){
      
      summary(model)$fixed %>%
        
        as.data.frame() %>%
        
        rownames_to_column("Parameter") %>%
        
        mutate(
          Model=name
        )
      
    }
  )


print(table_Rhat)



###############################################################
# FIN BLOC 1/2
############################################################### 
###############################################################
# PRESENTATION ARTICLE - MODELES BAYESIENS
# BLOC 2/2 : FIGURES PUBLICATION
###############################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


###############################################################
# 1. FONCTION GENERALE : PREDICTION BAYESIENNE
###############################################################

prediction_dataframe <- function(model,
                                 data,
                                 variable,
                                 n=200){
  
  
  newdata <- data.frame(
    
    seq(
      min(data[[variable]], na.rm=TRUE),
      max(data[[variable]], na.rm=TRUE),
      length.out=n
    )
    
  )
  
  colnames(newdata) <- variable
  
  
  pred <- fitted(
    model,
    newdata=newdata,
    probs=c(0.025,0.975)
  )
  
  
  newdata$Estimate <- pred[,1]
  
  newdata$Low <- pred[,3]
  
  newdata$High <- pred[,4]
  
  
  return(newdata)
  
}



###############################################################
# 2. FONCTION FIGURE MODELE
###############################################################

plot_model <- function(model,
                       data,
                       variable,
                       response,
                       titre){
  
  
  pred <- prediction_dataframe(
    model,
    data,
    variable
  )
  
  
  ggplot()+
    
    geom_point(
      data=data,
      aes_string(
        x=variable,
        y=response
      ),
      alpha=0.45,
      size=2
    )+
    
    
    geom_ribbon(
      data=pred,
      aes_string(
        x=variable,
        ymin="Low",
        ymax="High"
      ),
      alpha=0.25
    )+
    
    
    geom_line(
      data=pred,
      aes_string(
        x=variable,
        y="Estimate"
      ),
      linewidth=1.2
    )+
    
    
    theme_bw(base_size=13)+
    
    labs(
      title=titre,
      x=variable,
      y=response
    )
  
}



###############################################################
# FIGURE 1
# COUTS D'EXPLOITATION
###############################################################

fig_cable_cost <-
  
  plot_model(
    
    mod_tarif_presta_cable,
    
    df_cable,
    
    "PENTE_MOY",
    
    "TARIF_PRESTA_M3",
    
    "(A) Cable harvesting"
    
  )



fig_ground_cost <-
  
  plot_model(
    
    mod_tarif_presta_terre,
    
    df_terre,
    
    "PENTE_MOY",
    
    "TARIF_PRESTA_TERRE_M3",
    
    "(B) Ground-based harvesting"
    
  )



figure_costs <-
  
  fig_cable_cost +
  fig_ground_cost +
  plot_layout(ncol=2)



figure_costs



ggsave(
  
  "Figure_1_Harvesting_costs.png",
  
  figure_costs,
  
  width=12,
  
  height=5,
  
  dpi=300
  
)



###############################################################
# FIGURE 2
# PRODUCTIVITE
###############################################################


fig_cable_prod <-
  
  plot_model(
    
    mod_vitesse_intra_cable,
    
    df_intra_clean,
    
    "G_TOT_CUT",
    
    "VITESSE_MOY_JOUR_M3_INTRA",
    
    "(A) Cable productivity"
    
  )



fig_ground_prod <-
  
  plot_model(
    
    mod_vitesse_intra_terre,
    
    df_intra_terre,
    
    "G_TOT_CUT",
    
    "VITESSE_MOY_JOUR_M3_INTRA",
    
    "(B) Ground-based productivity"
    
  )



figure_productivity <-
  
  fig_cable_prod +
  fig_ground_prod +
  plot_layout(ncol=2)



figure_productivity



ggsave(
  
  "Figure_2_Productivity.png",
  
  figure_productivity,
  
  width=12,
  
  height=5,
  
  dpi=300
  
)



###############################################################
# 3. OBSERVE VS PREDIT
###############################################################


observed_predicted <- function(model,
                               data,
                               response,
                               titre){
  
  
  prediction <- fitted(model)
  
  
  df <- data.frame(
    
    Observed =
      data[[response]],
    
    Predicted =
      prediction[,1]
    
  )
  
  
  
  ggplot(df,
         aes(
           Observed,
           Predicted
         ))+
    
    geom_point(
      alpha=.5,
      size=2
    )+
    
    geom_abline(
      slope=1,
      intercept=0,
      linetype=2
    )+
    
    theme_bw(base_size=13)+
    
    labs(
      title=titre,
      x="Observed",
      y="Predicted"
    )
  
}



obs_prod_cable <-
  
  observed_predicted(
    
    mod_vitesse_intra_cable,
    
    df_intra_clean,
    
    "VITESSE_MOY_JOUR_M3_INTRA",
    
    "Cable productivity"
    
  )



obs_prod_ground <-
  
  observed_predicted(
    
    mod_vitesse_intra_terre,
    
    df_intra_terre,
    
    "VITESSE_MOY_JOUR_M3_INTRA",
    
    "Ground-based productivity"
    
  )



figure_obs_pred <-
  
  obs_prod_cable +
  obs_prod_ground +
  plot_layout(ncol=2)



figure_obs_pred



ggsave(
  
  "Figure_3_Observed_predicted.png",
  
  figure_obs_pred,
  
  width=10,
  
  height=5,
  
  dpi=300
  
)



###############################################################
# 4. POSTERIOR PREDICTIVE CHECKS
###############################################################


pdf(
  "Figure_4_Posterior_predictive_checks.pdf",
  width=7,
  height=5
)


pp_check(
  mod_tarif_presta_cable
)


pp_check(
  mod_tarif_presta_terre
)


pp_check(
  mod_vitesse_intra_cable
)


pp_check(
  mod_vitesse_intra_terre
)


dev.off()



###############################################################
# 5. DISTRIBUTIONS POSTERIEURES
###############################################################

library(bayesplot)


pdf(
  "Figure_5_Posterior_parameters.pdf",
  width=8,
  height=6
)


mcmc_areas(
  as.array(mod_tarif_presta_cable)
)


mcmc_areas(
  as.array(mod_tarif_presta_terre)
)


mcmc_areas(
  as.array(mod_vitesse_intra_cable)
)


mcmc_areas(
  as.array(mod_vitesse_intra_terre)
)


dev.off()



###############################################################
# FIN BLOC 2/2
###############################################################