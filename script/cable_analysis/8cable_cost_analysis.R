# =========================
# SCRIPT N°8 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif d'analyser les variables ayant une influence sur le tarif de prestation du câble aérien, précédemment calculé.
# =========================

# =========================
# 1. PACKAGES
# =========================
install.packages("brms")  
library(brms)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev/")
data_chantiers <- read_csv("results/global/intermediate/df_inter3.csv")
data_chantiers <- data_chantiers |>
  mutate(
    TYPE_ESSENCE = case_when(
      
      # FEUILLUS (prioritaire si mélange)
      str_detect(ESSENCE_MAJ, regex("chêne|chene|charme|hêtre|hetre|frêne|chênes|feuillu|feuillus|chènes", ignore_case = TRUE)) ~ "FEUILLUS",
      
      # RESINEUX
      str_detect(ESSENCE_MAJ, regex("sapin|épicéa|epicéa|epicea|pin|douglas|mélèze|meleze|résineux|resineux", ignore_case = TRUE)) ~ "RESINEUX",
      
      # cas non identifié
      TRUE ~ NA_character_
    )
  )

data_chantiers |>
  group_by(TARIF_PRESTA_M3) |>
  summarise(
    n = n(),
    diam = mean(DIAM_MOY, na.rm = TRUE),
    long = mean(LONGUEUR_MOY, na.rm = TRUE),
    vol = mean(VOL_MOYEN_GRUME, na.rm = TRUE),
    nb_grumes = mean(NB_GRUMES, na.rm = TRUE),
    nb_jours = mean(JOURS_TRAVAILLES_AVEC_MONTAGE, na.rm = TRUE),
    pente = mean(PENTE_MOY, na.rm = TRUE),
    nb_lignes = mean(NB_LIGNES, na.rm = TRUE)
  )

data_chantiers <- data_chantiers |>
  mutate(TARIF_PRESTA_TOT = COUT_FIXES_TOT + COUT_VARIABLES_TOT) |>
  relocate(TARIF_PRESTA_TOT, .after = TARIF_PRESTA_M3)


#write_csv(data_chantiers, "data_chantiers_cost_output")

set.seed(10)  # nombre tiré aléatoirement sur random.org
# Nombre total de lignes
n <- nrow(data_chantiers)
# On enlève 10% des données pour test modèle ensuite
test_idx <- sample(1:n, size = 0.1 * n)
# Création des datasets
data_test <- data_chantiers[test_idx, ]
data_train <- data_chantiers[-test_idx, ]
  
#data_train[59, "COUT_FIXES_TOT"] <- NA #Un 0 a remplacé un NA dans cette ligne, le remettre en NA permet de comparer les modèles

cor(data_chantiers$COUT_FIXES_M3, data_chantiers$VOL_HA_MOY, use = "complete.obs")
# = -O.42
cor(data_chantiers$VOL_REF, data_chantiers$NB_JOURS_CHANTIER, use = "complete.obs")
# = 0.67
cor(data_chantiers$VOL_REF, data_chantiers$JOURS_TRAVAILLES, use = "complete.obs")
# = 0.55
cor(data_chantiers$VOL_REF, data_chantiers$G_TOT_CUT, use = "complete.obs")
# = 0.52
cor(data_chantiers$VOL_REF, data_chantiers$TARIF_PRESTA_M3, use = "complete.obs")
# = -0.18 


# =========================
# 3. ANALYSE AVEC MODELE LINEAIRE
# =========================

trafi_presta_model <- lm(TARIF_PRESTA_M3 ~ # Modèle linéaire du tarif de presta
                           PENTE_MOY +
                           VOL_MOYEN_GRUME +
                           DIAM_MOY + 
                           G_TOT_CUT +
                           JOURS_TRAVAILLES +
                           VOL_HA_MOY,
                         data = data_train)

summary(trafi_presta_model)
# La PENTE_MOY écrase tout : PENTE_MOY         0.582442   0.099344   5.863 6.72e-06 ***
# 2e = VOL_HA_MOY       -0.008412   0.006893  -1.220  0.23525 
# 3e = VOL_MOYEN_GRUME  -4.494812   4.220816  -1.065  0.29846
# Adjusted R-squared:   0.6364  ;  p-value: 4.403e-05

cout_fixes_model <- lm(COUT_FIXES_M3 ~ # Modèle linéaire des couts fixes
                     VOL_MOYEN_GRUME + 
                     DIAM_MOY + 
                     G_TOT_CUT +
                     PENTE_MOY +
                     JOURS_TRAVAILLES +
                     VOL_HA_MOY,
                   data = data_train)

summary(cout_fixes_model)
#VOL_HA_MOY       -0.014210   0.005513  -2.577   0.0160 *
#G_TOT_CUT        -0.008886   0.008332  -1.067   0.2960  
#Adjusted R-squared:  0.2511  ; p-value: 0.03141

cout_variables_model <- lm(COUT_VARIABLES_TOT ~ # Modèle linéaire des couts variables
                     VOL_MOYEN_GRUME +
                     DIAM_MOY + 
                     G_TOT_CUT +
                     PENTE_MOY +
                     JOURS_TRAVAILLES +
                     VOL_HA_MOY,
                   data = data_train)

summary(cout_variables_model)
#G_TOT_CUT          107.528     48.735   2.206    0.036 *
#JOURS_TRAVAILLES   331.263    232.759   1.423    0.166  
#PENTE_MOY          485.602    423.408   1.147    0.261  
#Adjusted R-squared:  0.3532  ; p-value: 0.005392

model_full <- lm(TARIF_PRESTA_M3 ~ 
                   VOL_MOYEN_GRUME + 
                   VOL_HA_MOY +
                   G_TOT_CUT +
                   PENTE_MOY +
                   TYPE_ESSENCE +
                   TYPE_CHANTIER,
                 data = data_train)

summary(model_full)
#VOL_HA_MOY           -0.012774   0.008176  -1.562 0.129445 
#PENTE_MOY             0.521184   0.124640   4.182 0.000258 ***
#Adjusted R-squared:  0.472  ; p-value: 0.000367

plot(data_train$PENTE_MOY,
     data_train$TARIF_PRESTA_M3,
     main = "Relation entre tarif presta et pente moyenne",
     xlab = "pente",
     ylab = "tarif presta",
     pch = 16)
model <- lm(TARIF_PRESTA_M3 ~ PENTE_MOY, data = data_train)
abline(model, col = "red", lwd = 2)
r2 <- summary(model)$r.squared
text(x = max(data_train$PENTE_MOY, na.rm = TRUE),
     y = max(data_train$TARIF_PRESTA_M3, na.rm = TRUE),
     labels = paste0("R² = ", round(r2, 3)),
     pos = 2)  # position à gauche du point



plot(data_train$G_TOT_CUT,
     data_train$COUT_VARIABLES_TOT,
     main = "Relation entre coûts variables et surface terrière totale à débarder",
     xlab = "G",
     ylab = "Coûts variables",
     pch = 16)
model <- lm(COUT_VARIABLES_TOT ~ G_TOT_CUT, data = data_train)
abline(model, col = "red", lwd = 2)
r2 <- summary(model)$r.squared
text(max(data_train$G_TOT_CUT, na.rm = TRUE),
     max(data_train$COUT_VARIABLES_TOT, na.rm = TRUE),
     paste0("R² = ", round(r2, 3)),
     pos = 2)



plot(data_train$JOURS_TRAVAILLES,
     data_train$COUT_FIXES_TOT,
     main = "Relation entre coûts fixes et jours travaillés",
     xlab = "Jours travaillés",
     ylab = "Coûts fixes",
     pch = 16)
model <- lm(COUT_FIXES_TOT ~ JOURS_TRAVAILLES, data = data_train)
abline(model, col = "red", lwd = 2)
r2 <- summary(model)$r.squared
text(max(data_train$JOURS_TRAVAILLES, na.rm = TRUE),
     max(data_train$COUT_FIXES_TOT, na.rm = TRUE),
     paste0("R² = ", round(r2, 3)),
     pos = 2)


plot(data_train$VOL_HA_MOY,
     data_train$COUT_FIXES_TOT,
     main = "Relation entre coûts fixes et volume moyen à l'hectare",
     xlab = "Volume moyen à l'hectare",
     ylab = "Coûts fixes",
     pch = 16)
model <- lm(COUT_FIXES_TOT ~ VOL_HA_MOY, data = data_train)
abline(model, col = "red", lwd = 2)
r2 <- summary(model)$r.squared
text(max(data_train$VOL_HA_MOY, na.rm = TRUE),
     max(data_train$COUT_FIXES_TOT, na.rm = TRUE),
     paste0("R² = ", round(r2, 3)),
     pos = 2)


# =========================
# 4. ANALYSE AVEC MODELE BAYESIEN
# =========================

################
# MODELISATION TARIF DE PRESTATION
################

#Test avec distribution Normale
model_bayes <- brm( TARIF_PRESTA_M3 ~ PENTE_MOY, data = data_train)
summary(model_bayes)
# TARIF_PRESTA_M3 = 36.2 + 0.50 * PENTE_MOY
# 95% de probabilité que l’effet de la pente soit entre 0.31 et 0.69 €/M3 par degré d'augmentation
plot(conditional_effects(model_bayes),
     points = TRUE)
bayes_R2(model_bayes) #R2 0.4170073 0.09583568 0.1971463 0.568263

#Test avec distribution Lognormale
model_bayes_log <- brm(TARIF_PRESTA_M3 ~ PENTE_MOY, data = data_train,
                      family = lognormal())
summary(model_bayes_log)
plot(conditional_effects(model_bayes_log),
     points = TRUE)
bayes_R2(model_bayes_log) #R2 0.3975852 0.1099704 0.1528542 0.5746861

loo(model_bayes, model_bayes_log) # modèle log vs modèle gaussien :	elpd_loo -110.0 vs -112.9	et	looic 220.0	vs 225.7
# elpd_diff = -2.8  (SE = 0.8)
# modèle log meilleur mais je vais faire les deux options


#Je test plusieurs modèles en modifiant les variables selon les résultats :

   model_bayes2 <- brm ( TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY, data = data_train)
   summary(model_bayes2) #VOL_HA_MOY réagi à -0,03
   bayes_R2(model_bayes2) #R2 0.5079143 0.08413356 0.3046303 0.6320668
         model_bayes3 <- brm( TARIF_PRESTA_M3 ~ PENTE_MOY + LONGUEUR_MOY + G_TOT_CUT , data = data_train)
         summary(model_bayes3) # LONGUEUR et G sont également proche de O
         bayes_R2(model_bayes3) #R2 0.5305705 0.08144568 0.3386248 0.646383
               model_bayes4 <- brm( TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_MOYEN_GRUME , data = data_train)
               summary(model_bayes4) # VOL_MOYEN_GRUME comprend 0
               bayes_R2(model_bayes4) #R2 0.4203168 0.09816707 0.1904328 0.5723285
                      model_bayes5 <- brm( TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY + LONGUEUR_MOY , data = data_train)
                      summary(model_bayes5) # VOL_HA_MOY + LONGUEUR_MOY sont compris entre 0 et 0,03
                      bayes_R2(model_bayes5) #R2 0.557692 0.08104163 0.36075 0.6715385


   model_bayes2_log <- brm(TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY, data = data_train,
                                             family = lognormal())
   summary(model_bayes2_log)
   bayes_R2(model_bayes2_log)
         model_bayes3_log <- brm(TARIF_PRESTA_M3 ~ PENTE_MOY + LONGUEUR_MOY + G_TOT_CUT, data = data_train,
                                             family = lognormal())
         summary(model_bayes3_log)
         bayes_R2(model_bayes3_log) #R2 0.515088 0.09972089 0.2857289 0.6656818
                model_bayes4_log <- brm(TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_MOYEN_GRUME, data = data_train,
                                             family = lognormal())
                summary(model_bayes4_log)
                bayes_R2(model_bayes4_log) #R2 0.4016904 0.1071054 0.1576171 0.5718879
                      model_bayes5_log <- brm(TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY + LONGUEUR_MOY, data = data_train,
                                             family = lognormal())
                      summary(model_bayes5_log)
                      bayes_R2(model_bayes5_log) #R2 0.5493389 0.09222734 0.3274567 0.684253
                      
loo(model_bayes5, model_bayes5_log) 
# modèle log vs modèle gaussien :	elpd_loo -106.7 vs -110.7	et	looic 213.5	vs 221.4
# elpd_diff = -4.0 (SE = 0.9)
# modèle log meilleur

# On gardera donc uniquement le modèle log pour tester les effets fixes ensuite

#Je refais les tests précédents avec distribution normale et lognormale sur les tarifs de presta TOTAUX                                            

#Test distribution Normale
model_bayestot <- brm(TARIF_PRESTA_TOT ~ PENTE_MOY, data = data_train)
summary(model_bayestot) #PENTE_MOY comprend 0
bayes_R2(model_bayestot) #R2 0.07711132 0.06803412 0.0002853353 0.2385892

#Test distribution Lognormale
model_bayestot_log <- brm(TARIF_PRESTA_TOT ~ G_TOT_CUT, data = data_train,
                         family = lognormal())
summary(model_bayestot_log)
bayes_R2(model_bayestot_log) #R2 0.1192766 0.1112143 0.0002684102 0.3869182

loo(model_bayestot, model_bayestot_log) # modèle log vs modèle gaussien :	elpd_loo -414.5 vs -417.2	et	looic 829	vs 834.4
# elpd_diff = -2.7  (SE = 2.8)
# aucune diff significative entre les 2 modèles, je tente les 2

# Je test plusieurs modèles en modifiant les variables selon les résultats :

    model_bayes2tot <- brm( TARIF_PRESTA_TOT ~ VOL_HA_MOY , data = data_train)
    summary(model_bayes2tot) #VOL_HA_MOY comprend 0 
    bayes_R2(model_bayes2tot) #R2 0.0346073 0.04318004 3.661142e-05 0.1526092
         model_bayes3tot <- brm( TARIF_PRESTA_TOT ~ LONGUEUR_MOY + G_TOT_CUT , data = data_train)
         summary(model_bayes3tot) # G_TOT_CUT ne pas comprend pas 0 
         bayes_R2(model_bayes3tot) #R2 0.4753457 0.09204642 0.2552408 0.6141373
             model_bayes4tot <- brm( TARIF_PRESTA_TOT ~ G_TOT_CUT + VOL_MOYEN_GRUME , data = data_train)
             summary(model_bayes4tot) # VOL_MOYEN comprend 0
             bayes_R2(model_bayes4tot) # R2 0.4776708 0.09024314 0.2598124 0.6123737
                  model_bayes5tot <- brm( TARIF_PRESTA_TOT ~ G_TOT_CUT, data = data_train)
                  summary(model_bayes5tot) 
                  bayes_R2(model_bayes5tot) #R2 0.5519383 0.07821226 0.3606994 0.6659585
                  
                  
    model_bayes2tot_log <- brm(TARIF_PRESTA_TOT ~ VOL_HA_MOY, data = data_train,
                                            family = lognormal())
    summary(model_bayes2tot_log)
    bayes_R2(model_bayes2tot_log) #R2 0.06078007 0.06608681 0.0001127632 0.2308183
         model_bayes3tot_log <- brm(TARIF_PRESTA_TOT ~ LONGUEUR_MOY + G_TOT_CUT, data = data_train,
                                            family = lognormal())
         summary(model_bayes3tot_log)
         bayes_R2(model_bayes3tot_log) #R2 0.5613045 0.1070523 0.2560217 0.6447858
             model_bayes4tot_log <- brm(TARIF_PRESTA_TOT ~ G_TOT_CUT + VOL_MOYEN_GRUME + ALTITUDE_MOY + HA_DES_TOT, data = data_train,
                                            family = lognormal())
             summary(model_bayes4tot_log)
             bayes_R2(model_bayes4tot_log) #R2 0.5671454 0.1020896 0.2668442 0.646925
                  model_bayes5tot_log <- brm(TARIF_PRESTA_TOT ~ G_TOT_CUT, data = data_train,
                                            family = lognormal())
                  summary(model_bayes5tot_log)
                  bayes_R2(model_bayes5tot_log) #R2 0.5600476  0.110413 0.2342211 0.6450408


loo(model_bayes5tot, model_bayes5tot_log) # modèle log vs modèle gaussien :	elpd_loo -407.5 vs -406.6	et	looic 815.1	vs 813.3
# elpd_diff = -0.9  (SE = 3.1)
# aucune diff significative entre les 2 modèles, gaussien même un peu meilleur     

#On gardera donc uniquement le modèle famille Normale pour tester les effets fixes ensuite

# Tests des modèles coûts/m3 famille lognormale en incluant les effets fixes : 
model_bayes2_log <- brm( TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY + (1 | TYPE_CHANTIER), data = data_train,
                         family = lognormal())
summary(model_bayes2_log)
bayes_R2(model_bayes2_log)


model_bayes3_log <- brm(
  TARIF_PRESTA_M3 ~ PENTE_MOY + LONGUEUR_MOY + G_TOT_CUT + (1 | TYPE_CHANTIER),
  data = data_train,
  family = lognormal()
  )
summary(model_bayes3_log)
bayes_R2(model_bayes3_log)

model_bayes4_log <- brm(
  TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_MOYEN_GRUME + (1 | TYPE_CHANTIER),
  data = data_train,
  family = lognormal()
  )
summary(model_bayes4_log)
bayes_R2(model_bayes4_log)

model_bayes5_log <- brm(
  TARIF_PRESTA_M3 ~ PENTE_MOY + VOL_HA_MOY + LONGUEUR_MOY + (1 | TYPE_CHANTIER),
  data = data_train,
  family = lognormal()
  )
summary(model_bayes5_log) # Volume moyen à l'hectare sont à 0 mais ajoute 11% d'explication du modèle 
bayes_R2(model_bayes5_log) #R2 0.5537595 0.09028792 0.3400227 0.6871074
posterior_summary(model_bayes5_log)

# Le modèle en séparant les jeux de données me donnait ce résultat 
#model_bayes_montagne_log <- brm(TARIF_PRESTA_M3 ~ PENTE_MOY + LONGUEUR_MOY , data = data_montagne, family = lognormal())
#summary(model_bayes_montagne_log) 
#bayes_R2(model_bayes_montagne_log) #R2 0.6770191 0.09369287 0.4179433 0.781028

# Tests des modèles coûts/totaux famille Normale en incluant les effets fixes : 
model_bayes2tot <- brm( 
  TARIF_PRESTA_TOT ~ VOL_HA_MOY + (1 | TYPE_CHANTIER), 
  data = data_train)
summary(model_bayes2tot) #VOL_HA_MOY comprend 0 
bayes_R2(model_bayes2tot) #R2 0.05801328 0.05328436 0.0008796169 0.1956269

model_bayes3tot <- brm( 
  TARIF_PRESTA_TOT ~ LONGUEUR_MOY + G_TOT_CUT + (1 | TYPE_CHANTIER), 
  data = data_train)
summary(model_bayes3tot) # G_TOT_CUT ne pas comprend pas 0 
bayes_R2(model_bayes3tot) #R2 0.4837673 0.08526633 0.2843918 0.6136329

model_bayes4tot <- brm( 
  TARIF_PRESTA_TOT ~ G_TOT_CUT + VOL_MOYEN_GRUME + (1 | TYPE_CHANTIER),
  data = data_train)
summary(model_bayes4tot) # VOL_MOYEN comprend 0
bayes_R2(model_bayes4tot) # R2 0.4878647 0.09076006 0.2658843 0.6198815

model_bayes5tot <- brm( 
  TARIF_PRESTA_TOT ~ G_TOT_CUT + (1 | TYPE_CHANTIER), 
  data = data_train)
summary(model_bayes5tot) 
bayes_R2(model_bayes5tot) # R2 0.4797349 0.09275301 0.2601679 0.6146928

#RESULTATS TARIF DE PRESTATION : 

################
# MODELISATION COUTS FIXES 
################

hist(data_train$COUT_FIXES_TOT,
     main = "Nombre de chantiers selon les coûts fixes",
     xlab = "Coûts fixes",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")

hist(log(data_train$COUT_FIXES_TOT),
     main = "Nombre de chantiers selon log(coûts fixes)",
     xlab = "log(Coûts fixes)",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")

hist(data_train$COUT_FIXES_M3,
     main = "Nombre de chantiers selon les coûts fixes/m3",
     xlab = "Coûts fixes/m3",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")
hist(log(data_train$COUT_FIXES_M3),
     main = "Nombre de chantiers selon log(coûts fixes/m3)",
     xlab = "log(Coûts fixes/m3)",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")


#Tests des modèles en utilisant coûts fixes totaux des chantiers 
model_bayes_fixes <- brm(COUT_FIXES_TOT ~ VOL_REF + NB_LIGNES + G_TOT_CUT + PENTE_MOY, data = data_train)
summary (model_bayes_fixes)
bayes_R2(model_bayes_fixes) #R2 0.6413066 0.05956616 0.4969914 0.7239415
# L’effet NB_JOURS_CHANTIER devient non significatif dès que VOL_REF est introduit, 
# cor = 0.67
# COUT_FIXES_TOT = 4416.82 + 6.63 * VOL_REF + 909.73 * NB_LIGNES − 29.93 * G + 184.63 * PENTE_MOY

model_bayes_fixes_log <- brm(COUT_FIXES_TOT ~ VOL_REF + NB_LIGNES + G_TOT_CUT + PENTE_MOY, data = data_train, family = lognormal())
summary (model_bayes_fixes_log)
bayes_R2(model_bayes_fixes_log) #R2 0.6342705 0.03022065 0.5611635 0.6714254

loo(model_bayes_fixes, model_bayes_fixes_log)

#Tests des modèles en utilisant coûts fixes/m3 
model_bayes_fixes_m3 <- brm(COUT_FIXES_M3 ~ NB_LIGNES + G_TOT_CUT + VOL_MOYEN_GRUME, data = data_train)
summary (model_bayes_fixes_m3)
bayes_R2(model_bayes_fixes_m3)
# Rien n'est significatif lorsque je passe au M3, 0 est compris pour toutes les variables


#Tests des modèles avec effets fixes

model_bayes_fixes <- brm(COUT_FIXES_TOT ~ VOL_REF + NB_LIGNES + G_TOT_CUT + PENTE_MOY + (1 | TYPE_CHANTIER), 
                         data = data_train)
summary (model_bayes_fixes) # COUT_FIXES_TOT = 2522.38 + 7.09 * VOL_REF + 840.05 * NB_LIGNES − 29.49 * G + 271.11 * PENTE_MOY
bayes_R2(model_bayes_fixes) # R2 0.6859679 0.05099455 0.5583462 0.753883


model_bayes_fixes_m3 <- brm(COUT_FIXES_M3 ~ NB_LIGNES + G_TOT_CUT + VOL_MOYEN_GRUME + (1 | TYPE_CHANTIER), data = data_train)
summary (model_bayes_fixes_m3)
bayes_R2(model_bayes_fixes_m3) #R2 0.2040137 0.08664454 0.04643068 0.3733656
# Rien n'est significatif lorsque je passe au M3, 0 est compris pour toutes les variables

model_bayes_fixes_m3_log <- brm(COUT_FIXES_M3 ~ NB_LIGNES + G_TOT_CUT + VOL_MOYEN_GRUME + (1 | TYPE_CHANTIER), data = data_train,  family = lognormal())
summary (model_bayes_fixes_m3_log)
bayes_R2(model_bayes_fixes_m3_log) #R2 R2 0.2249032 0.08643764 0.06458076 0.4009976
# Rien n'est significatif lorsque je passe au M3, 0 est compris pour toutes les variables


################
# MODELISATION COUTS VARIABLES 
################

hist(data_train$COUT_VARIABLES_TOT,
     main = "Nombre de chantiers selon les coûts variables",
     xlab = "Coûts variables",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")

hist(log(data_train$COUT_VARIABLES_TOT),
     main = "Nombre de chantiers selon log(coûts variables)",
     xlab = "log(Coûts variables)",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")

hist(data_train$COUT_VARIABLES_M3,
     main = "Nombre de chantiers selon les coûts variables",
     xlab = "Coûts variables",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")

hist(log(data_train$COUT_VARIABLES_M3),
     main = "Nombre de chantiers selon log(coûts variables/M3)",
     xlab = "log(Coûts variables/M3)",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")


# Test en ajoutant les effets fixes aux modèles :

model_bayes_variables <- brm(COUT_VARIABLES_TOT ~ G_TOT_CUT + NB_GRUMES + (1 | TYPE_CHANTIER), data = data_train)
summary (model_bayes_variables)
bayes_R2(model_bayes_variables) #R2 0.4573341 0.0817096 0.2695486 0.5830667
#COUT_VARIABLES_TOT = 19831.59 + 232.69 * G_TOT_CUT - 9.96 * NB_GRUMES


model_bayes_variables_m3 <- brm(COUT_VARIABLES_M3 ~ PENTE_MOY + VOL_MOYEN_GRUME + (1 | TYPE_CHANTIER), data = data_train) 
summary (model_bayes_variables_m3)
bayes_R2(model_bayes_variables_m3) # R2 0.2128457 0.08720571 0.05006319 0.3740014
#COUT_VARIABLES_M3 = 28.00 + 0.25 * PENTE_MOY - 0.67 * VOL_MOYEN_GRUME
