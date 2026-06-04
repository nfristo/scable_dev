# =========================
# SCRIPT LAND N°4 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de modéliser les tarifs de prestation du débardage terrestre.
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
setwd("~/Documents/S10_BETA/scable_dev")
commandes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP_terrestres.csv")
data_chantier_terre <- read_csv ("~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_alti_slope_complet.csv")
grumes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/GRUMES_terrestres.csv")
contour_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO_terrestres.csv")
tige_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/TIGES_terrestres.csv")


data_chantier_terre <- data_chantier_terre |>
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


mean (data_chantier_terre$TARIF_PRESTA_TERRE_M3, na.rm = TRUE)
summary(data_chantier_terre$TARIF_PRESTA_TERRE_M3, na.rm = TRUE)
data_chantier_terre <- data_chantier_terre|>
  filter(!is.na(TARIF_PRESTA_TERRE_M3))

Q1 <- quantile(data_chantier_terre$TARIF_PRESTA_TERRE_M3, 0.25, na.rm = TRUE)
Q3 <- quantile(data_chantier_terre$TARIF_PRESTA_TERRE_M3, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
seuil_supp <- Q3 + 1.5 * IQR # Détection des valeurs aberrantes en utilisant la méthode de Tukey
seuil_inf<- Q1 - 1.5 * IQR

data_chantier_terre <- data_chantier_terre |>
  mutate(extremes = TARIF_PRESTA_TERRE_M3 > seuil_supp | TARIF_PRESTA_TERRE_M3 < seuil_inf)

cols <- ifelse(data_chantier_terre$extremes, "red", "darkblue")

plot(data_chantier_terre$PENTE_MOY,
     data_chantier_terre$TARIF_PRESTA_TERRE_M3,
     main = "Relation entre pente et tarif de presta terre",
     xlab = "Pente",
     ylab = "Tarif de presta (€/m³)",
     pch = 16,
     col = cols)
legend("topright",
       legend = c("Inférieur au seuil Q3+1,5*IQR", "Supérieure au seuil Q3+1,5*IQR"),
       col = c("darkblue", "red"),
       pch = 16)


plot(data_chantier_terre$VOL_REF,
     data_chantier_terre$TARIF_PRESTA_TERRE_M3,
     main = "Relation entre volume et tarif de presta terre",
     xlab = "Volume",
     ylab = "Tarif de presta (€/m³)",
     pch = 16,
     col = cols)
legend("topright",
       legend = c("Inférieur au seuil Q3+1,5*IQR", "Supérieure au seuil Q3+1,5*IQR"),
       col = c("darkblue", "red"),
       pch = 16)

set.seed(17)  # nombre tiré aléatoirement sur random.org
# Nombre total de lignes
n <- nrow(data_chantier_terre)
# On enlève 10% des données pour test modèle ensuite
test_idx <- sample(1:n, size = 0.1 * n)
# Création des datasets
data_test_terre <- data_chantier_terre[test_idx, ]
data_train_terre <- data_chantier_terre[-test_idx, ]

#data_train_terre[686, "PRIX_VENTE_GRUME_TERRE"] <- NA #Un Inf est présent dans cette ligne, le remettre en NA permet de comparer les modèles

cor(data_train_terre$TARIF_PRESTA_TERRE_M3, data_train_terre$PRIX_VENTE_GRUME_TERRE, use = "complete.obs")
# = 0.174
cor(data_train_terre$TARIF_PRESTA_TERRE_M3, data_train_terre$PRIX_VENTE_TOT, use = "complete.obs")
# = 0.106
cor(data_train_terre$G_TOT_CUT, data_train_terre$PRIX_VENTE_TOT, use = "complete.obs")
# = 0.60

data_train_terre_clean <- data_train_terre[data_train_terre$extremes == FALSE, ] # retire les valeurs aberrantes des vitesses pour l'analyse


# =========================
# 3. ANALYSE AVEC MODELE LINEAIRE
# =========================

tarif_presta_terre_model <- lm(TARIF_PRESTA_TERRE_M3 ~ # Modèle linéaire du tarif de presta
                           PENTE_MOY +
                           VOL_MOYEN_GRUME +
                           DIAM_MOY + 
                           G_TOT_CUT +
                           PRIX_VENTE_GRUME_TERRE +
                           VOL_HA_MOY,
                         data = data_train_terre)

summary(tarif_presta_terre_model)
#PRIX_VENTE_GRUME_TERRE  0.003758   0.001172   3.207   0.0014 **
#PENTE_MOY               0.099016   0.043756   2.263   0.0240 *  

model_full <- lm(TARIF_PRESTA_TERRE_M3 ~ 
                   VOL_MOYEN_GRUME + 
                   VOL_HA_MOY +
                   G_TOT_CUT +
                   PENTE_MOY +
                   TYPE_ESSENCE +
                   PRIX_VENTE_GRUME_TERRE +
                   TYPE_CHANTIER,
                 data = data_train_terre)

summary(model_full)
#PENTE_MOY              -0.135946   0.050207  -2.708  0.00695 ** 
#TYPE_ESSENCERESINEUX   -4.024918   0.394033 -10.215  < 2e-16 ***
#PRIX_VENTE_GRUME_TERRE  0.002550   0.001028   2.482  0.01331 *  
#TYPE_CHANTIERPlaine    -6.968540   0.896210  -7.776 2.82e-14 ***



# =========================
# 4. ANALYSE AVEC MODELE BAYESIEN
# =========================

################
# MODELISATION TARIF DE PRESTATION
################

hist(data_train_terre$TARIF_PRESTA_TERRE_M3,
     main = "Nombre de chantiers selon le tarif de prestation",
     xlab = "Tarif de prestation",
     ylab = "Nombre de chantiers",
     col = "lightblue",
     border = "black")


#Test avec distribution Normale
model_bayes_terre <- brm( TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE, data = data_train_terre)
summary(model_bayes_terre)
# TARIF_PRESTA_M3 = 20.36 - 3.16  * TYPE_ESSENCERESINEUX
plot(conditional_effects(model_bayes_terre),
     points = TRUE)
bayes_R2(model_bayes_terre) #R2 0.1085662 0.02069875 0.07095391 0.1506269

#Test avec distribution Lognormale
model_bayes_terre_student <- brm(TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE, data = data_train_terre,
                       family = student())
summary(model_bayes_terre_student)
plot(conditional_effects(model_bayes_terre_student),
     points = TRUE)
bayes_R2(model_bayes_terre_student) #R2 0.1443964 0.01478986 0.1150741 0.1730576

loo(model_bayes_terre, model_bayes_terre_log) 
# modèle gaussien meilleur 

model_bayes_terre2 <- brm( TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE + PRIX_VENTE_GRUME_TERRE , data = data_train_terre)
summary(model_bayes_terre2) #PRIX_VENTE_GRUME_TERRE vaut 0 
bayes_R2(model_bayes_terre2) #R2 0.1197586 0.02155049 0.07807092 0.1622105
          model_bayes_terre3 <- brm( TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE + PENTE_MOY , data = data_train_terre)
          summary(model_bayes_terre3)  
          bayes_R2(model_bayes_terre3) #R2 0.1171082 0.02136041 0.07732406 0.1600046
                      model_bayes_terre4 <- brm( TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE + PENTE_MOY + TYPE_CHANTIER , data = data_train_terre)
                      summary(model_bayes_terre4) 
                      bayes_R2(model_bayes_terre4) # R2 0.1883903 0.02415422 0.1402621 0.2358816
                                 model_bayes_terre5 <- brm( TARIF_PRESTA_TERRE_M3 ~ TYPE_ESSENCE + PENTE_MOY + TYPE_CHANTIER + DIAM_MOY, data = data_train_terre)
                                 summary(model_bayes_terre5) 
                                 bayes_R2(model_bayes_terre5) #R2 0.1910659 0.02370924 0.1451843 0.2369915
                      



