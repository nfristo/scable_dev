# =========================
# SCRIPT LAND N°6 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de modéliser les dynamiques de production du débardage terrestre.
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
data_chantiers_terre <- read_csv ("~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_alti_slope_complet.csv")
df_vitesse_terre_complet <- read_csv("results/global/intermediate/terre/df_vitesse_terre_complet.csv")

df_vitesse_terre_complet2 <- df_vitesse_terre_complet|>
  mutate(
    VITESSE_MOY_JOUR_M3_INTRA = ifelse(TYPE_VITESSE == "intra_chantier", VITESSE_MOY_JOUR_M3, NA),
    VITESSE_MOY_JOUR_M3_INTER = ifelse(TYPE_VITESSE == "inter_chantier", VITESSE_MOY_JOUR_M3, NA)
  )


data_chantiers_terre <- data_chantiers_terre |>
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

set.seed(83)  # nombre tiré aléatoirement sur random.org
# Nombre total de lignes
n <- nrow(data_chantiers_terre)
# On enlève 10% des données pour test modèle ensuite
test_idx <- sample(1:n, size = 0.1 * n)
# Création des datasets
data_test <- data_chantiers_terre[test_idx, ]
data_train <- data_chantiers_terre[-test_idx, ]

# =========================
# 3. ANALYSES VITESSE INTER
# =========================

#PARTIE 1 : Analyse générale 

df_inter_terre <- data_train|>
  left_join(
    df_vitesse_terre_complet2|>
      select(ID_FB, VITESSE_MOY_JOUR_M3_INTER, JOURS_TRAVAILLES),
    by = c("ID_FB")
  )

df_inter_terre <- df_inter_terre|>
  filter(!is.na(VITESSE_MOY_JOUR_M3_INTER))

Q1 <- quantile(df_inter_terre$VITESSE_MOY_JOUR_M3_INTER, 0.25, na.rm = TRUE)
Q3 <- quantile(df_inter_terre$VITESSE_MOY_JOUR_M3_INTER, 0.75, na.rm = TRUE)
iqr_val <- Q3 - Q1

borne_inf <- Q1 - 1.5 * iqr_val
borne_sup <- Q3 + 1.5 * iqr_val # Détection des valeurs aberrantes en utilisant la méthode de Tukey

df_inter_terre <- df_inter_terre |>
  mutate(
    extreme_inter = VITESSE_MOY_JOUR_M3_INTER < borne_inf |
      VITESSE_MOY_JOUR_M3_INTER > borne_sup
  ) 

df_inter_terre_clean <- df_inter_terre[df_inter_terre$extreme_inter == FALSE, ] # retire les valeurs aberrantes des vitesses pour l'analyse

df_inter_terre |>
  group_by(extreme_inter) |>
  summarise(
    n = n(),
    diam = mean(DIAM_MOY, na.rm = TRUE),
    vol = mean(VOL_MOYEN_GRUME, na.rm = TRUE),
    nb_grumes = mean(NB_GRUMES, na.rm = TRUE),
    nb_jours = mean(JOURS_TRAVAILLES, na.rm = TRUE),
    pente = mean(PENTE_MOY, na.rm = TRUE)
  )

cols <- ifelse(df_inter_terre$extreme_inter, "red", "darkblue")

plot(df_inter_terre$JOURS_TRAVAILLES,
     df_inter_terre$VITESSE_MOY_JOUR_M3_INTER,
     main = "Relation entre jours travaillés et vitesse inter-chantier",
     xlab = "Nombre de jours travaillés",
     ylab = "Vitesse moyenne journalière (m³/jour)",
     pch = 16,
     col = cols)
legend("topright",
       legend = c("Inférieur au seuil Q3+1,5*IQR", "Supérieure au seuil Q3+1,5*IQR"),
       col = c("darkblue", "red"),
       pch = 16)

#PARTIE 2 : Analyse des valeurs inter 

linear_model_extreme_inter_terre <- lm(extreme_inter ~ # MODELE EXPLICATIF des extrêmes de la vitesse inter
                                   VOL_MOYEN_GRUME + 
                                   G_TOT_CUT +
                                   PENTE_MOY +
                                   DIAM_MOY +
                                   VOL_HA_MOY,
                                 data = df_inter_terre)

summary(linear_model_extreme_inter_terre)

linear_model_inter_terre <- lm(VITESSE_MOY_JOUR_M3_INTER ~ # MODELE EXPLICATIF de la vitesse inter
                           VOL_MOYEN_GRUME +
                           DIAM_MOY +
                           HA_DES_TOT +
                           VOL_HA_MOY,
                         data = df_inter_terre_clean)

summary(linear_model_inter_terre)

model_full_terre <- glm(VITESSE_MOY_JOUR_M3_INTER ~ 
                    VOL_MOYEN_GRUME + 
                    DIAM_MOY +
                    NB_GRUMES +
                    VOL_HA_MOY,
                  data = df_inter_clean)

summary(model_full_terre)


table(df_inter_terre$extreme_inter, df_inter_terre$TYPE_CHANTIER)

# =========================
# ANALYSE AVEC MODELE BAYESIEN
# =========================

hist(df_inter_terre_clean$VITESSE_MOY_JOUR_M3_INTER)
hist(log(df_inter_terre_clean$VITESSE_MOY_JOUR_M3_INTER))

model_bayes_speed_inter_terre_log <- brm( VITESSE_MOY_JOUR_M3_INTER ~ VOL_MOYEN_GRUME + DIAM_MOY + NB_GRUMES + G_TOT_CUT, data = df_inter_terre,
                                    family = lognormal())
summary(model_bayes_speed_inter_terre_log)
bayes_R2(model_bayes_speed_inter_terre_log) # R2 0.2889337  0.115606 0.07696737 0.474547


model_bayes_speed_inter_terre_std <- brm( VITESSE_MOY_JOUR_M3_INTER ~ VOL_MOYEN_GRUME + DIAM_MOY + NB_GRUMES + G_TOT_CUT , data = df_inter_terre,
                                    family = student())
summary(model_bayes_speed_inter_terre_std)
bayes_R2(model_bayes_speed_inter_terre_std) #R2 0.01010665 0.003514984 0.004206981 0.0180713


loo_log <- loo(model_bayes_speed_inter_terre_log, moment_match = TRUE)
loo_std <- loo(model_bayes_speed_inter_terre_std, moment_match = TRUE)

loo_compare(loo_log, loo_std)


model_bayes_speed_inter_terre <- brm( VITESSE_MOY_JOUR_M3_INTER ~ VOL_MOYEN_GRUME + DIAM_MOY + NB_GRUMES + G_TOT_CUT , data = df_inter_terre_clean,
                                    family = lognormal())
summary(model_bayes_speed_inter_terre)
bayes_R2(model_bayes_speed_inter_terre) #R2 0.3608339 0.08631532 0.1750129 0.4822922
posterior_summary(model_bayes_speed_inter_terre)

# =========================
# ANALYSES DES VITESSES INTRA
# =========================


#PARTIE 1 : Analyse générale 


df_intra_terre <- data_train |>
  left_join(
    df_vitesse_terre_complet2|>
      select(ID_FB, TYPE_VITESSE,
             JOURS_TRAVAILLES, VITESSE_MOY_JOUR_M3_INTRA),
    by = c("ID_FB")
  )

df_intra_terre <- df_intra_terre |>
  filter(!is.na(VITESSE_MOY_JOUR_M3_INTRA))

Q1 <- quantile(df_intra_terre$VITESSE_MOY_JOUR_M3_INTRA, 0.25, na.rm = TRUE)
Q3 <- quantile(df_intra_terre$VITESSE_MOY_JOUR_M3_INTRA, 0.75, na.rm = TRUE)
iqr_val <- Q3 - Q1

borne_inf <- Q1 - 1.5 * iqr_val
borne_sup <- Q3 + 1.5 * iqr_val # Détection des valeurs aberrantes en utilisant la méthode de Tukey

df_intra_terre <- df_intra_terre |>
  mutate(
    extreme_intra = VITESSE_MOY_JOUR_M3_INTRA < borne_inf |
      VITESSE_MOY_JOUR_M3_INTRA > borne_sup
  ) 

df_intra_terre_clean <- df_intra_terre[ df_intra_terre$extreme_intra == FALSE &
                                          df_intra_terre$VITESSE_MOY_JOUR_M3_INTRA >= 1,] # retire les valeurs aberrantes des vitesses pour l'analyse

df_intra_terre |>
  group_by(extreme_intra) |>
  summarise(
    n = n(),
    diam = mean(DIAM_MOY, na.rm = TRUE),
    vol = mean(VOL_MOYEN_GRUME, na.rm = TRUE),
    nb_grumes = mean(NB_GRUMES, na.rm = TRUE),
    nb_jours = mean(JOURS_TRAVAILLES, na.rm = TRUE)
  )

cols <- ifelse(df_intra_terre$extreme_intra, "red", "darkblue")

plot(df_intra_terre$JOURS_TRAVAILLES,
     df_intra_terre$VITESSE_MOY_JOUR_M3_INTRA,
     xlim = c(0, 65),
     main = "Relation entre jours travaillés et vitesse intra-chantier",
     xlab = "Nombre de jours travaillés",
     ylab = "Vitesse moyenne journalière (m³/jour)",
     pch = 16,
     col = cols)

legend("topright",
       legend = c("Inférieur au seuil Q3+1,5*IQR", "Supérieure au seuil Q3+1,5*IQR"),
       col = c("darkblue", "red"),
       pch = 16)


#PARTIE 2 : Analyse des valeurs intra 

hist(df_intra_terre_clean$VITESSE_MOY_JOUR_M3_INTRA)
hist(log(df_intra_terre_clean$VITESSE_MOY_JOUR_M3_INTRA))

# MODELE EXPLICATIF de la vitesse intra
model_intra_terre <- lm(VITESSE_MOY_JOUR_M3_INTRA ~ 
                    VOL_MOYEN_GRUME +
                    VOL_HA_MOY +
                    G_TOT_CUT + 
                    DIAM_MOY +
                    PENTE_MOY + 
                    PEUPLEMENT_STR + 
                    TYPE_ESSENCE +
                    TYPE_CHANTIER,
                  data = df_intra_terre_clean)

summary(model_intra_terre) 


# =========================
# ANALYSE AVEC MODELE BAYESIEN
# =========================

model_bayes_speed_intra_terre <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT + TYPE_ESSENCE, data = df_intra_terre_clean,
                               family = lognormal())
summary(model_bayes_speed_intra_terre)  
bayes_R2(model_bayes_speed_intra_terre) #R2 0.458887 0.05504281 0.3099366 0.5138549

model_bayes_speed_intra_std <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT , data = df_intra_terre, #avec student le test est fait sans retirer les valeurs aberantes 
                                   family = student())
summary(model_bayes_speed_intra_std) # G_TOT_CUT 
bayes_R2(model_bayes_speed_intra_std) #R2 0.04138766 0.007743561 0.0247633 0.0547649



model_bayes_speed_intra_terre <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT + TYPE_ESSENCE + (1 | TYPE_CHANTIER), data = df_intra_terre_clean,
                               family = lognormal())
summary(model_bayes_speed_intra_terre) 
bayes_R2(model_bayes_speed_intra_terre) #R2 0.4602678 0.0564769 0.3110867 0.5146388

#Attention comme pour les chantiers câble, l'ajout de (1 | TYPE_CHANTIER), destabilise le modèle 
#Le retrait complet de TYPE_CHANTIER rend le modèle plus robuste 

