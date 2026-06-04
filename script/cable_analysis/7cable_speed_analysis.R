# =========================
# SCRIPT N°7 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif d'analyser les variables ayant une influence sur la vitesse du débardage par câble aérien précédemment calculée.
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
df_vitesse_complet <- read_csv("results/global/intermediate/df_vitesse_complet.csv")

df_vitesse_complet2 <- df_vitesse_complet|>
  mutate(
    VITESSE_MOY_JOUR_M3_INTRA = ifelse(TYPE_VITESSE == "intra_chantier", VITESSE_MOY_JOUR_M3, NA),
    VITESSE_MOY_JOUR_M3_INTER = ifelse(TYPE_VITESSE == "inter_chantier", VITESSE_MOY_JOUR_M3, NA)
  )


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


cor(df_inter$VOL_MOYEN_GRUME, df_inter$G_TOT_CUT)
# = 0.96
cor(df_inter$VOL_MOYEN_GRUME, df_inter$NB_GRUMES)
# = -0.65
cor(df_inter$DIAM_MOY, df_inter$G_MOY_GRUME)

set.seed(53)  # nombre tiré aléatoirement sur random.org
# Nombre total de lignes
n <- nrow(data_chantiers)
# On enlève 10% des données pour test modèle ensuite
test_idx <- sample(1:n, size = 0.1 * n)
# Création des datasets
data_test <- data_chantiers[test_idx, ]
data_train <- data_chantiers[-test_idx, ]


# =========================
# 3. ANALYSES VITESSE INTER
# =========================

#PARTIE 1 : Analyse générale 

df_inter <- data_train|>
  left_join(
    df_vitesse_complet2|>
      select(ID_CHANTIER, ACCORD_CADRE,
            VITESSE_MOY_JOUR_M3_INTER),
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  )
df_inter <- df_inter|>
  filter(!is.na(VITESSE_MOY_JOUR_M3_INTER))

Q1 <- quantile(df_inter$VITESSE_MOY_JOUR_M3_INTER, 0.25, na.rm = TRUE)
Q3 <- quantile(df_inter$VITESSE_MOY_JOUR_M3_INTER, 0.75, na.rm = TRUE)
iqr_val <- Q3 - Q1

borne_inf <- Q1 - 1.5 * iqr_val
borne_sup <- Q3 + 1.5 * iqr_val # Détection des valeurs aberrantes en utilisant la méthode de Tukey

df_inter <- df_inter |>
  mutate(
    extreme_inter = VITESSE_MOY_JOUR_M3_INTER < borne_inf |
      VITESSE_MOY_JOUR_M3_INTER > borne_sup
  ) 

df_inter_clean <- df_inter[df_inter$extreme_inter == FALSE, ] # retire les valeurs aberrantes des vitesses pour l'analyse

df_inter |>
  group_by(extreme_inter) |>
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

cols <- ifelse(df_inter$extreme_inter, "red", "darkblue")

plot(df_inter$JOURS_TRAVAILLES_AVEC_MONTAGE,
     df_inter$VITESSE_MOY_JOUR_M3_INTER,
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

linear_model_extreme_inter <- lm(extreme_inter ~ # MODELE EXPLICATIF des extrêmes de la vitesse inter
                           VOL_MOYEN_GRUME + 
                             G_TOT_CUT +
                           PENTE_MOY +
                           DIAM_MOY +
                           VOL_HA_MOY,
                         data = df_inter)

summary(linear_model_extreme_inter)

linear_model_inter <- lm(VITESSE_MOY_JOUR_M3_INTER ~ # MODELE EXPLICATIF  de la vitesse inter
                           VOL_MOYEN_GRUME +
                           DIAM_MOY +
                           HA_DES_TOT +
                           VOL_HA_MOY +
                           NB_LIGNES,
                         data = df_inter_clean)

summary(linear_model_inter)

model_full <- glm(VITESSE_MOY_JOUR_M3_INTER ~ 
                            VOL_MOYEN_GRUME + 
                            NB_GRUMES +
                            VOL_HA_MOY +
                            PEUPLEMENT_STR,
                          data = df_inter_clean)

summary(model_full)


table(df_inter$extreme_inter, df_inter$TYPE_CHANTIER)


# =========================
# ANALYSE AVEC MODELE BAYESIEN
# =========================

hist(df_inter_clean$VITESSE_MOY_JOUR_M3_INTER)
hist(log(df_inter_clean$VITESSE_MOY_JOUR_M3_INTER))

model_bayes_speed_inter_log <- brm( VITESSE_MOY_JOUR_M3_INTER ~ VOL_MOYEN_GRUME + TYPE_CHANTIER, data = df_inter,
           family = lognormal())
summary(model_bayes_speed_inter_log)
bayes_R2(model_bayes_speed_inter_log) # R2 0.07249708 0.06089178 0.003445147 0.2278749


model_bayes_speed_inter_std <- brm( VITESSE_MOY_JOUR_M3_INTER ~ VOL_MOYEN_GRUME + TYPE_CHANTIER, data = df_inter,
                                family = student())
summary(model_bayes_speed_inter_std)
bayes_R2(model_bayes_speed_inter_std) #R2 0.02606459 0.01705888 0.002474732 0.0675184


loo_log <- loo(model_bayes_speed_inter_log, moment_match = TRUE)
loo_std <- loo(model_bayes_speed_inter_std, moment_match = TRUE)

loo_compare(loo_log, loo_std)


model_bayes_speed_inter_log <- brm( VITESSE_MOY_JOUR_M3_INTER ~ DIAM_MOY + LONGUEUR_MOY + TYPE_CHANTIER, data = df_inter_clean,
                                    family = lognormal())
summary(model_bayes_speed_inter_log)
bayes_R2(model_bayes_speed_inter_log) #R2 0.3856108 0.08850664 0.1897023 0.5261359
posterior_summary(model_bayes_speed_inter_log)

# =========================
# ANALYSES DES VITESSES INTRA
# =========================


#PARTIE 1 : Analyse générale 

df_intra <- data_train
df_intra <- df_intra |>
  select(-JOURS_MONTAGE,
         -JOURS_TRAVAILLES,
         -JOURS_TRAVAILLES_AVEC_MONTAGE,
         -NB_PERIODES,
         -NB_JOURS_CHANTIER )

df_intra <- df_intra|>
  left_join(
    df_vitesse_complet2|>
      select(ID_CHANTIER, ACCORD_CADRE, TYPE_VITESSE,
             JOURS_TRAVAILLES, VITESSE_MOY_JOUR_M3_INTRA),
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  )

df_intra <- df_intra |>
  filter(!is.na(VITESSE_MOY_JOUR_M3_INTRA))

Q1 <- quantile(df_intra$VITESSE_MOY_JOUR_M3_INTRA, 0.25, na.rm = TRUE)
Q3 <- quantile(df_intra$VITESSE_MOY_JOUR_M3_INTRA, 0.75, na.rm = TRUE)
iqr_val <- Q3 - Q1

borne_inf <- Q1 - 1.5 * iqr_val
borne_sup <- Q3 + 1.5 * iqr_val # Détection des valeurs aberrantes en utilisant la méthode de Tukey

df_intra <- df_intra |>
  mutate(
    extreme_intra = VITESSE_MOY_JOUR_M3_INTRA < borne_inf |
      VITESSE_MOY_JOUR_M3_INTRA > borne_sup
  ) 

df_intra_clean <- df_intra[ df_intra$extreme_intra == FALSE &
                              df_intra$VITESSE_MOY_JOUR_M3_INTRA >= 1,] # retire les valeurs aberrantes des vitesses pour l'analyse

df_intra |>
  group_by(extreme_intra) |>
  summarise(
    n = n(),
    diam = mean(DIAM_MOY, na.rm = TRUE),
    long = mean(LONGUEUR_MOY, na.rm = TRUE),
    vol = mean(VOL_MOYEN_GRUME, na.rm = TRUE),
    nb_grumes = mean(NB_GRUMES, na.rm = TRUE),
    nb_jours = mean(JOURS_TRAVAILLES, na.rm = TRUE),
    nb_lignes = mean(NB_LIGNES, na.rm = TRUE)
  )

cols <- ifelse(df_intra$extreme_intra, "red", "darkblue")

plot(df_intra$JOURS_TRAVAILLES,
     df_intra$VITESSE_MOY_JOUR_M3_INTRA,
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

hist(df_intra_clean$VITESSE_MOY_JOUR_M3_INTRA)
hist(log(df_intra_clean$VITESSE_MOY_JOUR_M3_INTRA))

# MODELE EXPLICATIF des extrêmes de la vitesse intra
model_intra <- lm(VITESSE_MOY_JOUR_M3_INTRA ~ 
                    VOL_MOYEN_GRUME +
                    VOL_HA_MOY +
                    LONGUEUR_MOY +
                    NB_LIGNES +
                    PEUPLEMENT_STR + 
                    TYPE_ESSENCE +
                    TYPE_CHANTIER,
                  data = df_intra_clean)

summary(model_intra) 


# =========================
# ANALYSE AVEC MODELE BAYESIEN
# =========================

model_bayes_speed_intra <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT , data = df_intra_clean,
                               family = lognormal())
summary(model_bayes_speed_intra) # G_TOT_CUT 
bayes_R2(model_bayes_speed_intra) #R2 0.324081  0.128661 0.07525297 0.5518789

model_bayes_speed_intra_std <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT , data = df_intra,
                               family = student())
summary(model_bayes_speed_intra_std) # G_TOT_CUT 
bayes_R2(model_bayes_speed_intra_std) #R2 0.02081887 0.00718499 0.007871395 0.03584645




model_bayes_speed_intra <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT + (1 | TYPE_CHANTIER), data = df_intra_clean,
                               family = lognormal())
summary(model_bayes_speed_intra) # G_TOT_CUT 
bayes_R2(model_bayes_speed_intra) #R2 0.3351783 0.1246979 0.09124961 0.5538511



#model_bayes_speed_intra_montagne <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ VOL_MOYEN_GRUME + G_TOT_CUT , data = data_intra_montagne,
#                                        family = lognormal())
summary(model_bayes_speed_intra_montagne) #VOL_MOYEN_GRUME     0.23      0.13    -0.02     0.49
bayes_R2(model_bayes_speed_intra_montagne) #R2 0.4403184

#Les effets de VOL_MOYEN_GRUME sont complètements opposés en plaine et en montagne. 
# Plus volume augmente plus la vitesse augmente en montagne, c'est l'inverse en plaine  
# Ce qui explique pourquoi VOL_MOYEN_GRUME est centré sur 0 dans la vitesse intra générale  





