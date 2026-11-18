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
data_chantiers <- read_csv("results/global/intermediate/6df_inter2.csv")
df_vitesse_complet <- read_csv("results/global/intermediate/6df_vitesse_complet.csv")
access_chantier <- read_csv("results/global/intermediate/access_chantier.csv")

df_vitesse_complet2 <- df_vitesse_complet|>
  mutate(
    VITESSE_MOY_JOUR_M3_INTRA = ifelse(TYPE_VITESSE == "intra_chantier", VITESSE_MOY_JOUR_M3, NA),
    VITESSE_MOY_JOUR_M3_INTER = ifelse(TYPE_VITESSE == "inter_chantier", VITESSE_MOY_JOUR_M3, NA)
  )

data_chantiers <- data_chantiers |>
  left_join(
    access_chantier |>
      select(ID_CHANTIER, ACCORD_CADRE, classe_majoritaire),
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  ) |>
  relocate(
    classe_majoritaire,
    .after = dist_frontiere_est
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

write.csv (data_chantiers, "~/Documents/S10_BETA/scable_dev/results/global/output/data_chantiers_output")


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
df_inter_clean <- df_inter |>
  filter(!is.na(VITESSE_MOY_JOUR_M3_INTER)) |>
  filter(VITESSE_MOY_JOUR_M3_INTER <= 100) # retire les valeurs aberrantes des vitesses pour l'analyse

cols <- ifelse(df_inter$VITESSE_MOY_JOUR_M3_INTER > 100, "red", "darkblue")

plot(df_inter$JOURS_TRAVAILLES_AVEC_MONTAGE,
     df_inter$VITESSE_MOY_JOUR_M3_INTER,
     main = "Relation entre jours travaillés et vitesse inter-chantier",
     xlab = "Nombre de jours travaillés",
     ylab = "Vitesse moyenne journalière (m³/jour)",
     pch = 16,
     col = cols)
legend("topright",
       legend = c("Inférieure au seuil de 100m3/jour", "Supérieure au seuil de 100m3/jour"),
       col = c("darkblue", "red"),
       pch = 16)


#PARTIE 2 : Analyse des valeurs inter 

linear_model_extreme_inter <- lm(VITESSE_MOY_JOUR_M3_INTER ~ # MODELE EXPLICATIF des extrêmes de la vitesse inter
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


model_bayes_speed_inter_log <- brm( VITESSE_MOY_JOUR_M3_INTER ~ PENTE_MOY + VOL_HA_MOY + DIAM_MOY + LONGUEUR_MOY , data = df_inter_clean,
                                    family = lognormal())
summary(model_bayes_speed_inter_log)
bayes_R2(model_bayes_speed_inter_log) #R2 0.3770628 0.09177944 0.1761947 0.5278027
posterior_summary(model_bayes_speed_inter_log)
saveRDS(model_bayes_speed_inter_log, "mod_vitesse_inter_cable.rds")



# =========================
# ANALYSES DES VITESSES INTRA
# =========================


#PARTIE 1 : Analyse générale 

df_intra <- data_train
df_intra <- df_intra |>
  select(-JOURS_MONTAGE,
         -JOURS_TRAVAILLES,
         -JOURS_TRAVAILLES_AVEC_MONTAGE,
         -NB_JOURS_CHANTIER )

df_intra <- df_intra|>
  left_join(
    df_vitesse_complet2|>
      select(ID_CHANTIER, ACCORD_CADRE, TYPE_VITESSE,
             JOURS_TRAVAILLES, VITESSE_MOY_JOUR_M3_INTRA),
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  )

df_intra_clean <- df_intra |>
  filter(!is.na(VITESSE_MOY_JOUR_M3_INTRA)) |>
  filter(VITESSE_MOY_JOUR_M3_INTRA <= 100) # retire les valeurs aberrantes des vitesses pour l'analyse


df_intra_clean <- df_intra_clean |>
  mutate(
    G_CUT_HA = G_TOT_CUT / HA_DES_TOT
  )

summary(df_intra_clean$G_CUT_HA)
hist(df_intra_clean$G_CUT_HA)

plot(df_intra$JOURS_TRAVAILLES,
     df_intra$VITESSE_MOY_JOUR_M3_INTRA,
     xlim = c(0, 65),
     main = "Relation entre jours travaillés et vitesse intra-chantier",
     xlab = "Nombre de jours travaillés",
     ylab = "Vitesse moyenne journalière (m³/jour)",
     pch = 16,)


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




model_bayes_speed_intra <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ G_TOT_CUT, data = df_intra_clean,
                               family = lognormal())
summary(model_bayes_speed_intra) # G_TOT_CUT 
bayes_R2(model_bayes_speed_intra) #R2 0.5913275 0.1061111 0.2856247 0.6667131
posterior_summary(model_bayes_speed_intra)
saveRDS(model_bayes_speed_intra, "mod_vitesse_intra_cable.rds")


#model_bayes_speed_intra_montagne <- brm(VITESSE_MOY_JOUR_M3_INTRA ~ VOL_MOYEN_GRUME + G_TOT_CUT , data = data_intra_montagne,
#                                        family = lognormal())
summary(model_bayes_speed_intra_montagne) #VOL_MOYEN_GRUME     0.23      0.13    -0.02     0.49
bayes_R2(model_bayes_speed_intra_montagne) #R2 0.4403184

#Les effets de VOL_MOYEN_GRUME sont complètements opposés en plaine et en montagne. 
# Plus volume augmente plus la vitesse augmente en montagne, c'est l'inverse en plaine  
# Ce qui explique pourquoi VOL_MOYEN_GRUME est centré sur 0 dans la vitesse intra générale  

summary(df_intra_clean$VITESSE_MOY_JOUR_M3_INTRA)

cor.test(
  data_chantiers$PENTE_MOY,
  data_chantiers$classe_majoritaire,
  method = "spearman"
)

cor.test(
  data_chantiers$LONGUEUR_MOY,
  data_chantiers$classe_majoritaire,
  method = "spearman"
)

cor.test(
  data_chantiers$ALTITUDE_MOY,
  data_chantiers$classe_majoritaire,
  method = "spearman"
)

