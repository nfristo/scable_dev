# =========================
# SCRIPT N°6 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer la vitesse du débardage par câble aérien selon plusieurs méthodes différentes : 
# Annuelle par ETF, intra-chantier grâce aux dates de réception et inter-chantier grâce aux dates DEBUT et FIN du tableur commandes 
# =========================

# =========================
# 1. PACKAGES
# =========================
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CHANTIERS.csv") |>
  filter(ETAT_CHANTIER %in% c("Terminé", "En cours")) #On prend en compte uniquement les chantiers terminés ou en cours
ID_ok <- chantiers$ID_CHANTIER
df_inter3 <- read_csv("results/global/intermediate/5cable_df_inter_alti_slope.csv")
commandes <- read_tsv("~/Documents/S10_BETA/scable_dev/resources/inhouse/Commandes_corr.tsv",
                      col_types = cols(.default = col_character()),
)

# =========================
# 3. CALCUL DU NOMBRE DE JOURS TRAVAILLES PAR CHANTIER
# =========================

commandes <- commandes|> 
  mutate(
    QUANTITE_RECEPTION = parse_number(
      QUANTITE_RECEPTION,
      locale = locale(decimal_mark = ",", grouping_mark = " ")
    )
  )

commandes2 <- commandes|>
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION, format = "%d/%m/%Y"), 
  ) |>
  arrange(ID_CHANTIER, ACCORD_CADRE, DATE_RECEPTION) |>
  
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  
  mutate(
    ecart_jours = as.numeric(DATE_RECEPTION - lag(DATE_RECEPTION)) #calcul du nombre de jours entre chaque réception
  ) |>
  
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  
  summarise(
    DATE_DEBUT = min(DATE_RECEPTION), #On définit les dates de chaque période pour chaque chantier
    DATE_FIN   = max(DATE_RECEPTION),
    
    .groups = "drop"
  )

commandes3 <- commandes2 |>
  mutate(
    DUREE_JOURS = as.numeric(DATE_FIN - DATE_DEBUT) + 1 # On calcule la durée calendaire de chaque période
  ) |>
  
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  
  summarise(
    NB_JOURS_CHANTIER = sum(DUREE_JOURS, na.rm = TRUE), #On additione le nombre de jours de chaque période pour obtenir une durée de chantier en jours
    .groups = "drop"
  )

df_inter3 <- df_inter3 |>
  left_join(commandes3, by = c("ID_CHANTIER", "ACCORD_CADRE"))


jours_chantier <- df_inter3 |>
  group_by(ID_CHANTIER, ACCORD_CADRE, NUM_ETF) |>
  summarise(
    NB_JOURS_CHANTIER = sum(NB_JOURS_CHANTIER, na.rm = TRUE),
    NB_LIGNES_TOTAL = sum(NB_LIGNES, na.rm = TRUE),
    LONGUEUR_MOY = max(LONGUEUR_MOY, na.rm = TRUE),
    type_chantier = first(TYPE_CHANTIER)
  ) |>
  mutate(
    NB_TRANCHES_200 = floor(pmax(0, LONGUEUR_MOY - 200) / 200), # Création des tranches de pose de supports en plaine (1 par 200m)

    JOURS_MONTAGE = ifelse(  # Correction des jours travaillés par les jours de montage
      type_chantier == "plaine",
      NB_LIGNES_TOTAL * (2 + (0.25 * NB_TRANCHES_200)), # Modalité du calcul pour les chantiers Plaine
      -2 * NB_LIGNES_TOTAL # Modalité du calcul pour les chantiers Montagne
    ),

    JOURS_TRAVAILLES_AVEC_MONTAGE = NB_JOURS_CHANTIER * 5/7, #Jours travaillés sans correction montage
    JOURS_TRAVAILLES = (JOURS_TRAVAILLES_AVEC_MONTAGE + JOURS_MONTAGE), #Jours travaillés avec correction montage
    
    JOURS_TRAVAILLES_AVEC_MONTAGE = if_else(
      JOURS_TRAVAILLES_AVEC_MONTAGE < 3,
      NA_real_,
      JOURS_TRAVAILLES_AVEC_MONTAGE
    ),
    
    JOURS_TRAVAILLES = if_else(
      JOURS_TRAVAILLES < 3,
      NA_real_,
      JOURS_TRAVAILLES
    )
    )

df_inter3 <- df_inter3 |>
  st_drop_geometry() |> # On mets la géométrie des données à la même frome que df pour faire la jointure
  left_join(
    jours_chantier |> st_drop_geometry() |> select(ID_CHANTIER, ACCORD_CADRE, JOURS_MONTAGE, JOURS_TRAVAILLES, JOURS_TRAVAILLES_AVEC_MONTAGE), #on ajoute jours travaillés au df par ID chantier
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  )

df_inter3 <- df_inter3 |>
  select(-...1) |>
  relocate(TARIF_PRESTA_TOT, .after = TARIF_PRESTA_M3)

write_csv(df_inter3, "~/Documents/S10_BETA/scable_dev/results/global/intermediate/6df_inter2.csv")

ggplot(df_inter3, aes(x = NB_JOURS_CHANTIER)) +
  geom_density(na.rm = TRUE) +
  scale_x_log10() +
  theme_minimal() # Permet de visualiser la répartition des nombres de jours de chantier 

ggplot(df_inter3, aes(x = NB_JOURS_CHANTIER)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  labs(
    x = "Nombre de jours de chantier",
    y = "Nombre de chantiers"
  ) +
  theme_minimal()

ggplot(df_inter3, aes(x = NB_JOURS_CHANTIER)) +
  geom_histogram(bins = 30) +
  theme_minimal()


# Le calcul suivant permet de calculer tous les écarts entre deux dates de réception 
ecarts <- commandes |>
  mutate(DATE_RECEPTION = as.Date(DATE_RECEPTION, "%d/%m/%Y")) |>
  arrange(ID_CHANTIER, ACCORD_CADRE, DATE_RECEPTION) |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  mutate(ecart_jours = as.numeric(DATE_RECEPTION - lag(DATE_RECEPTION))) |>
  ungroup() |>
  filter(!is.na(ecart_jours) & ecart_jours > 0)

# Les graphes suivants permettetn de visualiser la répartition de tous les écarts entre 2 dates de récpetion
ggplot(ecarts, aes(ecart_jours)) +
  stat_ecdf() +
  geom_vline(xintercept = 365, linetype = "dashed", color = "red") +
  theme_minimal()

ggplot(ecarts, aes(x = ecart_jours)) +
  geom_histogram(bins = 30) +
  theme_minimal()

ggplot(ecarts, aes(ecart_jours)) +
  geom_density() +
  scale_x_log10() +
  theme_minimal()

summary(ecarts$ecart_jours)

#=========================
# 4. ESTIMATION DE LA VITESSE DE DÉBARDAGE (m3/jour effectif) : 3 méthodes différentes
# =========================

####
# 1ère estimation en utilisant les cinétiques de débardage intra-chantier grâce aux dates de réception : 
####

df_vitesse_intra <- commandes |> # On garde uniquement les colonnes qui nous sont utiles
  select(ID_CHANTIER, ACCORD_CADRE, NUM_ETF, TYPE_OP,DATE_RECEPTION,QUANTITE_RECEPTION)

df_vitesse_intra <- df_vitesse_intra |>
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION, format = "%d/%m/%Y"),
  ) |>
  filter(TYPE_OP %in% c("CVar_CABLE_DEBARDAGE", "CVar_CABLE_DEB-ABA", "CVar_CABLE_ABATTAGE")) |> #== "CVar_CABLE_DEBARDAGE") |> # OU %in% c("CVar_CABLE_DEBARDAGE", "CVar_CABLE_DEB-ABA", "CFixe_CABLE_MONTAGE")) |>#
  arrange(ID_CHANTIER, ACCORD_CADRE, DATE_RECEPTION) |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  mutate(
    DATE_PREC = lag(DATE_RECEPTION),
    JOURS_CALENDAIRES = as.numeric(DATE_RECEPTION - DATE_PREC),
    JOURS_TRAVAILLES = JOURS_CALENDAIRES * (5/7),
    VITESSE_MOY_JOUR_M3 = ifelse(JOURS_TRAVAILLES > 0,
                          QUANTITE_RECEPTION / JOURS_TRAVAILLES,
                          NA_real_)
  ) |>
  filter(JOURS_CALENDAIRES > 3 & JOURS_CALENDAIRES < 90 & VITESSE_MOY_JOUR_M3 > 0 & VITESSE_MOY_JOUR_M3 < 100) |> 
  ungroup()

#Filtres : Jours calendaires 1 montage + 1 démontage + 1 travail effectif = 3 jours minimum
# Jours calendaires au delà de 90 jours, c'est que la dernière date concerne le bois énergie laissé sur place et récolté minimum 3 mois après
# Vitesse moyenne < 100 car impossible d'aller au dessus d'après la productivité max en europe 
# Vitesse moyenne > 0 obvious

df_vitesse_intra_chantier <- df_vitesse_intra |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    VITESSE_MOY_JOUR_M3 = weighted.mean(
      x = VITESSE_MOY_JOUR_M3, 
      w = JOURS_TRAVAILLES,
      na.rm = TRUE),
    .groups = "drop"
  ) #  affiche la vitesse moyenne pondérée par chantier en moyennant les différentes vitesses selon leur temps de chantier

df_vitesse_intra_chantier |>
  summarise(vitesse_moy = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)) # affiche la vitessse moyenne de débardage tout chnatier confondu

####
# 2e estimation en utilisant la cinétique globale sur le nombre de jours effectifs et le volume réel débardé : 
####

df_vitesse_inter <- df_inter3 |> # On garde uniquement les colonnes qui nous sont utiles
  select(ID_CHANTIER, ACCORD_CADRE, JOURS_TRAVAILLES, 
         JOURS_TRAVAILLES_AVEC_MONTAGE, VOL_REF, TYPE_CHANTIER) |>
  mutate(
    VITESSE_MOY_JOUR_M3 = ifelse(
      JOURS_TRAVAILLES_AVEC_MONTAGE > 3,
      VOL_REF / JOURS_TRAVAILLES_AVEC_MONTAGE, #Volume réel divisé par le nombre de jours effectifs par chantier 
      NA_real_ # On met NA si JOURS_EFFECTIFS <= 0 pour éviter les divisions par zéro
    )
  ) |>
  filter(
    !is.na(VITESSE_MOY_JOUR_M3),
    VITESSE_MOY_JOUR_M3 > 0,
    VITESSE_MOY_JOUR_M3 < 100 #on enlève les valeurs supérieures à 100M3/jour
  )

vitesse_moyenne_debardage <- df_vitesse_inter |>
  summarise(
    VITESSE_MOY_JOUR_M3 = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)
  ) |>
  pull(VITESSE_MOY_JOUR_M3)

vitesse_moyenne_debardage #affiche la vitesse moyenne de débardage 


####
# 3e estimation en utilisant les cinétiques de débardage annuelle par ETF : 
####

df_vitesse_annuelle <- df_vitesse_intra |>
    mutate(ANNEE = year(DATE_RECEPTION)) |> # on affecte le volume à l’année de réception du bois, pas à l’année du chantier, pour permettre de visualiser la production annuelle
    group_by(NUM_ETF, ANNEE) |>
    summarise(
      VOL_TOT_ANNUEL_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE),
      NB_CHANTIERS = n_distinct(ID_CHANTIER), # Un même chantier se retrouve dans les deux années s'il s'étale sur plusieurs années
      .groups = "drop"
    )

df_jours_travailles <- commandes |>
  mutate(DATE_RECEPTION = as.Date(DATE_RECEPTION, format = "%d/%m/%Y"),
         ANNEE = year(DATE_RECEPTION)) |>
  group_by(NUM_ETF, ANNEE) |>
  summarise(
    debut = min(DATE_RECEPTION, na.rm = TRUE),
    fin   = max(DATE_RECEPTION, na.rm = TRUE),
    JOURS_CALENDAIRES = as.numeric(fin - debut) + 1,
    JOURS_TRAVAILLES = JOURS_CALENDAIRES * (5/7),
    
    .groups = "drop"
  )

df_vitesse_annuelle <- df_vitesse_annuelle |> # PRODUCTIVITE DATE DE CHANTIER
  left_join(df_jours_travailles,
            by = c("NUM_ETF", "ANNEE")) |>
  mutate(
    VITESSE_MOY_JOUR_M3 = VOL_TOT_ANNUEL_M3 / JOURS_TRAVAILLES
  ) |>
  
  filter(JOURS_TRAVAILLES >= 130 & NB_CHANTIERS > 1) # On ne retietn que les ETF ayant travaillés + de 130jours par mois et au moins 2 chantiers/an
# 130 jours car 5 mois corrspond à période hivernales où les sols sont les + engorgés et où le câble est requis

df_vitesse_annuelle |>
  summarise(vitesse_moy = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)) # affiche la vitessse moyenne de débardage tout chnatier confondu


df_vitesse_intra_chantier$TYPE_VITESSE <- "intra_chantier"
df_vitesse_inter$TYPE_VITESSE <- "inter_chantier"
df_vitesse_annuelle$TYPE_VITESSE <- "annuelle_etf"
df_vitesse_complet <- bind_rows(df_vitesse_intra_chantier, df_vitesse_inter, df_vitesse_annuelle)

write_csv(df_vitesse_complet, "6df_vitesse_complet.csv")

##### PLOT 1

install.packages("ggstatsplot")
library(ggstatsplot)

plt <- ggbetweenstats(
  data = df_vitesse_complet,
  x = TYPE_VITESSE,
  y = VITESSE_MOY_JOUR_M3
)

plt

