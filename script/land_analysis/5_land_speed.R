# =========================
# SCRIPT LAND N°5 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer les dynamique de production du débardage terrestre.
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
library(sf)
library(lubridate)


# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
commandes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP_terrestres.csv")
terre_before_speed <- read_csv ("~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_alti_slope_complet.csv")

# =========================
# 3. CALCUL DU NOMBRE DE JOURS TRAVAILLES PAR CHANTIER
# =========================


commandes_terre_2 <- commandes_terre|>
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION, format = "%d/%m/%Y"), 
  ) |>
  arrange(ID_FB, DATE_RECEPTION) |>
  
  group_by(ID_FB) |>
  
  mutate(
    ecart_jours = as.numeric(DATE_RECEPTION - lag(DATE_RECEPTION)), #calcul du nombre de jours entre chaque réception
    nouveau_chantier = if_else(is.na(ecart_jours) | ecart_jours > 65, 1, 0), # On construit des périodes de chantier, dès que l’écart entre deux réceptions dépasse 90 jours = nouvelle période
    periode_chantier = cumsum(nouveau_chantier) # on cumule ensuite ces périodes
  ) |>
 
   group_by(ID_FB, periode_chantier) |>
  
  summarise(
    DATE_DEBUT = min(DATE_RECEPTION), #On définit les dates de chaque période pour chaque chantier
    DATE_FIN   = max(DATE_RECEPTION),
    
    .groups = "drop"
  )

commandes_terre_3 <- commandes_terre_2 |>
  mutate(
    DUREE_JOURS = as.numeric(DATE_FIN - DATE_DEBUT) + 1 # On calcule la durée calendaire de chaque période
  ) |>
  
  group_by(ID_FB) |>
  
  summarise(
    NB_JOURS_CHANTIER = sum(DUREE_JOURS, na.rm = TRUE), #On additione le nombre de jours de chaque période pour obtenir une durée de chantier en jours
    NB_PERIODES = n(), # donne le nombre de périodes d'interruption par chantier 
    .groups = "drop"
  )

df_terre_speed <- terre_before_speed |>
  left_join(commandes_terre_3, by = c("ID_FB"))


jours_chantier_terre <- df_terre_speed |>
  group_by(ID_FB) |>
  summarise(
    NB_JOURS_CHANTIER = sum(NB_JOURS_CHANTIER, na.rm = TRUE)
  ) |>
  mutate(
    JOURS_TRAVAILLES = NB_JOURS_CHANTIER * 5/7, #Calcul du nombre dejours travaillés théoriques
    JOURS_TRAVAILLES = if_else(
      JOURS_TRAVAILLES < 2,
      NA_real_,
      JOURS_TRAVAILLES
    )
  ) |>
  select(ID_FB, JOURS_TRAVAILLES)

df_terre_speed <- df_terre_speed |>
  left_join(jours_chantier_terre, by = "ID_FB") #on ajoute jours travaillés au df par ID FB

write_csv(df_terre_speed, "df_terre_speed.csv")

#=========================
# 4. ESTIMATION DE LA VITESSE DE DÉBARDAGE (m3/jour effectif) : 2 méthodes différentes
# =========================

####
# 1ère estimation en utilisant les cinétiques de débardage intra-chantier grâce aux dates de réception : 
####

df_vitesse_intra_terre <- commandes_terre |> # On garde uniquement les colonnes qui nous sont utiles
  select(ID_FB, NUM_ETF, LIBELLE_ARTICLE, UNITE, DATE_RECEPTION, QUANTITE_RECEPTION)

df_vitesse_intra_terre <- df_vitesse_intra_terre |>
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION, format = "%d/%m/%Y"),
  ) |>
  filter(
    grepl("batt", LIBELLE_ARTICLE, ignore.case = TRUE) |
      grepl("^04-EXPL-AB", LIBELLE_ARTICLE) |
      grepl("bardag", LIBELLE_ARTICLE, ignore.case = TRUE) |
      grepl("^04-EXPL-DE", LIBELLE_ARTICLE) 
  )|>
  arrange(ID_FB, DATE_RECEPTION) |>
  group_by(ID_FB) |>
  mutate(
    DATE_PREC = lag(DATE_RECEPTION),
    JOURS_CALENDAIRES = as.numeric(DATE_RECEPTION - DATE_PREC),
    JOURS_TRAVAILLES = JOURS_CALENDAIRES * (5/7),
    VITESSE_MOY_JOUR_M3 = ifelse(JOURS_TRAVAILLES > 0,
                                 QUANTITE_RECEPTION / JOURS_TRAVAILLES,
                                 NA_real_)
  ) |>
  filter(JOURS_CALENDAIRES > 2 & JOURS_CALENDAIRES <= 65 & VITESSE_MOY_JOUR_M3 > 0 ) |> # car si + de 65 jours d'écart entre 2 livraisons, on estime que le chantier a du s'arreter
  ungroup()

  df_vitesse_intra_terre |>
  group_by(ID_FB) |>
  summarise(vitesse_moy = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)) # affiche la vitesse moyenne par chantier en moyennant les différentes vitesses

  df_vitesse_intra_terre |>
  summarise(vitesse_moy = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)) # affiche la vitessse moyenne de débardage tout chnatier confondu

####
# 2e estimation en utilisant la cinétique globale sur le nombre de jours effectifs et le volume réel débardé : 
####

df_vitesse_inter_terre <- df_terre_speed |> # On garde uniquement les colonnes qui nous sont utiles
  select(ID_FB, JOURS_TRAVAILLES, VOL_REF, TYPE_CHANTIER)|>
  mutate(
    VITESSE_MOY_JOUR_M3 = ifelse(
      JOURS_TRAVAILLES > 2, 
      VOL_REF / JOURS_TRAVAILLES,#Volume réel divisé par le nombre de jours effectifs par chantier 
      NA)) |> # On met NA si JOURS_EFFECTIFS <= 0 pour éviter les divisions par zéro
  filter(!is.na(VITESSE_MOY_JOUR_M3) ) #& VITESSE_MOY_JOUR_M3 <= 205 on enlève les valeurs supérieures à 220M3/jour

vitesse_moyenne_debardage_terre <- df_vitesse_inter_terre |>
  summarise(
    VITESSE_MOY_JOUR_M3 = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)
  ) |>
  pull(VITESSE_MOY_JOUR_M3)

vitesse_moyenne_debardage_terre #affiche la vitesse moyenne de débardage 



####
# 3e estimation en utilisant les cinétiques de débardage annuelle par ETF : 
####

df_vitesse_annuelle_terre <- df_vitesse_intra_terre |>
  mutate(ANNEE = year(DATE_RECEPTION)) |> # on affecte le volume à l’année de réception du bois, pas à l’année du chantier, pour permettre de visualiser la production annuelle
  group_by(NUM_ETF, ANNEE) |>
  summarise(
    VOL_TOT_ANNUEL_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE),
    NB_CHANTIERS = n_distinct(ID_FB), # Un même chantier se retrouve dans les deux années s'il s'étale sur plusieurs années
    .groups = "drop"
  )

df_jours_travailles <- commandes_terre |>
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

df_vitesse_annuelle_terre <- df_vitesse_annuelle_terre |> # PRODUCTIVITE DATE DE CHANTIER
  left_join(df_jours_travailles,
            by = c("NUM_ETF", "ANNEE")) |>
  mutate(
    VITESSE_MOY_JOUR_M3 = VOL_TOT_ANNUEL_M3 / JOURS_TRAVAILLES
  ) |>
  
  filter(JOURS_TRAVAILLES >= 130 & NB_CHANTIERS > 1) # On ne retietn que les ETF ayant travaillés + de 130jours par mois et au moins 2 chantiers/an
# 130 jours car 5 mois corrspond à période hivernales où les sols sont les + engorgés et où le câble est requis

df_vitesse_annuelle_terre |>
  summarise(vitesse_moy = mean(VITESSE_MOY_JOUR_M3, na.rm = TRUE)) # affiche la vitessse moyenne de débardage tout chnatier confondu



df_vitesse_intra_terre$TYPE_VITESSE <- "intra_chantier"
df_vitesse_inter_terre$TYPE_VITESSE <- "inter_chantier"
df_vitesse_annuelle_terre$TYPE_VITESSE <- "annuelle_etf"
df_vitesse_terre_complet <- bind_rows(df_vitesse_intra_terre, df_vitesse_inter_terre, df_vitesse_annuelle_terre)

write_csv(df_vitesse_terre_complet, "df_vitesse_terre_complet.csv")

##### PLOT 1

install.packages("ggstatsplot")
library(ggstatsplot)

plt <- ggbetweenstats(
  data = df_vitesse_terre_complet,
  x = TYPE_VITESSE,
  y = VITESSE_MOY_JOUR_M3
)

plt
