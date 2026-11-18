# =========================
# SCRIPT N°2 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif d'abattage et débardage (au m3) facturé à l'ONF du débardage par câble aérien. 
# =========================

# =========================
# 1. PACKAGES
# =========================
library(stringr)
library(dplyr)
library(lubridate)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
df_input <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/input/cable_dataset_input.csv")


# =====================================
# CORRECTION DES MONTANTS FACTURÉS À L'ONF AVEC L'IPP BOIS INSEE
# =====================================

ipp <- read_csv2(
  "~/Documents/S10_BETA/scable_dev/resources/public/INSEE_IPP/valeurs_mensuelles.csv"
)

head(ipp)
names(ipp) <- c("PERIODE", "IPP", "CODE")
ipp <- ipp[-c(1:3), ]

ipp <- ipp |>
  mutate(
    PERIODE = as.Date(paste0(PERIODE, "-01")),
    IPP = as.numeric(IPP)
  )

df_input <- df_input |>
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION),
    
    # Correction des dates mal encodées : 0004-MM-JJ -> 2025-MM-04
    DATE_RECEPTION = if_else(
      year(DATE_RECEPTION) < 2000,
      make_date(
        year = 2025,
        month = month(DATE_RECEPTION),
        day = year(DATE_RECEPTION)
      ),
      DATE_RECEPTION
    ),
    
    PERIODE = floor_date(DATE_RECEPTION, unit = "month")
  )

df_input <- df_input |>
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION),
    PERIODE = floor_date(DATE_RECEPTION, unit = "month")
  )

df_input <- df_input |>
  left_join(
    ipp |> select(PERIODE, IPP), # On ajoute l'IPP correspondant à chaque mois de réception
    by = "PERIODE"
  )


df_input <- df_input |> # Correction des dates mal encodées : 0004-MM-JJ -> 2025-MM-04
  mutate(
    DATE_RECEPTION = as.Date(DATE_RECEPTION),
    DATE_RECEPTION = if_else(
      year(DATE_RECEPTION) < 2000,
      make_date(
        year = 2025,
        month = month(DATE_RECEPTION),
        day = year(DATE_RECEPTION)
      ),
      DATE_RECEPTION
    ),
    
    PERIODE = floor_date(DATE_RECEPTION, unit = "month")
  )

date_ref <- df_input |> # Ici on choisit comme IPP de référence celui du premier mois du premier chantier effectué 
  summarise(
    DATE_REF = min(DATE_RECEPTION, na.rm = TRUE)
  ) |>
  pull(DATE_REF)
periode_ref <- floor_date(date_ref, unit = "month")
IPP_REF <- ipp |>
  filter(PERIODE == periode_ref) |>
  pull(IPP)

df_input <- df_input |>
  mutate(
    MONTANT_RECEPTION_IPP =
      MONTANT_RECEPTION * IPP_REF / IPP # Correction des montants facturés
  )

# =========================
# 3. VOLUME RÉEL ABATTU PAR L'ETF ET FACTURÉ À L'ONF
# =========================

vol_reel_aba <- df_input |>
  filter(TYPE_TRAVAUX == "ABATTAGE") |> #Sélectionne uniquement les lignes d'abattage
  group_by(ID_CHANTIER, ACCORD_CADRE) |> #Regroupe la sélection des bois débardés par chantier
  summarise(VOL_REEL_ABA_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE))

# =========================
# 4. TARIF D’ABATTAGE (€/m3)
# =========================

# Calcul du coût moyen par chantier

cout_aba <- df_input |>
  filter(TYPE_TRAVAUX == "ABATTAGE") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_ABA_M3 = sum(MONTANT_RECEPTION_IPP, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )

# =========================
# 5. VOLUME RÉEL DÉBARDÉ PAR L'ETF ET FACTURÉ À L'ONF
# =========================

vol_reel_deb <- df_input |>
  filter(TYPE_TRAVAUX == "DEBARDAGE") |> #Sélectionne uniquement les lignes de débardage
  group_by(ID_CHANTIER, ACCORD_CADRE) |> #Regroupe la sélection des bois débardés par chantier
  summarise(VOL_REEL_DEB_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE)) #Somme la quantité de bois réceptionné par l'ONF par chantier

# =====================================
# 6. TARIF DU DÉBARDAGE PAR CÂBLE (€/m3)
# =====================================

# Calcul du coût moyen par chantier
cout_deb <- df_input |>
  filter(TYPE_TRAVAUX == "DEBARDAGE") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_DEB_M3 = sum(MONTANT_RECEPTION_IPP, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )


# =========================
# 7. VOLUME RÉEL ABATTU/DÉBARDÉ (SANS DETAILS DU SPLIT DES PRIX) PAR L'ETF ET FACTURÉ À L'ONF
# =========================

vol_reel_abaxdeb <- df_input |>
  filter(TYPE_TRAVAUX == "ABADEB") |> #Sélectionne uniquement les lignes d' abattage x débardage sans détails du split des prix
  group_by(ID_CHANTIER, ACCORD_CADRE) |> #Regroupe la sélection des bois débardés par chantier
  summarise(VOL_REEL_ABADEB_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE)) #Somme la quantité de bois réceptionné par l'ONF par chantier

# =====================================
# 8. TARIF DE L'ABATTAGE/DÉBARDAGE (SANS DETAILS DU SPLIT DES PRIX) PAR CÂBLE (€/m3)
# =====================================

# Calcul du coût moyen par chantier
cout_abaxdeb <- df_input |>
  filter(TYPE_TRAVAUX == "ABADEB") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_ABADEB_M3 = sum(MONTANT_RECEPTION_IPP, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )


cable_aba_deb_complet <- vol_reel_aba |>
  full_join(cout_aba, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  full_join(vol_reel_deb, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  full_join(cout_deb, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  full_join(vol_reel_abaxdeb, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  full_join(cout_abaxdeb, by = c("ID_CHANTIER", "ACCORD_CADRE")) 


write_csv(cable_aba_deb_complet, "~/Documents/S10_BETA/scable_dev/results/global/intermediate/2cable_aba_deb_complet.csv")
