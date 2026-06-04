# =========================
# SCRIPT N°2 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif d'abattage et débardage (au m3) facturé à l'ONF du débardage par câble aérien. 
# =========================

# =========================
# 1. PACKAGES
# =========================
library(stringr)
library(dplyr)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
df_input <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/input/cable_dataset_input.csv")

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
    COUT_ABA_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
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
    COUT_DEB_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
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
    COUT_ABADEB_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
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


write_csv(cable_aba_deb_complet, "cable_aba_deb_complet.csv")
