# =========================
# SCRIPT N°3 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif du montage/démontage (au m3), et du transfert matériel facturé à l'ONF du débardage par câble aérien. 
# =========================

# =====================================
# 1. TARIF DU MONTAGE/DÉMONTAGE DES LIGNES DE DÉBARDAGE  (€/m3)
# =====================================
library(readr)
setwd("~/Documents/S10_BETA/scable_dev")
df_input <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_dataset_input.csv")
cable_aba_deb_complet <- read.csv("~/Documents/S10_BETA/scable_dev/results/cable_aba_deb_complet.csv")


# Calcul du coût moyen par chantier

cout_montage <- df_input |>
  filter(TYPE_TRAVAUX == "MONTAGE") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_MONTAGE_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE)
  ) |>
  left_join(cable_aba_deb_complet, by = c("ID_CHANTIER", "ACCORD_CADRE")) |>
  mutate(
    VOL_REF = ifelse(
      is.na(VOL_REEL_DEB_M3) | VOL_REEL_DEB_M3 == 0,
      VOL_REEL_ABADEB_M3,
      VOL_REEL_DEB_M3
    ),
    COUT_MONTAGE_M3 = COUT_MONTAGE_TOTAL / VOL_REF
  ) |>
  relocate(COUT_MONTAGE_TOTAL, .after = COUT_MONTAGE_M3)


# =====================================
# 2. COUT DU TRANSFERT MATÉRIEL ETF  (€/m3)
# =====================================

# Calcul du coût moyen par chantier

cout_transfert2 <- df_input |>
  filter(TYPE_TRAVAUX == "TRANSFERT") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_TRANSFERT_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE),
    .groups = "drop"
  ) |>
  full_join(cout_montage |>
      select(ID_CHANTIER, ACCORD_CADRE, VOL_REF), by = c("ID_CHANTIER", "ACCORD_CADRE")
  ) |>
  mutate(
    COUT_TRANSFERT_M3 = COUT_TRANSFERT_TOTAL / VOL_REF
  ) |>
  relocate(COUT_TRANSFERT_TOTAL, .after = COUT_TRANSFERT_M3)

cout_final <- cout_montage |>
  full_join(
    cout_transfert2,
    by = c("ID_CHANTIER", "ACCORD_CADRE")
  )

write_csv(cout_final, "cable_montage_transf_complet.csv")

