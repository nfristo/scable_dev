# =========================
# SCRIPT N°3 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif du montage/démontage (au m3), et du transfert matériel facturé à l'ONF du débardage par câble aérien. 
# =========================

library(readr)
library(lubridate)
setwd("~/Documents/S10_BETA/scable_dev")
df_input <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/input/cable_dataset_input.csv")
cable_aba_deb_complet <- read.csv("~/Documents/S10_BETA/scable_dev/results/global/intermediate/2cable_aba_deb_complet.csv")

# =====================================
# 1. CORRECTION DES MONTANTS FACTURÉS À L'ONF AVEC L'IPP BOIS INSEE
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

# =====================================
# 2. TARIF DU MONTAGE/DÉMONTAGE DES LIGNES DE DÉBARDAGE  (€/m3)
# =====================================

# Calcul du coût moyen par chantier

cout_montage <- df_input |>
  filter(TYPE_TRAVAUX == "MONTAGE") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_MONTAGE_TOTAL = sum(MONTANT_RECEPTION_IPP, na.rm = TRUE)
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
# 3. COUT DU TRANSFERT MATÉRIEL ETF  (€/m3)
# =====================================

# Calcul du coût moyen par chantier

cout_transfert2 <- df_input |>
  filter(TYPE_TRAVAUX == "TRANSFERT") |>
  group_by(ID_CHANTIER, ACCORD_CADRE) |>
  summarise(
    COUT_TRANSFERT_TOTAL = sum(MONTANT_RECEPTION_IPP, na.rm = TRUE),
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

cout_final <- cout_final |>
  mutate(VOL_REF = VOL_REF.x) |>
  select(-VOL_REF.x, -VOL_REF.y) |>
  relocate(VOL_REF, .after = COUT_ABADEB_M3)

write_csv(cout_final, "~/Documents/S10_BETA/scable_dev/results/global/intermediate/3cable_montage_transf_complet.csv")

