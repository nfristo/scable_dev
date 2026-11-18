#TEST COMPARAISON DE SCRIPT
# =========================
# 1. PACKAGES
# =========================
library(readr)
library(dplyr)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CHANTIERS.csv") |>
  filter(ETAT_CHANTIER %in% c("Terminé", "En cours")) #On prend en compte uniquement les chantiers terminés ou en cours
ID_ok <- chantiers$ID_CHANTIER
commandesX <- read_tsv("~/Documents/S10_BETA/scable_dev/resources/inhouse/Commandes_corr.tsv") |>
  filter(ID_CHANTIER %in% ID_ok)

debardage <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_DEB_dataset.csv")
abattage <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_ABA_dataset.csv")

# =========================
# 3. COUT D’ABATTAGE (€/m3)
# =========================

# Filtre uniquement les lignes d'abattage dans le tableur commandes
abattageX <- commandesX |>
  filter(grepl("CVar_CABLE_ABATTAGE", TYPE_OP, ignore.case = TRUE))

non_trouves_abattage <- abattage |>
  anti_join(abattageX, by = c("ID_CHANTIER", "ACCORD_CADRE"))

cable_abadeb <- non_trouves_abattage %>%
  filter(!is.na(MONTANT_RECEPTION))

#lignes_a_ajouter <- abattage[c(45,46,47), ]
#cable_abadeb <- cable_abadeb[-c(39:44), ]
abattage_final <- abattage_final %>%
  mutate(across(everything(), as.character))
lignes_a_ajouter <- lignes_a_ajouter %>%
  mutate(across(everything(), as.character))
#abattage_final <- bind_rows(abattage_final, lignes_a_ajouter) 

cable_abadeb <- cable_abadeb %>%
  mutate(TYPE_TRAVAUX = ifelse(TYPE_TRAVAUX == "ABATTAGE", "ABADEB", TYPE_TRAVAUX))

write_csv(cable_abadeb, "cable_abaxdeb_dataset.csv")


#abattage <- abattage[-c(59:75, 108:111), ]

abattage <- abattage %>%
  mutate(across(everything(), as.character))

non_trouves_abattageX <- non_trouves_abattageX %>%
  mutate(across(everything(), as.character))

abattage <- bind_rows(abattage, non_trouves_abattageX)

abattage <- abattage %>%
  mutate(TYPE_TRAVAUX = ifelse(is.na(TYPE_TRAVAUX), "ABATTAGE", TYPE_TRAVAUX))
abattage <- abattage %>%
  select(-TYPE_OP, -CHECK)

write_csv(abattage_final, "cable_ABA_final_dataset.csv")

# =====================================
# 4. COUT DU DÉBARDAGE PAR CÂBLE (€/m3)
# =====================================

# Filtre uniquement les lignes de débardage par câble dans le tableur commandes (=hors coûts de déplacements et de mise en place)
debardageX <- commandesX |>
  filter(grepl("CVar_CABLE_DEBARDAGE", TYPE_OP, ignore.case = TRUE))


non_trouves_debardage <- debardage |>
  anti_join(debardageX, by = c("ID_CHANTIER", "ACCORD_CADRE"))

#debardage <- debardage[-c(173), ]

write_csv(debardage, "cable_DEB_final_dataset.csv")

# Filtre uniquement les coûts de transfert du matériel câble et ETF dans le tableur commandes
transfertX <- commandesX |>
  filter(
    (grepl("CFixe_CABLE_TRANSFERT_MAT", TYPE_OP, ignore.case = TRUE)),
  )

non_trouves_transfert <- transfert |>
  anti_join(transfertX, by = c("ID_CHANTIER", "ACCORD_CADRE")) 

#transfert <- transfert[-c(53, 67, 70, 71), ]
write_csv(transfert, "cable_transfert.csv")


montage <- read_delim("~/Documents/S10_BETA/scable_dev/results/cable_montage.csv", delim = ";", locale = locale(decimal_mark = "."))
montageX <- commandesX |>
  filter(
    (grepl("CFixe_CABLE_MONTAGE", TYPE_OP, ignore.case = TRUE)),
  )

non_trouves_montage <- montage |>
  anti_join(montageX, by = c("ID_CHANTIER", "ACCORD_CADRE")) 

