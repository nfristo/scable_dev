# =========================
#SCRIPT D'ASSEMBLAGE DES DONNEES
# =========================
library(readr)
library(dplyr)

debardage <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_DEB_final_dataset.csv")
abattage <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_ABA_final_dataset.csv")
cable_abadeb <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_abaxdeb_dataset.csv")
transfert <- read_csv("~/Documents/S10_BETA/scable_dev/results/cable_transfert.csv")
montage <- read_delim("~/Documents/S10_BETA/scable_dev/results/cable_montage.csv", delim = ";", locale = locale(decimal_mark = "."))

transfert <- transfert %>%
  mutate(TYPE_TRAVAUX = "TRANSFERT")
montage <- montage %>%
  mutate(TYPE_TRAVAUX = "MONTAGE")

abattage <- abattage %>%
  mutate(across(everything(), as.character))
debardage <- debardage %>%
  mutate(across(everything(), as.character))
cable_abadeb <- cable_abadeb %>%
  mutate(across(everything(), as.character))
transfert <- transfert %>%
  mutate(across(everything(), as.character))
montage <- montage %>%
  mutate(across(everything(), as.character))

cable_dataset_inter <- bind_rows(debardage, abattage, cable_abadeb, transfert, montage)

write_csv(cable_dataset_inter, "cable_dataset_inter.csv")
