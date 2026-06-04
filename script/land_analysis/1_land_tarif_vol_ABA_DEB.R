# =========================
# SCRIPT LAND n°1 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer Ce script a pour objectif de créer les dataset répertoriant les actes d'abattage et débardage du débardage terrestre en triant les LIBELLE_ARTICLE et LIBELLE_POSTE
# du csv commandes_sap_terrestre, puis de calculer le tarif d'abattage et débardage (au m3). 
# Ces actes sont facturé à l'ONF et ont eu lieux dans les mêmes massifs que les chantiers d'exploitation par câble aérien. 
# =========================

# =========================
# 1. PACKAGES
# =========================
library(readr)
library(dplyr)
library(sf)
library(ggplot2)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
commandes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP_terrestres.csv")

# =========================
# 3. COUT DE L’ABATTAGE TERRESTRE (€/m3)
# =========================

# Filtrer uniquement les lignes d'abattage dans le tableur commandes
abattage_terre <- commandes_terre |>
  filter(UNITE == "M3"| UNITE == "M3A"| UNITE == "M3E") |>
  filter(grepl("batt", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("^04-EXPL-AB", LIBELLE_ARTICLE)) # ^ pour "commence par..."

# Calcul du coût moyen par chantier
cout_aba_terre <- abattage_terre |>
  group_by(ID_FB) |>
  summarise(
    VOL_REEL_ABA_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE),
    COUT_TOTAL_ABA = sum(MONTANT_RECEPTION, na.rm = TRUE),
    COUT_ABA_TERRE_M3 = COUT_TOTAL_ABA / VOL_REEL_ABA_M3,
    .groups = "drop"
  ) |>
  filter(VOL_REEL_ABA_M3 > 0)

ggplot(cout_aba_terre, aes(x = COUT_ABA_TERRE_M3)) +
  geom_histogram(binwidth = 5, fill = "forestgreen", color = "black") +
  labs(
    title = "Répartition des coûts d'abattage (€/m³)",
    x = "Coût d'abattage (€/m³)",
    y = "Nombre de chantiers"
  ) +
  theme_minimal()

ggplot(cout_aba_terre, aes(x = COUT_ABA_TERRE_M3)) + 
  geom_density(fill = "lightblue") +
  labs(
    title = "Densité des coûts d'abattage",
    x = "€/m³"
  ) +
  theme_minimal()

# =====================================
# 4. COUT DU DÉBARDAGE TERRESTRE (€/m3)
# =====================================

# Filtrer uniquement les lignes de débardage par câble dans le tableur commandes (=hors coûts de déplacements et de mise en place)
debardage_terre <- commandes_terre |>
  filter(UNITE == "M3"| UNITE == "M3A"| UNITE == "M3E") |>
  filter(grepl("bardag", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("^04-EXPL-DE", LIBELLE_ARTICLE)) # ^ pour "commence par..."

# Calcul du coût moyen par chantier
cout_deb_terre <- debardage_terre |>
  group_by(ID_FB) |>
  summarise(
    VOL_REEL_DEB_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE),
    COUT_TOTAL_DEB = sum(MONTANT_RECEPTION, na.rm = TRUE),
    COUT_DEB_TERRE_M3 = COUT_TOTAL_DEB / VOL_REEL_DEB_M3,
    .groups = "drop"
  ) |>
  filter(VOL_REEL_DEB_M3 > 0)

ggplot(cout_deb_terre, aes(x = COUT_DEB_TERRE_M3)) +
  geom_histogram(binwidth = 5, fill = "forestgreen", color = "black") +
  labs(
    title = "Répartition des coûts de débardage (€/m³)",
    x = "Coût de débardage (€/m³)",
    y = "Nombre de chantiers"
  ) +
  theme_minimal()

ggplot(cout_deb_terre, aes(x = COUT_DEB_TERRE_M3)) + 
  geom_density(fill = "lightblue") +
  labs(
    title = "Densité des coûts de débardage",
    x = "€/m³"
  ) +
  theme_minimal()


terre_aba_deb_complet <- cout_aba_terre |>
  left_join(cout_deb_terre, by = "ID_FB") |>
  mutate(
    VOL_REF = VOL_REEL_DEB_M3
  )


write_csv(terre_aba_deb_complet, "terre_aba_deb_complet.csv")

