# =========================
# SCRIPT LAND N°2 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer les tarifs annexes (au m3) à l'abbatage/débardage terrestre facturé à l'ONF.
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
terre_aba_deb_complet <- read_csv ("~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_aba_deb_complet.csv")


# =====================================
# 3. COUTS ANNEXE DU CHANTIER (Transport places de dépôt, Remise en état de coupe
# Cubage/classement, Ehouppage, Transfert matériel, autres...)  (€/m3)
# =====================================

couts_annexes_terre <- commandes_terre |>
  filter(grepl("ransport|anut|remise|houppage|cubage|ransfert|divers|autres",
               LIBELLE_ARTICLE, ignore.case = TRUE)) |>
  group_by(ID_FB) |>
  summarise(
    COUTS_ANNEXES_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE),
    .groups = "drop"
  )

terre_aba_deb_complet <- terre_aba_deb_complet |>
  left_join(couts_annexes_terre, by = "ID_FB") |>
  mutate(
    COUTS_ANNEXES_M3 = COUTS_ANNEXES_TOTAL / VOL_REF
  )


# =====================================
# 4. CALCUL TARIF DE PRESTATION DEBARDAGE TERRESTRE  (€/m3)
# =====================================
terre_aba_deb_complet <- terre_aba_deb_complet |>
  mutate(
    COUTS_ANNEXES_M3 = ifelse(is.na(COUTS_ANNEXES_M3), 0, COUTS_ANNEXES_M3),
    COUTS_ANNEXES_TOTAL = ifelse(is.na(COUTS_ANNEXES_TOTAL), 0, COUTS_ANNEXES_TOTAL)
  ) |>
  mutate(
    TARIF_PRESTA_TERRE_M3 = COUT_DEB_TERRE_M3 + COUT_ABA_TERRE_M3 + COUTS_ANNEXES_M3,
    TARIF_PRESTA_TERRE_TOT = COUT_TOTAL_DEB + COUT_TOTAL_ABA + COUTS_ANNEXES_TOTAL
  )

write.csv(terre_aba_deb_complet, "~/Documents/S10_BETA/scable_dev/results/global/intermediate/terre/terre_tarif_presta_complet.csv", row.names = FALSE)


ggplot(terre_aba_deb_complet, aes(x = TARIF_PRESTA_TERRE_M3)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Répartition du tarif de prestation terrestre (€/m³)",
    x = "Tarif prestation (€/m³)",
    y = "Nombre de chantiers"
  ) +
  theme_minimal()

ggplot(terre_aba_deb_complet, aes(x = TARIF_PRESTA_TERRE_M3)) +
  geom_density(fill = "lightgreen") +
  labs(
    title = "Densité du tarif de prestation terrestre",
    x = "€/m³"
  ) +
  theme_minimal()

ggplot(terre_aba_deb_complet, aes(x = VOL_REF, y = TARIF_PRESTA_TERRE_M3)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Tarif prestation terrestre vs volume",
    x = "Volume (m³)",
    y = "€/m³"
  )

