# Charger les packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales) #pour le camembert des coûts fixes

#Importation des données
setwd("~/Documents/S10_BETA/scable_dev")
cable <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/cable_dataset.csv")
terre <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/terrestre_dataset.csv")
#============================
# Tri des données terrestres pour enlever les valeurs aberrantes du tableau >50€/m³
#============================
terre_clean <- terre %>%
  filter(COUT_DEB_TERRE_M3 < 50, COUT_ABA_TERRE_M3 < 50)

cat("\nNb chantiers câble:", nrow(cable), "\n")
cat("Nb lignes terrestres (brut):", nrow(terre), "\n") #permet d'afficher le nombre de ligne avant retrait des valeurs aberrantes
cat("Nb lignes terrestres (clean):", nrow(terre_clean), "\n") #affiche le nombre de ligne après retrait 


#=============================
# Comparaison des médianes
#=============================
metrics <- data.frame(
  Variable = c("Coût débardage (€/m³)",
               "Coût abattage (€/m³)",
               "Coût fixes totaux (€/m³)",
               "Tarif prestation (€/m³)",
               "Volume réel (m³)",
               "Diamètre moyen (cm)",
               "Vol moyen grume (m³)",
               "Prix vente moyen (€)"),
  Cable = c(median(cable$COUT_DEB_M3, na.rm=TRUE),
            median(cable$COUT_ABA_M3, na.rm=TRUE),
            median(cable$COUT_FIXES_M3, na.rm=TRUE),
            median(cable$TARIF_PRESTA_M3, na.rm=TRUE),
            median(cable$VOL_REEL_M3, na.rm=TRUE),
            median(cable$DIAM_MOY, na.rm=TRUE),
            median(cable$VOL_MOYEN_GRUME, na.rm=TRUE),
            median(cable$PRIX_VENTE_MOY, na.rm=TRUE)),
  Terrestre = c(median(terre_clean$COUT_DEB_TERRE_M3, na.rm=TRUE),
                median(terre_clean$COUT_ABA_TERRE_M3, na.rm=TRUE),
                median(terre_clean$COUT_FIXES_TERRE_M3, na.rm=TRUE),
                median(terre_clean$TARIF_PRESTA_TERRE_M3, na.rm=TRUE),
                median(terre_clean$VOL_REEL_TERRE_M3, na.rm=TRUE),
                median(terre_clean$DIAM_MOY, na.rm=TRUE),
                median(terre_clean$VOL_MOYEN_GRUME, na.rm=TRUE),
                median(terre_clean$PRIX_VENTE_MOY, na.rm=TRUE))
)

#J'arrondi les valeurs à un chiffre après la virgule pour les analyses 
metrics <- metrics %>%
  mutate(
    Cable = round(Cable, 1),
    Terrestre = round(Terrestre, 1),
    Diff = round((Cable - Terrestre) / Terrestre * 100, 1) 
  )
print(metrics)

#J'arrondi les valeurs à un chiffre après la virgule pour l'export et l'affichage
metrics_export <- metrics %>%
  mutate(
    Cable = sprintf("%.1f", Cable),
    Terrestre = sprintf("%.1f", Terrestre),
    Diff = sprintf("%.1f", Diff)
  )

write_csv(metrics_export, "~/Documents/S10_BETA/scable_dev/results/global/comparaison_cable_terrestre.csv")


#============================
# Comparaison des essences dominantes pour chaqu emode de débardage
#============================

top_ess <- bind_rows(cable, terre_clean) %>%
  count(ESSENCE_DOM) %>%
  slice_max(n, n = 8) %>%
  pull(ESSENCE_DOM)

ess_all <- bind_rows(
  cable %>% mutate(Type = "Câble"),
  terre_clean %>% mutate(Type = "Terrestre")
) %>%
  filter(ESSENCE_DOM %in% top_ess) %>%
  count(Type, ESSENCE_DOM) %>%
  group_by(Type) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(ess_all, aes(x = reorder(ESSENCE_DOM, pct), y = pct, fill = Type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Comparaison des essences dominantes (%)",
       x = "Essence",
       y = "Pourcentage (%)") +
  scale_fill_manual(values = c("Câble" = "steelblue", "Terrestre" = "orange")) +
  theme_minimal()


#============================
# Comparaison des tarifs de prestation câble vs terrestre
#============================
tarif <- data.frame(
  Type = c(rep("Câble", nrow(cable)), rep("Terrestre", nrow(terre_clean))),
  Tarif = c(cable$TARIF_PRESTA_M3, terre_clean$TARIF_PRESTA_TERRE_M3)
)
# Filtre les valeurs aberrantes (>75€/m³)
tarif_clean <- tarif %>%
  filter(Tarif <= 75)

ggplot(tarif_clean, aes(x=Type, y=Tarif, fill=Type)) +
  geom_boxplot(outlier.colour="red", outlier.shape=1) +
  labs(title="Comparaison du tarif prestation câble vs terrestre (≤75 €/m³)",
       y="Tarif prestation (€/m³)", x="Type de chantier") +
  theme_minimal()

