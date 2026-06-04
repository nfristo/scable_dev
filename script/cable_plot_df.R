
# =========================
# ANALYSE des données du tableur df
# =========================
library(dplyr)
library(ggplot2)
library(scales)
# =========================
# 1ere étape : tester toutes les variables non liées à COUT_VARIABLE_M3 qui ont une influence sur elle
# =========================
# Toutes les variables explicatives non liées sont : 
vars <- c("DEP", "FRT", "VOL_PREVU", "NB_LIGNES", "LONGUEUR_MIN", "LONGUEUR_MAX",
          "VOL_REEL_M3", "DIAM_MOY","VOL_MOYEN_GRUME", "PRIX_VENTE_MOY", "NB_GRUMES", 
          "G_TOT_MOY", "VOL_HA_MOY","HA_DES_TOT", "NB_TIGES_CONT",
          "LONGUEUR_MOY", "LON", "LAT", "ESSENCE_DOM", "PEUPLEMENT_MODE")

# Il est possible de tester chaque variable individuellement avec la ligne : 
#summary(lm(COUT_VARIABLES_M3 ~ NB_LIGNES, data = df)) en changeant NB_LIGNES par n'importe quelle variable souhaitant être testée
#Pour interpréter tous les résultats des tests rapidement, nous allons tous les tester et afficher directement les résultats.
#Cration d'un df pour stocker tous les résultats de la boucle for

results <- data.frame(
  variable = character(),
  estimate = numeric(),
  p_value = numeric(),
  r_squared = numeric(),
  stringsAsFactors = FALSE
)

for (v in vars) { #Pour chaque variable, on teste son effet individuel sur COUT_VARIABLES_M3
  formula <- as.formula(paste("COUT_VARIABLES_M3 ~", v))
  model <- lm(formula, data = df)
  
  est <- coef(summary(model))[2, "Estimate"]
  pval <- coef(summary(model))[2, "Pr(>|t|)"]
  r2 <- summary(model)$r.squared
  
  results <- rbind(results, data.frame( #Chaque résultat de la boucle pour un variable est stockée dans le df
    variable = v,
    estimate = est,
    p_value = pval,
    r_squared = r2
  ))
}

# On ajoute la colonne des étoiles de significativité
results <- results %>%
  mutate(signif = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01  ~ "**",
    p_value <= 0.05  ~ "*",
    p_value <= 0.1   ~ ".",
    TRUE             ~ ""
  ))

# Les résultats sont triés par ordre croissant de p-value
results <- results %>%
  arrange(p_value) 
head(results, 20) # Affiche les 20 premiers résultats par p-value (à modifier en fonction du nombre de variables significatives par exemple)

write_csv(results, "~/Documents/S10_BETA/scable_dev/results/global/Cout_variable_relation.csv")

# =========================
# On dispose alors d'une liste de variables ayant une influence significative
# On utilise ces variables ayant un effet sur COUT_VARIABLES_M3 et on teste leur effet combiné
# Régression linéaire
# =========================
modele_variable <- lm(COUT_VARIABLES_M3 ~ DIAM_MOY + VOL_HA_MOY + VOL_MOYEN_GRUME, data = df)
summary(modele_variable) 

modele_variable2 <- lm(COUT_VARIABLES_M3 ~ DIAM_MOY + VOL_HA_MOY + LON, data = df)
summary(modele_variable2)

modele_fixes <- lm(COUT_FIXES_M3 ~ VOL_REEL_M3 + LAT + G_TOT_MOY, data = df)
summary(modele_fixes)

modele_tarif <- lm(TARIF_PRESTA_M3 ~ DIAM_MOY + VOL_HA_MOY + LON, data = df)
summary(modele_tarif)

modele_tarif <- lm(TARIF_PRESTA_M3 ~ DIAM_MOY + VOL_HA_MOY + LON * LAT, data = df)
summary(modele_tarif)

# =========================
# Test corrélation de Pearson
# =========================
cor(df$COUT_VARIABLES_M3, df$DIAM_MOY, use = "complete.obs")
cor(df$COUT_VARIABLES_M3, df$VOL_HA_MOY, use = "complete.obs")
cor(df$COUT_VARIABLES_M3, df$VOL_MOYEN_GRUME, use = "complete.obs")

# =========================
#Relation entre longitude et tarif de prestation (câble)
# =========================

df_plot <- df %>%
  filter(TARIF_PRESTA_M3 <= 45,) #Filtre les valeurs aberrantes au dessus de 60 €/m3

ggplot(df_plot, aes(x = LON, y = TARIF_PRESTA_M3)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relation entre longitude et tarif de prestation (câble)",
    x = "Longitude (Ouest → Est)",
    y = "Tarif de prestation (€/m³)"
  ) +
  scale_x_reverse() +  #permet d'inverser l'axe pour avoir l'ouest à gauche et l'est à droite
  theme_minimal()

# =========================
# Calcul de la part représentée par les coûts fixes dans le tarif de prestation :
# =========================
df_plot <- df_plot %>%
  mutate(part_fixe = COUT_FIXES_M3 / TARIF_PRESTA_M3 * 100)

cat("\nPart coûts fixes dans tarif câble (médiane):", median(df_plot$part_fixe, na.rm=TRUE), "%\n")

#============================
#Répartition des coûts fixes des chantiers par câbles
#============================

repartition <- data.frame(
  Type_cout = c("Montage", "Transfert"),
  Valeur = c(df$Montage, df$Transfert)
) %>%
  mutate(
    pct = Valeur / sum(Valeur),
    label = paste0(Type_cout, "\n", percent(pct, accuracy = 0.1))
  )

ggplot(repartition, aes(x = "", y = Valeur, fill = Type_cout)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 5) +
  scale_fill_manual(values = c("Montage" = "#1f77b4",   
                               "Transfert" = "#ff7f0e")) + 
  labs(title = "Répartition des coûts fixes câble") +
  theme_void()


#============================
#tests relation vitesse de débardage vs contraintes mais là y a mahcine learning/modele c'est sur 
#============================
modele_lin <- lm(
  VITESSE_DEBARDAGE_M3_JOUR ~ VOL_HA_MOY + NB_GRUMES + DIAM_MOY + LONGUEUR_MOY + NB_LIGNES_TOTAL,
  data = df
)
summary(modele_lin)

modele_log <- lm(log(VITESSE_DEBARDAGE_M3_JOUR) ~ VOL_HA_MOY + NB_GRUMES + DIAM_MOY, data = df)
summary(modele_log)
