# =========================
# SCRIPT réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif de prestation (au m3) du débardage terrestre facturé à l'ONF dans les mêmes massifs que l'exploitation par câble aérien. 
# Il est séparé en plusieurs parties, comprenant le calcul du prix de l'abattage/façonnage, le calcul du prix du débardage,
# le calcul des travaux annexes sur chantier et le calcul des fraix divers supplémentaires des entreprises. 
# =========================

# =========================
# 1. PACKAGES
# =========================
library(readr)
library(dplyr)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
commandes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP_terrestres.csv")
grumes_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/GRUMES_terrestres.csv")
contour_terre <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CONTOUR_GEO_terrestres.csv")

# =========================
# 3. COUT DE L’ABATTAGE TERRESTRE (€/m3)
# =========================

# Filtrer uniquement les lignes d'abattage dans le tableur commandes
abattage_terre <- commandes_terre %>%
  filter(grepl("batt", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("^04-EXPL-AB", LIBELLE_ARTICLE)) # ^ pour "commence par..."

# Calcul du coût moyen par chantier
cout_aba_terre <- abattage_terre %>%
  group_by(ID_FB) %>%
  summarise(
    COUT_ABA_TERRE_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )

# =====================================
# 4. COUT DU DÉBARDAGE TERRESTRE (€/m3)
# =====================================

# Filtrer uniquement les lignes de débardage par câble dans le tableur commandes (=hors coûts de déplacements et de mise en place)
debardage_terre <- commandes_terre %>%
  filter(grepl("bardag", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("^04-EXPL-DE", LIBELLE_ARTICLE)) # ^ pour "commence par..."

# Calcul du coût moyen par chantier
cout_deb_terre <- debardage_terre %>%
  group_by(ID_FB) %>%
  summarise(
    COUT_DEB_TERRE_M3 = sum(MONTANT_RECEPTION, na.rm = TRUE) /
      ifelse(sum(QUANTITE_RECEPTION, na.rm = TRUE) > 0,
             sum(QUANTITE_RECEPTION, na.rm = TRUE),
             NA)
  )

# =========================
# 5. VOLUME RÉEL DÉBARDÉ PAR L'ETF ET FACTURÉ À L'ONF
# =========================

vol_reel_terre <- commandes_terre %>%
  filter(UNITE == "M3"| UNITE == "M3A"| UNITE == "M3E") %>% #Filtre par m3 pour éviter d'inclure les coûts fixes
  group_by(ID_FB) %>% #Regroupe la sélection des bois débardés par chantier
  summarise(VOL_REEL_TERRE_M3 = sum(QUANTITE_RECEPTION, na.rm = TRUE)) #Somme la quantité de bois réceptionné par l'ONF par chantier


# =====================================
# 6. COUT ANNEXE DU CHANTIER (Transport places de dépôt, Remise en état de coupe
# Cubage/classement, Ehouppage)  (€/m3)
# =====================================

# Filtrer uniquement les coûts annexes du chantier dans le tableur commandes
annexe_terre <- commandes_terre %>%
  filter(grepl("ransport", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("anut", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("remise", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("houppage", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("cubage", LIBELLE_ARTICLE, ignore.case = TRUE)
  )

# Calcul du coût moyen par chantier
cout_annexe_terre <- annexe_terre %>%
  group_by(ID_FB) %>%
  summarise(
    COUT_ANNEXE_TERRE_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE) #Calcul du tarif facturé à l'ONF pour les couts annexes au débardage/abattage (hors frais fixes) 
  ) %>%
  left_join(vol_reel_terre, by = "ID_FB") %>%
  mutate(
    COUT_ANNEXE_TERRE_M3 =COUT_ANNEXE_TERRE_TOTAL / VOL_REEL_TERRE_M3, #Tarif ramené au m3 débardé par chantier
  )

# =====================================
# 7. COUT DU TRANSFERT MATÉRIEL ETF (€/m3)
# =====================================

# Filtrer uniquement les coûts de transfert du matériel ETF dans le tableur commandes
transfert_terre <- commandes_terre %>%
  filter(grepl("ransfert", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("divers", LIBELLE_ARTICLE, ignore.case = TRUE) |
           grepl("autres", LIBELLE_ARTICLE, ignore.case = TRUE)
  )

# Calcul du coût moyen par chantier
cout_transfert_terre <- transfert_terre %>%
  group_by(ID_FB) %>%
  summarise(
    COUT_TRANSFERT_TERRE_TOTAL = sum(MONTANT_RECEPTION, na.rm = TRUE) #Calcul du tarif facturé à l'ONF pour le transfert du matériel par chantier 
  ) %>%
  left_join(vol_reel_terre, by = "ID_FB") %>%
  mutate(
    COUT_TRANSFERT_TERRE_M3 = COUT_TRANSFERT_TERRE_TOTAL / VOL_REEL_TERRE_M3 #Tarif ramené au m3 débardé par chantier
  )

# =========================
# 8. CARACTÉRISTIQUES DES GRUMES DEBARDÉES 
# =========================

gr_terre_stats <- grumes_terre %>%
  group_by(ID_FB) %>%
  summarise(
    DIAM_MOY = mean(DIAM_SUR, na.rm = TRUE), #Moyenne du diamètre des grumes débardées par chantier
    VOL_MOYEN_GRUME = mean(VOL_SUR, na.rm = TRUE), #Moyenne du volume des grumes débardées par chantier
    PRIX_VENTE_MOY = mean(PRIX_VENTE, na.rm = TRUE), #Moyenne des prix de vente des grumes par chantier
    NB_GRUMES = n()
  )

# =========================
# 9. CARACTÉRISTIQUES DES PEUPLEMENTS
# =========================

peupl_terre_stats <- contour_terre %>%
  group_by(ID_FB) %>%
  summarise(
    G_TOT_MOY = mean(G_TOT, na.rm = TRUE), #Surface terrière moyenne à l'hectare 
    VOL_HA_MOY = mean(VOL_HA, na.rm = TRUE), #Volume moyen à l'hectare 
    HA_DES_TOT = sum(HA_DES, na.rm = TRUE), #Surface du peuplement en hectare désigné par le TFT  
    NB_TIGES_CONT = sum(NB_TIGES, na.rm = TRUE) #Nombre de tiges à l'hectare
  )

# ESSENCE DOMINANTE PAR PEUPLEMENT
essence <- contour_terre %>%
  group_by(ID_FB) %>%
  summarise(ESSENCE_DOM = names(sort(table(PEUPLEMENT_COMPO), decreasing = TRUE))[1])

# =========================
# 10. ASSEMBLAGE DE TOUS LES TABLEURS PRÉCÉDENTS AVEC ID FB
# =========================

df_terre <- commandes_terre %>%
  select(ID_FB, NUM_SAP, AGENCE, QUANTITE_COMMANDE, 
  ) %>%
  left_join(cout_deb_terre, by = "ID_FB") %>%
  left_join(cout_aba_terre, by = "ID_FB") %>%
  left_join(vol_reel_terre, by = "ID_FB") %>%
  left_join(cout_annexe_terre, by = "ID_FB") %>%
  left_join(cout_transfert_terre, by = "ID_FB") %>%
  left_join(gr_terre_stats, by = "ID_FB") %>%
  left_join(peupl_terre_stats, by = "ID_FB") %>%
  left_join(essence, by = "ID_FB")

df_terre <- df_terre %>%
  mutate(VOL_REEL_TERRE_M3 = VOL_REEL_TERRE_M3.x) %>%
  select(-VOL_REEL_TERRE_M3.x, -VOL_REEL_TERRE_M3.y) #permet de retirer les colonnes Volume réel qui sont ajoutés en doublon lors des jointures

# Variables supplémentaires facilement calculables
df_terre <- df_terre %>%
  mutate(
    COUT_VARIABLES_TERRE_M3 = COUT_DEB_TERRE_M3 + COUT_ABA_TERRE_M3,  #Tarif facturé à l'ONF pour l'abattage et le débardage uniquement 
    COUT_FIXES_TERRE_M3 = COUT_TRANSFERT_TERRE_M3 + COUT_ANNEXE_TERRE_M3, #Tarif facturé à l'ONF pour les frais annexes de chantier et les frais de transferts
    TARIF_PRESTA_TERRE_M3 = COUT_VARIABLES_TERRE_M3 + COUT_FIXES_TERRE_M3, #Tarif de prestation facturé à l'ONF pour le chantier tout compris
  )
df_terre <- df_terre %>% #Permet de mettre tous les NA en 0 mais attendre avant de faire car pas sûr
mutate(
  COUT_VARIABLES_TERRE_M3 = coalesce(COUT_DEB_TERRE_M3, 0) + coalesce(COUT_ABA_TERRE_M3, 0),
  
  COUT_FIXES_TERRE_M3 = coalesce(COUT_TRANSFERT_TERRE_M3, 0) + coalesce(COUT_ANNEXE_TERRE_M3, 0),
  
  TARIF_PRESTA_TERRE_M3 = coalesce(COUT_VARIABLES_TERRE_M3, 0) + coalesce(COUT_FIXES_TERRE_M3, 0)
)

# =========================
# 12. EXPORT
# =========================
write.csv(df_terre, "~/Documents/S10_BETA/scable_dev/results/global/terrestre_dataset.csv", row.names = FALSE)






#===============================================================================================================================================================================
# =========================
# 1. ANALYSE des données
# =========================


# Corrélations
cor(df_terre$COUT_VARIABLES_TERRE_M3, df_terre$DIAM_MOY, use = "complete.obs")
cor(df_terre$COUT_VARIABLES_TERRE_M3, df_terre$VOL_HA_MOY, use = "complete.obs")

# Régression linéaire

modele_variable <- lm(COUT_VARIABLES_TERRE_M3 ~ DIAM_MOY + VOL_HA_MOY + G_TOT_MOY, data = df_terre)
summary(modele_variable)

modele_fixes <- lm(COUT_FIXES_TERRE_M3 ~ DIAM_MOY + VOL_HA_MOY + G_TOT_MOY, data = df_terre)
summary(modele_fixes)

modele_tarif <- lm(TARIF_PRESTA_TERRE_M3 ~ DIAM_MOY + VOL_HA_MOY + G_TOT_MOY, data = df_terre)
summary(modele_tarif)

