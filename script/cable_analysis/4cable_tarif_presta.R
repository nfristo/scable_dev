# =========================
# SCRIPT N°4 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de calculer le tarif de prestation (au m3) et toutes ses variantes, facturé à l'ONF du débardage par câble aérien.
# =========================
library(readr)
setwd("~/Documents/S10_BETA/scable_dev")
df_inter <- read_csv("~/Documents/S10_BETA/scable_dev/results/global/intermediate/cable_montage_transf_complet.csv")
df_inter <- df_inter |>
  mutate(VOL_REF = VOL_REF.x) |>
  select(-VOL_REF.x, -VOL_REF.y) |>
  relocate(VOL_REF, .after = COUT_ABADEB_M3)

# Variables supplémentaires facilement calculables
df_inter <- df_inter |>
    # Cas où les deux coûts sont nuls ou NA
    mutate(
      COUT_VARIABLES_M3 = case_when(
        (is.na(COUT_DEB_M3) | COUT_DEB_M3 == 0) &
          (is.na(COUT_ABA_M3) | COUT_ABA_M3 == 0) ~ COUT_ABADEB_M3,
        TRUE ~ COUT_DEB_M3 + COUT_ABA_M3
      ),
      
    COUT_VARIABLES_TOT = COUT_VARIABLES_M3 * VOL_REF,
      
    COUT_FIXES_M3 = COUT_TRANSFERT_M3 + COUT_MONTAGE_M3,
    
    COUT_FIXES_TOT = COUT_TRANSFERT_TOTAL + COUT_MONTAGE_TOTAL,
    
    TARIF_PRESTA_M3 = COUT_VARIABLES_M3 + COUT_FIXES_M3,
    
    TARIF_PRESTA_TOT = COUT_FIXES_TOT + COUT_VARIABLES_TOT,
    
    TARIF_PRESTA_SANS_ABA_M3 = COUT_DEB_M3 + COUT_FIXES_M3,
    
    TARIF_PRESTA_SANS_TRANSFERT_M3 = COUT_VARIABLES_M3 + COUT_MONTAGE_M3,
    
    TARIF_PRESTA_SANS_ABA_TRANSFERT_M3 = COUT_DEB_M3 + COUT_MONTAGE_M3
  )

mean(df_inter$TARIF_PRESTA_M3, na.rm = TRUE) # = 42.78 moyenne avant la révision de la BDD avec Guillaume et Martin
mean(df_inter$TARIF_PRESTA_SANS_ABA_M3, na.rm = TRUE)# = 30.06
mean(df_inter$TARIF_PRESTA_SANS_TRANSFERT_M3, na.rm = TRUE)# = 40.0
mean(df_inter$TARIF_PRESTA_SANS_ABA_TRANSFERT_M3, na.rm = TRUE)# = 27.75

write_csv(df_inter, "df_inter.csv")
