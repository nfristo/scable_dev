# =========================
# SCRIPT N°1 réalisé par Nicolas FRISTO pour le stage SCABLEDEV au BETA 23/02/2026 au 21/08/2026
# Ce script a pour objectif de créer les dataset répertoriant les actes d'abattage et débardage en triant les LIBELLE_ARTICLE et LIBELLE_POSTE
# du csv commandes_sap.
# Le LIBELLE_POSTE prime sur le LIBELLE_ARTICLE tout au long du script.
# ATTENTION ! Le dataset a déjà été trié et existe, ne pas relancer le script sans avoir explorer la base de données.
# Il faut en effet effectuer certains tri manuellement dans le script et il faut connaitre quelles lignes refuser et approuver.
# =========================

# =========================
# 1. PACKAGES
# =========================
library(stringr)
library(dplyr)

# =========================
# 2. CHARGEMENT DES DONNÉES
# =========================
setwd("~/Documents/S10_BETA/scable_dev")
chantiers <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/CHANTIERS.csv") |>
  filter(ETAT_CHANTIER %in% c("Terminé", "En cours")) #On prend en compte uniquement les chantiers terminés ou en cours
ID_ok <- chantiers$ID_CHANTIER
commandes <- read_csv("~/Documents/S10_BETA/scable_dev/resources/inhouse/COMMANDES_SAP.csv") |>
  filter(ID_CHANTIER %in% ID_ok) #idem

# =====================================
# 1. CREATION D'UN ID UNIQUE PAR LIGNE
# =====================================
commandes <- commandes |>
  mutate(ID_LIGNE = row_number())

# ===================================
# 2. FILTRE DES LIGNES ABATTAGE
# ===================================
abattage_base <- commandes |>
  filter(
    str_detect(LIBELLE_ARTICLE, regex("Abattage", ignore_case = TRUE)) | 
      str_detect(LIBELLE_POSTE, regex("Abattage|Abbatge|AB|Abat|ABA", ignore_case = TRUE))
  ) # On créé une double vérification dans les deux colonnes pour sélectionner toutes les possibilités

abattage_ok <- abattage_base |> #LIBELLE_POSTE prime donc ceux qui contiennt abbatage ici sont OK
  filter(str_detect(LIBELLE_POSTE, regex("Abattage|Abbatge|Abat|ABA", ignore_case = TRUE)))

abattage_doute <- abattage_base |> # pour tout le reste, on les place dans un tableur et on effectue une review manuelle
  filter(!str_detect(LIBELLE_POSTE, regex("Abattage|Abbatge|Abat|ABA", ignore_case = TRUE)))

abattage_doute$validation <- NA
abattage_doute$type_validation <- NA

for(i in 1:nrow(abattage_doute)) {
  cat("\n-------------------------\n")
  print(abattage_doute[i, c("LIBELLE_ARTICLE", "LIBELLE_POSTE", "MONTANT_RECEPTION")]) #MONTANT_RCEPTION permet de se faire une idée si ça peut correspondre
  
  rep <- readline(prompt = "y = abattage | d = débardage | n = exclure : ") #donne la possibilité de placer directemetn dans débardage ceux qui sont mal classés
  # ex: LIBELLE_ARTICLE = Abattage et LIBELLE_POSTE = Débardage 
  if(tolower(rep) == "y") {
    abattage_doute$validation[i] <- TRUE
    abattage_doute$type_validation[i] <- "ABATTAGE"
  } else if(tolower(rep) == "d") {
    abattage_doute$validation[i] <- TRUE
    abattage_doute$type_validation[i] <- "DEBARDAGE"
  } else {
    abattage_doute$validation[i] <- FALSE
  }
}

abattage_valide <- abattage_doute |>
  filter(validation == TRUE, type_validation == "ABATTAGE") #tous ceux validés sont ajoutés au tableur des abattage OK

debardage_depuis_abattage <- abattage_doute |> #ceux classés en débardage seront ajoutés par la suite dans le tableur débardage
  filter(validation == TRUE, type_validation == "DEBARDAGE")

# =====================================
# 3. FILTRE DES LIGNES DEBARDAGE
# =====================================
#on répète l'opération précédente avec le débardage 
debardage_base <- commandes |>
  filter(
    str_detect(LIBELLE_ARTICLE, regex("Débardag", ignore_case = TRUE)) |
      str_detect(LIBELLE_POSTE, regex("Débard|DB|DEB", ignore_case = TRUE))
  )

debardage_ok <- debardage_base |>
  filter(str_detect(LIBELLE_POSTE, regex("Débard|DB|DEB", ignore_case = TRUE)))

debardage_doute <- debardage_base |>
  filter(!str_detect(LIBELLE_POSTE, regex("Débard|DB|DEB", ignore_case = TRUE)))

debardage_doute$validation <- NA
debardage_doute$type_validation <- NA

for(i in 1:nrow(debardage_doute)) {
  cat("\n-------------------------\n")
  print(debardage_doute[i, c("LIBELLE_ARTICLE", "LIBELLE_POSTE", "MONTANT_RECEPTION")])
  
  rep <- readline(prompt = "y = débardage | a = abattage | n = exclure : ") #posdiblité de classer directemetn en abattage les mal classés
  
  if(tolower(rep) == "y") {
    debardage_doute$validation[i] <- TRUE
    debardage_doute$type_validation[i] <- "DEBARDAGE"
  } else if(tolower(rep) == "a") {
    debardage_doute$validation[i] <- TRUE
    debardage_doute$type_validation[i] <- "ABATTAGE"
  } else {
    debardage_doute$validation[i] <- FALSE
  }
}

debardage_valide <- debardage_doute |>
  filter(validation == TRUE, type_validation == "DEBARDAGE")

abattage_depuis_debardage <- debardage_doute |>
  filter(validation == TRUE, type_validation == "ABATTAGE")

# =====================================
# 3. ASSEMBLAGE DES DATASETS
# =====================================

abattage_final <- bind_rows(
  abattage_ok,
  abattage_valide,
  abattage_depuis_debardage
)

debardage_final <- bind_rows(
  debardage_ok,
  debardage_valide,
  debardage_depuis_abattage
)

commandes_triees <- commandes |> #on modifie le dataset commandes en ajoutant la colonne TYPE_TRAVAUX et ID_LIGNE
  mutate(TYPE_TRAVAUX = case_when(
    ID_LIGNE %in% abattage_final$ID_LIGNE ~ "ABATTAGE",
    ID_LIGNE %in% debardage_final$ID_LIGNE ~ "DEBARDAGE",
    TRUE ~ "AUTRE"
  ))

#write.csv(commandes_triees, "~/Documents/S10_BETA/scable_dev/results/commandes_triees.csv", row.names = FALSE)

abattage <- commandes_triees |>
  filter(TYPE_TRAVAUX == "ABATTAGE") #le tableur abattage contient uniquement les abattages qui en sont vraiment

debardage <- commandes_triees |> #le tableur débardage contient uniquement les débardages qui en sont vraiment
  filter(TYPE_TRAVAUX == "DEBARDAGE")


#debardage <- debardage %>% 
  #slice(-c(112, 113,88,89,90,91,92)) 
#ligne utilisée pour supprimer les lignes du chantier MS35 et 38 qui s'étaient glissé dans les données mais qui ont été discriminées
#grâce à l'unité et le PU_COMMANDE

write.csv(abattage, "~/Documents/S10_BETA/scable_dev/results/cable_ABA_dataset.csv", row.names = FALSE)
write.csv(debardage, "~/Documents/S10_BETA/scable_dev/results/cable_DEB_dataset.csv", row.names = FALSE)

