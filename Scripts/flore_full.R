####################################################################
##### QFlore : Préparatioon de la liste d'espèce et de statuts #####
####################################################################



####################################################################
### 1 : Environnement de travail

# Charger les bibliothèques nécessaires
library(rstudioapi)
library(readr)
library(dplyr)

# Définir le dossier de travail
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))



####################################################################
### 2 : Importer les données

# Taxref
taxref <- read_delim("Data/TAXREF_v17_2024/TAXREFv17.txt", delim = "\t", na = "null", escape_double = FALSE, trim_ws = TRUE)

# BDC
bdc <- read_csv("Data/BDC-Statuts-v17/BDC_STATUTS_17.csv", na = "null")

# Table de correspondance Département/Région/Ancienne région
dep <- read_delim("Data/dep.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)



####################################################################
### 3 : Création de la liste d'espèce

## 3.1 : Filtre des bases

# Sélection Taxref
Liste <- taxref %>%
  filter(REGNE == "Plantae",
         # Sélection des noms valides seulement
         CD_REF == CD_NOM, 
         # Sélection des especes, sous espèces et variétés, mais aussi les familles et les genres
         RANG %in% c('FM','GN','ES','SSES','VAR','SVAR','FO'),
         # Présence en métropole
         FR %in% c('B','C','D','E','I','J','M','P','Q','S'))

# Sélection BDC
statuts_bdc <- bdc %>%
  filter(REGNE == "Plantae", 
         LB_TYPE_STATUT %in% c("Liste rouge nationale", "Directive Habitat", "Protection nationale"))

## 3.2 : Préparation des statuts
# Grouper les entités par CD_REF en récupérant l'information de CODE_STATUT et RQ_STATUT

# Liste Rouge Nationale
LRN <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Liste rouge nationale") %>%
  group_by(CD_REF) %>%
  summarise(LRN = paste0(CODE_STATUT, collapse = ", "), 
            LRN_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# Directive Habitat
DH <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Directive Habitat") %>%
  group_by(CD_REF) %>%
  summarise(DH = paste0(unique(CODE_STATUT), collapse = ", "),
            DH_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# Protection Nationale
PN <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Protection nationale") %>%
  group_by(CD_REF) %>%
  summarise(PN = paste0(CODE_STATUT, collapse = ", "), 
            PN_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

## 3.3 : Finalisation de la Liste 

# Fusionner la Liste avec les statuts nationaux
Liste <- Liste %>%
  left_join(LRN, by = "CD_REF") %>%
  left_join(DH, by = "CD_REF") %>%
  left_join(PN, by = "CD_REF")

# Gestion des valeurs nulles
Liste <- Liste %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))

# Réorganiser les colonnes dans l'ordre et factoriser les colonnes pour gagner en place
Liste <- Liste %>%
  select(CD_REF, LB_NOM, NOM_COMPLET, NOM_VERN, CLASSE, ORDRE, FAMILLE, PN, PN_det,
         LRN, LRN_det, DH, DH_det) %>%
  mutate(
    CLASSE = as.factor(CLASSE),
    ORDRE = as.factor(ORDRE),
    FAMILLE = as.factor(FAMILLE),
    PN = as.factor(PN),
    PN_det = as.factor(PN_det),
    DH = as.factor(DH),
    DH_det = as.factor(DH_det),
    LRN = as.factor(LRN),
    LRN_det = as.factor(LRN_det)
  )%>%
  arrange(LB_NOM)

## 3.4 : Export des fichiers en GPKG et CSV

# Créer le chemin du fichier
chemin_fichier <- "Output/Flore/Statuts.gpkg"

# Création du dossier si nécessaire
dir.create(dirname(chemin_fichier), recursive = TRUE)

# Écrire la table dans le fichier GPKG
st_write(obj = Liste, dsn = chemin_fichier, layer = "Liste", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(Liste, "Output/Flore/Liste.csv", row.names = FALSE)



####################################################################
### 4 : Statuts régionaux

## 4.1 : Filtre des bases

# Statuts régionaux
statuts_bdc <- bdc %>%
  filter(REGNE == "Plantae") %>%
  filter(LB_TYPE_STATUT %in% c("Liste rouge régionale", "Protection régionale"))

# Sélection des protections départementales pour un traitement séparé
PD_bdc <- bdc %>%
  filter(REGNE == "Plantae") %>%
  filter(LB_TYPE_STATUT == "Protection départementale") %>%
  # Jonction des noms de région
  merge(dep, by.x = "LB_ADM_TR", by.y = "dep", all.x = TRUE)

# Sélection des déterminances ZNIEFF pour un traitement séparé
ZNIEFF_bdc <- bdc %>%
  filter(REGNE == "Plantae" & LB_TYPE_STATUT == "ZNIEFF Déterminantes")

## 4.2 Initialisation de la boucle régionale

# Initialiser un data frame vide pour stocker les résultats
Region <- data.frame()

# Boucle à travers chaque ancienne région
for (i in unique(dep$anc_reg)) {
   
  # Sélectionner la région et les départements correspondants
  r <- dep[dep$anc_reg == i,]$reg[1]
  departements <- dep[dep$anc_reg == i,]$dep
  
  ## 4.3 Statuts par région
  
  # Filtrer les statuts pour la région spécifique
  statuts_bdc_i <- statuts_bdc %>%
    filter(LB_ADM_TR %in% c(i, r))
  
  # Filtrer les protections départementales pour la région spécifique
  PD_i <- PD_bdc %>%
    filter(anc_reg == i)
  
  # Filtrer les déterminances ZNIEFF pour la région spécifique
  ZNIEFF_i <- ZNIEFF_bdc %>%
    filter(LB_ADM_TR %in% c(i, r, departements))
  
  # Protection régionale
  PR <- statuts_bdc_i %>%
    filter(LB_TYPE_STATUT == "Protection régionale") %>%
    group_by(CD_REF) %>%
    summarise(PR = paste0(CODE_STATUT, collapse = ", "), 
              PR_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))
  
  # Liste rouge régionale
  LRR <- statuts_bdc_i %>%
    filter(LB_TYPE_STATUT == "Liste rouge régionale") %>%
    group_by(CD_REF) %>%
    summarise(LRR = paste0(CODE_STATUT, collapse = ", "), 
              LRR_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))
  
  # Protection départementale
  PD <- PD_i %>%
    #Pour chaque département, récupérer CODE_STATUT et RQ_STATUT par CD_REF
    group_by(LB_ADM_TR, CD_REF) %>%
    summarise(
      Prot = paste0(LB_ADM_TR, " : ", paste0(CODE_STATUT, collapse = " , ")),
      Prot_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "),
      .groups = "drop" ) %>%
    # Concatener sur une région les différents PD pour chaque espèce
    group_by(CD_REF) %>%
    summarise(
      PD = paste0(Prot, collapse = ", "),
      PD_det = paste0(Prot_det[!is.na(Prot_det) & Prot_det != ""], collapse = ", "))
  
  # Déterminance ZNIEFF
  ZNIEFF <- ZNIEFF_i %>%
    group_by(CD_REF) %>%
    summarise(
      ZNIEFF = "Oui", 
      ZNIEFF_det = paste0(unique(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""]), collapse = " ; "))
  
  ## 4.4 Finalisation de la liste régionale
  
  # Fusionner les tables PR, LRR, PD et ZNIEFF
  merged <- PR %>%
    full_join(LRR, by = "CD_REF") %>%
    full_join(PD, by = "CD_REF") %>%
    full_join(ZNIEFF, by = "CD_REF")
  
  # Ajouter la colonne pour l'ancienne région
  merged$REGION <- i
  
  # Ajouter cette table régionale à la table globale
  Region <- bind_rows(Region, merged)
}

## 4.5 Finalisation de la table globale

# Réorganisation et factorisation des colonnes
Region <- Region %>%
  select(REGION, CD_REF, PR, PR_det, LRR, LRR_det, PD, PD_det, ZNIEFF, ZNIEFF_det) %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))
  mutate(across(everything(), as.factor)) %>%
  arrange(REGION, CD_REF)

## 4.6 : Export des fichiers en GPKG et CSV

# Écrire la table dans le fichier GPKG
st_write(obj = Region, dsn = chemin_fichier, layer = "Region", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(Region, "Output/Flore/Region.csv", row.names = FALSE)



