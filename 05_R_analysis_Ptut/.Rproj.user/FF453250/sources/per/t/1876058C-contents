# Ce script a pour objectif de rassembler les donnees pour constituer le jeu de données à utiliser pour le modèle GAM et GAMM dans le cadre du projet tutoré UPPA 2025-2026 sur la caractérisation de la nage de Salmo trutta

# /////////////////////////////////////////////////////////////////////////////#
#### //////////////////// SÉQUENCES STEADY Stationnaire /////////////////// ####
# /////////////////////////////////////////////////////////////////////////////#

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2, readr, lme4)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

fichiers_AM <- list.files("INPUT/RESULTATS/", pattern = "AM_S", full.names = TRUE)
fichiers_AV <- list.files("INPUT/RESULTATS/", pattern = "AV_S", full.names = TRUE)

# import et formatage AM ####

data_AM <- fichiers_AM %>% map_df(~ read_csv(.x, show_col_types = TRUE) %>%
                                    mutate(source = basename(.x))) %>% rename(points = ...1)

# création colonne pour avoir les 15 points par segment

data_AM$trace <- rep(1:15, length.out = nrow(data_AM))

# rajout d'une colonne segment pour identifier les différents segments tracé par poisson

data_AM <- data_AM %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

# Nombre de segments max par poisson pour créer une palette de couleur pour observer le déplacement au fur et à mesure des segments

n_segments_max_AM <- data_AM %>%
  group_by(source) %>%
  summarise(n_seg = max(segment)) %>%
  pull(n_seg) %>%
  max()

# Palette de couleur AM

couleurs_AM <- colorRampPalette(c("darkblue", "lightblue"))(n_segments_max_AM)

# Ajout des couleurs a chaque segment

data_AM <- data_AM %>%
  mutate(couleur = couleurs_AM[segment])

# import et formatage AV ####

data_AV <- fichiers_AV %>% map_df(~ read_csv(.x, show_col_types = TRUE) %>%
                                    mutate(source = basename(.x))) %>% rename(points = ...1)

# création colonne pour avoir les 15 points par segment

data_AV$trace <- rep(1:15, length.out = nrow(data_AV))

# rajout d'une colonne segment pour identifier les différents segments tracé par poisson

data_AV <- data_AV %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

# Nombre de segments max par poisson pour créer une palette de couleur pour observer le déplacement au fur et à mesure des segments

n_segments_max_AV <- data_AV %>%
  group_by(source) %>%
  summarise(n_seg = max(segment)) %>%
  pull(n_seg) %>%
  max()

# Palette de couleur AM

couleurs_AV <- colorRampPalette(c("darkblue", "lightblue"))(n_segments_max_AV)

# Ajout des couleurs a chaque segment

data_AV <- data_AV %>%
  mutate(couleur = couleurs_AV[segment])

# rassembler les deux fichiers à la suite par lignes

dataAMAV <- bind_rows(data_AV,data_AM)

# //// AJOUT des covariables //// ####

# identifiant ####

# extraction des ID a partir de la source

dataAMAV <- dataAMAV %>% mutate(id = str_extract(source, "\\d+"))

# zone ####

dataAMAV <- dataAMAV %>% mutate(zone = str_extract(source, "z\\d+"))

# traitement ####

dataAMAV <- dataAMAV %>% mutate(traitement = str_extract(source, "A[VM]"))

# sequence ####

dataAMAV <- dataAMAV %>% mutate(sequence = str_extract(source, "[S]"))

# taille ####

taille <- read_excel("INPUT/TAILLE/taille.xlsx")

# mise en forme des ID dans le jeu de données taille 

taille <- taille %>%
  mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

# left join dans jeu données initial

dataAMAV <- dataAMAV %>% left_join(taille,dataAMAV, by = "id") %>% rename("taille_mm" = taille)

# ->->->-> jeu de données OBSERVATION / DATA  <-<-<-<- ####

# extraction du : poids_g, observateur, date_observation, traitement

observation <- read_csv("INPUT/data_07_10.csv",locale = locale(decimal_mark = ","))

# mettre id bien 

observation <- observation %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

# poids ####

poids_poisson <- observation %>% distinct(id, poids_g)

dataAMAV <- dataAMAV %>% left_join(poids_poisson, by = "id")

# ->->->-> jeu de données INFOPOISSON / etat d'avancement  <-<-<-<- ####

# Intégration de bief ####

infopoisson <- read_csv("INPUT/infopoisson04_10.csv", locale = locale(decimal_mark = ",")) 

infopoisson <- infopoisson %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

infpoiss <- infopoisson %>% dplyr::select(id,bief)

dataAMAV <- dataAMAV %>% left_join(infpoiss, by = "id")

# JEU DE DONNÉES FINAL -> prochain script : 02_STANDARDISATION ###

save(dataAMAV, file = "OUTPUT/dataAMAV.RData")

# /////////////////////////////////////////////////////////////////////////////#
#### /////////////////////// SÉQUENCES STEADY Mobile /////////////////////// ####
# /////////////////////////////////////////////////////////////////////////////#

# /// IMPORT DES DONNEES /// ####

fichiers_AM <- list.files("INPUT/RESULTATS/", pattern = "AM_M", full.names = TRUE)
fichiers_AV <- list.files("INPUT/RESULTATS/", pattern = "AV_M", full.names = TRUE)

# import et formatage AM ####

data_AM <- fichiers_AM %>% map_df(~ read_csv(.x, show_col_types = TRUE) %>%
                                    mutate(source = basename(.x))) %>% rename(points = ...1)

# création colonne pour avoir les 15 points par segment

data_AM$trace <- rep(1:15, length.out = nrow(data_AM))

# rajout d'une colonne segment pour identifier les différents segments tracé par poisson

data_AM <- data_AM %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

# Nombre de segments max par poisson pour créer une palette de couleur pour observer le déplacement au fur et à mesure des segments

n_segments_max_AM <- data_AM %>%
  group_by(source) %>%
  summarise(n_seg = max(segment)) %>%
  pull(n_seg) %>%
  max()

# Palette de couleur AM

couleurs_AM <- colorRampPalette(c("darkblue", "lightblue"))(n_segments_max_AM)

# Ajout des couleurs a chaque segment

data_AM <- data_AM %>%
  mutate(couleur = couleurs_AM[segment])

# import et formatage AV ####

data_AV <- fichiers_AV %>% map_df(~ read_csv(.x, show_col_types = TRUE) %>%
                                    mutate(source = basename(.x))) %>% rename(points = ...1)

# création colonne pour avoir les 15 points par segment

data_AV$trace <- rep(1:15, length.out = nrow(data_AV))

# rajout d'une colonne segment pour identifier les différents segments tracé par poisson

data_AV <- data_AV %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

# Nombre de segments max par poisson pour créer une palette de couleur pour observer le déplacement au fur et à mesure des segments

n_segments_max_AV <- data_AV %>%
  group_by(source) %>%
  summarise(n_seg = max(segment)) %>%
  pull(n_seg) %>%
  max()

# Palette de couleur AM

couleurs_AV <- colorRampPalette(c("darkblue", "lightblue"))(n_segments_max_AV)

# Ajout des couleurs a chaque segment

data_AV <- data_AV %>%
  mutate(couleur = couleurs_AV[segment])

# rassembler les deux fichiers à la suite par lignes

dataAMAV <- bind_rows(data_AV,data_AM)

# //// AJOUT des covariables //// ####

# identifiant ####

# extraction des ID a partir de la source

dataAMAV <- dataAMAV %>% mutate(id = str_extract(source, "\\d+"))

# zone ####

dataAMAV <- dataAMAV %>% mutate(zone = str_extract(source, "z\\d+"))

# traitement ####

dataAMAV <- dataAMAV %>% mutate(traitement = str_extract(source, "A[VM]"))

# sequence ####

dataAMAV <- dataAMAV %>% mutate(sequence = str_extract(source, "[M]"))

# taille ####

taille <- read_excel("INPUT/TAILLE/taille.xlsx")

# mise en forme des ID dans le jeu de données taille 

taille <- taille %>%
  mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

# left join dans jeu données initial

dataAMAV <- dataAMAV %>% left_join(taille,dataAMAV, by = "id") %>% rename("taille_mm" = taille)

# ->->->-> jeu de données OBSERVATION / DATA  <-<-<-<- ####

# extraction du : poids_g, observateur, date_observation, traitement

observation <- read_csv("INPUT/data_07_10.csv",locale = locale(decimal_mark = ","))

# mettre id bien 

observation <- observation %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

# poids ####

poids_poisson <- observation %>% distinct(id, poids_g)

dataAMAV <- dataAMAV %>% left_join(poids_poisson, by = "id")

# ->->->-> jeu de données INFOPOISSON / etat d'avancement  <-<-<-<- ####

# Intégration de bief ####

infopoisson <- read_csv("INPUT/infopoisson04_10.csv", locale = locale(decimal_mark = ",")) 

infopoisson <- infopoisson %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

infpoiss <- infopoisson %>% dplyr::select(id,bief)

dataAMAV <- dataAMAV %>% left_join(infpoiss, by = "id")
dataAMAV_M <- dataAMAV
# JEU DE DONNÉES FINAL -> prochain script : 02_STANDARDISATION ###

save(dataAMAV_M, file = "OUTPUT/dataAMAV_M.RData")





