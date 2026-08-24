################################################################################
################################ AMPLITUDE #####################################
################################################################################

# Ce script a pour objectif de créer des enveloppes qui caractérise les amplitudes 
## de nages des poissons qui ont subis un traitement (croissance fort débit = AVAL / 
## croissance sous faible débit et stable = AMONT). 

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2)

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

data_AM$trace <- rep(1:15, length.out = nrow(data_AM))

data_AM <- data_AM %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

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

data_AV$trace <- rep(1:15, length.out = nrow(data_AV))

data_AV <- data_AV %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

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

# /// I. PLOTS ENVELOPPES RÉUNIS /// ####

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

# AM ####

plot(data_AM$`X (cm)`, data_AM$`Y (cm)`, asp = 1, type = "n",
     xlab = "X (cm)", ylab = "Y (cm)", main = "AM")

for (fichier in unique(data_AM$source)) {
  subset_data <- subset(data_AM, source == fichier)
  n <- nrow(subset_data)
  n_traj <- n / 15
  for (i in 1:n_traj) {
    idx <- ((i - 1) * 15 + 1):(i * 15)
    points(subset_data$`X (cm)`[idx],
           subset_data$`Y (cm)`[idx],
           type = "l",
           col = subset_data$couleur[idx[1]])}}

# AV ####

plot(data_AV$`X (cm)`, data_AV$`Y (cm)`, asp = 1, type = "n",
     xlab = "X (cm)", ylab = "Y (cm)", main = "AV",
     xlim = c(0, 80), ylim = c(0, 36))

for (fichier in unique(data_AV$source)) {
  subset_data <- subset(data_AV, source == fichier)
  n <- nrow(subset_data)
  n_traj <- n / 15
  for (i in 1:n_traj) {
    idx <- ((i - 1) * 15 + 1):(i * 15)
    points(subset_data$`X (cm)`[idx],
           subset_data$`Y (cm)`[idx],
           type = "l",
           col = subset_data$couleur[idx[1]])}}

# PLOTS INDIVIDUELS POUR AM

n_poissons_AM <- length(unique(data_AM$source))
n_cols_AM <- ceiling(sqrt(n_poissons_AM))
n_rows_AM <- ceiling(n_poissons_AM / n_cols_AM)

par(mfrow = c(n_rows_AM, n_cols_AM), mar = c(2, 2, 2, 1))

for (f in unique(data_AM$source)) {
  subset_data <- data_AM %>% filter(source == f)
  plot(subset_data$`X (cm)`, subset_data$`Y (cm)`,
       asp = 1, type = "n", main = f, xlab = "", ylab = "")
  n <- nrow(subset_data)
  n_traj <- n / 15
  for (i in 1:n_traj) {
    idx <- ((i - 1) * 15 + 1):(i * 15)
    points(subset_data$`X (cm)`[idx],
           subset_data$`Y (cm)`[idx],
           type = "l",
           col = subset_data$couleur[idx[1]])}}

# PLOTS INDIVIDUELS POUR AV

n_poissons_AV <- length(unique(data_AV$source))
n_cols_AV <- ceiling(sqrt(n_poissons_AV))
n_rows_AV <- ceiling(n_poissons_AV / n_cols_AV)

par(mfrow = c(n_rows_AV, n_cols_AV), mar = c(2, 2, 2, 1))

for (f in unique(data_AV$source)) {
  subset_data <- data_AV %>% filter(source == f)
  plot(subset_data$`X (cm)`, subset_data$`Y (cm)`,
       asp = 1, type = "n", main = f, xlab = "", ylab = "")
  n <- nrow(subset_data)
  n_traj <- n / 15
  for (i in 1:n_traj) {
    idx <- ((i - 1) * 15 + 1):(i * 15)
    points(subset_data$`X (cm)`[idx],
           subset_data$`Y (cm)`[idx],
           type = "l",
           col = subset_data$couleur[idx[1]])}}

# voir combien d'ID on a par AM et AV

length(unique(data_AM$source))
length(unique(data_AV$source))
