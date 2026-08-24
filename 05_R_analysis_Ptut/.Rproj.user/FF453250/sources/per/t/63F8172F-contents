################################################################################
############################### BOUCLE_SEQ_M ###################################
################################################################################

# Ce script permets de donner un apercu des différentes classes de vitesses repérés
## sur l'ensemble des vidéos décortiquées. Au fur et à mesure de l'avancé du découpage
## de vidéos en annotant les différentes séquences de nage steady MOBILES ainsi que leur
## vitesse de nage associée, on peut retrouver les classes de vitesses qui rassemblent le 
## plus de séquences. La boucle va prendre des classes de vitesses sur l'ensemble des 
## vitesses de par 0 à 1 compter le nombre de séquences qui ont une vitesse comprise entre 0 et 1
## pour un poisson différent de 1 à 192. Puis de 1 à 2, de 2 à 3 etc... jusqu'à la vitesse max
## ensuite on prends des intervalles de 0 à 2 de 0 à 4, jusqu'a des intervalles de 0 à 5 par exemple.
## Au final on peut visualiser les classes de vitesses qui rassemblent le plus de séquences (mais
## il faut qu'il y ai au moins 1 séquence par individu pour que la classe soit valide).
## L'objectif est d'obtenir l'intervalle la plus petite avec le plus de séquences au sein
## de l'intervalle (avec 1 séquence au moins par individu)

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2, dplyr)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

data_seq_M <- read_excel("INPUT/data_03_10.xlsx", sheet = 2, na = "NA") %>% dplyr::select(id, traitement, code_steady, Vitesse_estimee_cm.s) %>% filter(code_steady == "M") %>% rename("vitesse" = Vitesse_estimee_cm.s)

data_seq_M <- read_csv("INPUT/data06_01.csv", 
                       col_types = cols(temps_seq_sec = col_number(), 
                                        Vitesse_estimee_cm.s = col_number()), 
                       locale = locale(decimal_mark = ","))%>% dplyr::select(id, traitement, code_steady, Vitesse_estimee_cm.s) %>% filter(code_steady == "M") %>% rename("vitesse" = Vitesse_estimee_cm.s)



data_seq_M$id %>% unique()

n_distinct(data_seq_M$id)

par(mfrow = c(2,2))
hist(data_seq_M$vitesse, breaks = 5, col = "lightblue")
hist(data_seq_M$vitesse, breaks = 10, col = "lightblue")
hist(data_seq_M$vitesse, breaks = 15, col = "lightblue")
hist(data_seq_M$vitesse, breaks = 20, col = "lightblue")
par(mfrow = c(1,1))

# II. BOUCLE ####

v_min <- floor(min(data_seq_M$vitesse, na.rm = TRUE))
v_max <- ceiling(max(data_seq_M$vitesse, na.rm = TRUE))

n_total <- length(unique(data_seq_M$id))

# SUR LES LARGEURS D'INTERVALLES DE 1 A 5

for (largeur in 1:5) {
  for (borne_inf in seq(v_min, v_max - largeur, by = largeur)) {
    borne_sup <- borne_inf + largeur
    subset <- data_seq_M %>%
      filter(vitesse >= borne_inf, vitesse < borne_sup)
    n_seq <- nrow(subset)
    n_poissons <- length(unique(subset$id))
    resultats <- rbind(resultats, data.frame(
      largeur,
      borne_inf,
      borne_sup,
      n_seq,
      n_poissons))}}

# Les 3 classes de vitesses qui regroupe le plus de poissons (donc de séquences) ####
resultats %>% arrange(desc(n_poissons), desc(n_seq)) %>% dplyr::slice(1:3)

# La classe de vitesse par intervalle qui regroupe plus de poissons (donc de séquences) ####

resultats %>% group_by(largeur) %>% arrange(desc(n_poissons), desc(n_seq)) %>% dplyr::slice(1) %>%
  ungroup() %>% arrange(largeur)

# III. GRAPHIQUES ####

resultats <- resultats %>%
  mutate(classe_vitesse = paste0(borne_inf, "-", borne_sup)) %>%
  arrange(borne_inf) %>%
  mutate(classe_vitesse = factor(classe_vitesse, levels = unique(classe_vitesse)))

resultats$n_poissons
ggplot(resultats, aes(x = classe_vitesse, y = n_seq, fill = as.factor(largeur))) +
  geom_col() +
  facet_wrap(~largeur, scales = "free_x") +
  labs(
    title = "Nombre de séquences par intervalle de vitesse",
    x = "Classe de vitesse (croissante)",
    y = "Nombre de séquences",
    fill = "Largeur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


resultats <- resultats %>%
  distinct(largeur, borne_inf, borne_sup, n_seq, n_poissons) %>%
  arrange(borne_inf, largeur) %>%
  mutate(classe_vitesse = paste0(borne_inf, "-", borne_sup))

resultats <- resultats %>%
  arrange(largeur, borne_inf) %>%
  mutate(classe_vitesse = paste0(borne_inf, "-", borne_sup)) %>%
  group_by(largeur) %>%
  mutate(classe_vitesse = factor(classe_vitesse, 
                                 levels = unique(classe_vitesse))) %>%
  ungroup()

ggplot(resultats, aes(x = classe_vitesse, y = n_poissons, fill = factor(largeur))) +
  geom_col(width = 0.8, colour = "black") +
  
  facet_wrap(~largeur, scales = "free_x", ncol = 1) +
  scale_fill_manual(
    values = c("#1B9E77", "#66C2A5", "#A6D854", "#4DBBD5", "#2C7BB6"),
    name = "Largeur d'intervalle") +
  labs(title = "Distribution des séquences selon les classes de vitesse",
    subtitle = "Nombre de poissons représentés par intervalle",
    x = "Classe de vitesse (cm·s)",
    y = "Nombre de poissons") +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"))

# prendre que 8 et 11 (intervalle de 3)

filtre <- data_seq_M %>% filter(vitesse > 10 & vitesse < 14) %>% group_by(id) %>% dplyr::slice(1) %>% ungroup()

filtre %>% filter(traitement == "AV")
filtre %>% filter(traitement == "AM")

filtre %>% group_by(traitement) %>% summarise(n = n())
