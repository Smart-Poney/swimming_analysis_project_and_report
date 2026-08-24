# Ce script a pour objectif de standardiser les donnees pour constituer le jeu de données à utiliser pour le modèle GAM et GAMM dans le cadre du projet tutoré UPPA 2025-2026 sur la caractérisation de la nage de Salmo trutta

# /////////////////////////////////////////////////////////////////////////////#
#### ///////////////////// SÉQUENCES STEADY Mobiles /////////////////////// ####
# /////////////////////////////////////////////////////////////////////////////#

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2, readr, lme4)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

load("OUTPUT/dataAMAV_M.RData")
dataAMAV_M %>% filter(traitement == "AM") %>% distinct(id) %>% nrow()
dataAMAV_M %>% filter(traitement == "AV") %>% distinct(id) %>% nrow()

# Étape 1 : Translation des données sur X #### 

# mettre chaque trace numéro 15 en 0 sur les axes X donc on décale tout sans déformer

data <- dataAMAV_M %>% group_by(id, segment) %>%
  mutate(x_trans = `X (cm)` - `X (cm)`[trace == 15]) %>%
  ungroup()

# plot collage du point 15 au 0

ggplot(data = data, aes(x = x_trans, y = `Y (cm)`, group = interaction(id, segment))) +
  geom_line()

# enlever le segment de ID 46 pour que le segemnt (a 14 points disparaisse) 

data <- data %>%
  filter(!(id == "019" & segment == 10))

# Étape 2 : Translation de données sur Y #### 

# prendre le barycentre de plusieurs points (trace) (13, 14 et 15) pour faire la translation et mettre ce barycentre sur l'axe Y en 0 et le reste des points en fonction de ce barycentre sur l'axe des Y

barycentres <- data %>% filter(trace %in% c(13, 14, 15)) %>%
  group_by(id, segment) %>% summarise(
    bary_x = mean(`X (cm)`, na.rm = TRUE),
    bary_y = mean(`Y (cm)`, na.rm = TRUE),
    .groups = "drop")

# 2.2 Jointure pour récupérer les coordonnées des barycentres

data <- data %>% left_join(barycentres, by = c("id", "segment"))

# 2.3 Translation sur Y : déplacer tous les points pour mettre le barycentre à Y=0

data <- data %>% mutate(y_trans = `Y (cm)` - bary_y)

ggplot(data = data, aes(x = x_trans, y = y_trans, group = interaction(id, segment),color = segment)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# Étape 3 : rotation des données ####

# ici l'objectif c'est de prendre les points 13 14 15 d'un segment de regarder et de prendre l'angle par rapport au zéro et de remttre l'ensemble des angles des segment au meme angle et ensuite on fait une rotation sur 0 

angles_segments <- data %>%
  filter(trace %in% c(12, 14)) %>%
  group_by(id, segment) %>%
  summarise(
    x_13 = x_trans[trace == 12],
    y_13 = y_trans[trace == 12],
    x_15 = x_trans[trace == 14],
    y_15 = y_trans[trace == 14],
    angle_rad = atan2(y_15 - y_13, x_15 - x_13),
    angle_deg = angle_rad * 180 / pi,
    .groups = "drop")

data <- data %>%
  left_join(angles_segments %>% dplyr::select(id, segment, angle_rad, angle_deg), 
            by = c("id", "segment"))

data <- data %>%
  mutate(x_rot = x_trans * cos(-angle_rad) - y_trans * sin(-angle_rad),
         y_rot = x_trans * sin(-angle_rad) + y_trans * cos(-angle_rad))

ggplot(data = data, aes(x = x_rot, y = y_rot, group = interaction(id, segment),
                        color = traitement)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# Étape 4 : normalisation par taille (X et Y) ####

# conversion cm 

data <- data %>%
  group_by(id, segment) %>%
  mutate(
    # Longueur totale du segment (distance entre min et max de x_rot)
    longueur_segment = max(x_rot) - min(x_rot),
    # Normalisation X : de 0 à -1
    x_norm = (x_rot - min(x_rot) -1) / longueur_segment,
    # Normalisation Y : même facteur de division pour garder les proportions
    y_norm = y_rot / longueur_segment) %>%
  ungroup()

ggplot(data = data, aes(x = x_norm, y = y_norm, group = interaction(id, segment),
                        color = traitement)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# recollage des X 15 sur 0 

data <- data %>% 
  group_by(id, segment) %>%
  mutate(X_xnorm = x_norm - x_norm[trace == 15]) %>%
  ungroup()

ggplot(data = data, aes(x = X_xnorm, y = y_norm, group = interaction(id, segment),
                        color = traitement)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

dataexport_M <- data %>% dplyr::select(id, y_norm, X_xnorm, points,trace, bief, segment, traitement, taille_mm, poids_g, zone,sequence)

save(dataexport_M, file = "OUTPUT/dataexport_M.RData")

# Étape 5 : prendre les max ####

# SANS valeurs absolues

datamax <- dataexport_M %>% group_by(id, trace) %>%
  summarise(Y_max = max(y_norm),
            X_norm = first(X_xnorm),
            segment = first(segment),
            traitement = first(traitement),
            sequence = first(sequence),
            poids_g = first(poids_g),
            .groups = "drop")


datamin <- dataexport_M %>% group_by(id, trace) %>%
  summarise(Y_max = min(y_norm),
            X_norm = first(X_xnorm),
            segment = first(segment),
            traitement = first(traitement),
            poids_g = first(poids_g),
            .groups = "drop")

sansmax <- ggplot(data = datamax, aes(x = X_norm, y = Y_max, group = interaction(id, segment),
                                      color = traitement)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)

sansmin <- ggplot(data = datamin, aes(x = X_norm, y = Y_max, group = interaction(id, segment),
                                      color = traitement)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)


# AVEC valeurs absolues

datamaxabs_M <- dataexport_M %>% 
  group_by(id, trace) %>%
  summarise(Y_max = max(abs(y_norm), na.rm = TRUE),
            X_norm = first(X_xnorm),
            segment = first(segment),
            traitement = first(traitement),
            taille_mm = first(taille_mm),
            poids_g = first(poids_g),
            bief = first(bief),
            zone = first(zone),
            sequence = first(sequence),
            .groups = "drop")

absm <- ggplot(data = datamaxabs_M, aes(x = X_norm, y = Y_max, group = interaction(id, segment),color = traitement)) +
  labs(subtitle = "Mobiles", x = "Position normalisée (X)",
       y = "Amplitude latérale maximale (Y)") +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)

par(mfrow = c(2, 1))
library(patchwork)
abs + absm
sansmax + abs
sansmin + abs
sansmax + sansmin

# deux superposée

datamin <- datamin %>% mutate(YY= Y_max * -1)
datamax <- datamax %>% mutate(type = "max")
datamin <- datamin %>% mutate(type = "min")

ggplot() +
  geom_line(data = datamax, aes(x = X_norm, y = Y_max, group = interaction(id, segment), color = "type"), color = "red") + 
  geom_line(data = datamin, aes(x = X_norm, y = YY, group = interaction(id, segment), color = "type"), color = "black")


# JEU DE DONNÉES FINAL -> prochain script : 05_GAMM ####

save(datamaxabs_M, file = "OUTPUT/dataAMAV02_M.RData")
