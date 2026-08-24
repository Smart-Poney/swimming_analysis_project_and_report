################################################################################
############################ Variabilité inter-opérateur #######################
################################################################################

# Ce script importe et standardise les trajectoires de nage (translation, rotation, normalisation par la taille) 
# afin d’extraire les enveloppes externes. Il évalue ensuite la variabilité inter-observateur à l’aide d’analyses 
# descriptives et de modèles additifs généralisés (GAM), incluant une partition de la variance entre effet individuel, 
# effet observateur et résiduel.

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2, stringr, gam, lme4, mgcv, dplyr, itsadug, pdp)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

fichiers_VIO <- list.files("INPUT/INTER_OPERATEUR/", full.names = TRUE)

# import et formatage ####

data_VIO <- fichiers_VIO %>% map_df(~ read_csv(.x, show_col_types = TRUE) %>%
                                      mutate(source = basename(.x))) %>% rename(points = ...1)

# Ajout trace
data_VIO$trace <- rep(1:15, length.out = nrow(data_VIO))

# Ajout segment
data_VIO <- data_VIO %>%
  group_by(source) %>%
  mutate(segment = rep(1:(n()/15), each = 15)) %>%
  ungroup()

n_segments_max_VIO <- data_VIO %>%
  group_by(source) %>%
  summarise(n_seg = max(segment)) %>%
  pull(n_seg) %>%
  max()

# Palette de couleur
couleurs_VIO <- colorRampPalette(c("darkblue", "lightblue"))(n_segments_max_VIO)

# Ajout des couleurs a chaque segment
data_VIO <- data_VIO %>%
  mutate(couleur = couleurs_VIO[segment])

# Ajout observateur 
data_VIO <- data_VIO %>%
  mutate(observateur = str_extract(source, "(?<=_)[^_]+(?=\\.csv$)"))

data_VIO <- data_VIO %>%
  mutate(observateur = if_else(observateur == "Eva", "EVA", observateur))

data_VIO <- data_VIO %>%
  mutate(id = str_extract(source, "(?<=_)[^_]+(?=_)"))

# /// I. PLOTS ENVELOPPES /// ####

# Plots enveloppes projetées

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

plot(data_VIO$`X (cm)`, data_VIO$`Y (cm)`, asp = 1, type = "n",
     xlab = "X (cm)", ylab = "Y (cm)", main = "VIO")

for (fichier in unique(data_VIO$source)) {
  subset_data_VIO <- subset(data_VIO, source == fichier)
  n_VIO <- nrow(subset_data_VIO)
  n_traj_VIO <- n_VIO / 15
  for (i in 1:n_traj_VIO) {
    idx <- ((i - 1) * 15 + 1):(i * 15)
    points(subset_data_VIO$`X (cm)`[idx],
           subset_data_VIO$`Y (cm)`[idx],
           type = "l",
           col = subset_data_VIO$couleur[idx[1]])}}

# Plots individuels

n_poissons_VIO <- length(unique(data_VIO$source))
n_cols_VIO <- ceiling(sqrt(n_poissons_VIO))
n_rows_VIO <- ceiling(n_poissons_VIO / n_cols_VIO)

par(mfrow = c(n_rows_VIO, n_cols_VIO), mar = c(2, 2, 2, 1))

for (f in unique(data_VIO$source)) {
  subset_data_VIO <- data_VIO %>% filter(source == f)
  plot(subset_data_VIO$`X (cm)`, subset_data_VIO$`Y (cm)`,
       asp = 1, type = "n", main = f, xlab = "", ylab = "")
  n_VIO <- nrow(subset_data_VIO)
  n_traj_VIO <- n_VIO / 15
  for (i in 1:n_traj_VIO) {
    idx <- ((i - 1) * 15 + 1):(i * 15)
    points(subset_data_VIO$`X (cm)`[idx],
           subset_data_VIO$`Y (cm)`[idx],
           type = "l",
           col = subset_data_VIO$couleur[idx[1]])}}

# //// II. STANDARDISATION /// ####

# Étape 1 : Translation des données sur X #### 

# mettre chaque trace numéro 15 en 0 sur les axes X donc on décale tout sans déformer

data <- data_VIO %>% group_by(observateur, id, segment) %>%
  mutate(x_trans = `X (cm)` - `X (cm)`[trace == 15]) %>%
  ungroup()

# plot collage du point 15 au 0

ggplot(data = data, aes(x = x_trans, y = `Y (cm)`, group = interaction(id, observateur, segment))) +
  geom_line()

# Étape 2 : Translation de données sur Y #### 

# prendre le barycentre de plusieurs points (trace) (13, 14 et 15) pour faire la translation et mettre ce barycentre sur l'axe Y en 0 et le reste des points en fonction de ce barycentre sur l'axe des Y

barycentres <- data %>% filter(trace %in% c(13, 14, 15)) %>%
  group_by(observateur, id, segment) %>% summarise(
    bary_x = mean(`X (cm)`, na.rm = TRUE),
    bary_y = mean(`Y (cm)`, na.rm = TRUE),
    .groups = "drop")

# 2.2 Jointure pour récupérer les coordonnées des barycentres

data <- data %>% left_join(barycentres, by = c("observateur", "id", "segment"))

# 2.3 Translation sur Y : déplacer tous les points pour mettre le barycentre à Y=0

data <- data %>% mutate(y_trans = `Y (cm)` - bary_y)

ggplot(data = data, aes(x = x_trans, y = y_trans, group = interaction(observateur, id, segment),color = segment)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# Étape 3 : rotation des données ####

# ici l'objectif c'est de prendre les points 13 14 15 d'un segment de regarder et de prendre l'angle par rapport au zéro et de remttre l'ensemble des angles des segment au meme angle et ensuite on fait une rotation sur 0 

angles_segments <- data %>%
  filter(trace %in% c(12, 14)) %>%
  group_by(observateur, id, segment) %>%
  summarise(
    x_13 = x_trans[trace == 12],
    y_13 = y_trans[trace == 12],
    x_15 = x_trans[trace == 14],
    y_15 = y_trans[trace == 14],
    angle_rad = atan2(y_15 - y_13, x_15 - x_13),
    angle_deg = angle_rad * 180 / pi,
    .groups = "drop"
  )

data <- data %>%
  left_join(angles_segments %>% select(observateur, id, segment, angle_rad, angle_deg),
            by = c("observateur", "id", "segment")
  )

data <- data %>%
  mutate(x_rot = x_trans * cos(-angle_rad) - y_trans * sin(-angle_rad),
         y_rot = x_trans * sin(-angle_rad) + y_trans * cos(-angle_rad))

data <- data %>%
  arrange(observateur, id, segment, x_rot)

ggplot(data = data, aes(x = x_rot, y = y_rot, group = interaction(observateur, id, segment))) +
  geom_path(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# Etape 4 : Standardisation par la taille ####

# conversion en cm 

data <- data %>%
  group_by(observateur, id, segment) %>%
  mutate(
    # Longueur totale du segment (distance entre min et max de x_rot)
    longueur_segment = max(x_rot) - min(x_rot),
    # Normalisation X : de 0 à -1
    x_norm = (x_rot - min(x_rot) -1) / longueur_segment,
    # Normalisation Y : même facteur de division pour garder les proportions
    y_norm = y_rot / longueur_segment) %>%
  ungroup()

ggplot(data = data, aes(x = x_norm, y = y_norm, group = interaction(observateur, id, segment))) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# recollage des x 15 sur 0 

data <- data %>% 
  group_by(observateur, id, segment) %>%
  mutate(X_xnorm = x_norm - x_norm[trace == 15]) %>%
  ungroup()

ggplot(data = data, aes(x = X_xnorm, y = y_norm, group = interaction(observateur, id, segment))) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# Étape 5 : Récupération des enveloppes externes ####

# SANS valeurs absolues

datamax <- data %>% group_by(observateur, id, trace) %>%
  summarise(Y_max = max(y_norm),
            X_norm = first(X_xnorm),
            segment = first(segment),
            observateur = first(observateur),
            .groups = "drop")

datamin <- data %>% group_by(observateur, id, trace) %>%
  summarise(Y_max = min(y_norm),
            X_norm = first(X_xnorm),
            segment = first(segment),
            observateur = first(observateur),
            .groups = "drop")

sansmax <- ggplot(data = datamax, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)
sansmax

sansmin <- ggplot(data = datamin, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)
sansmin

# AVEC valeurs absolues

datamaxabs <- data %>% 
  group_by(observateur, id, trace) %>%
  summarise(Y_max = max(abs(y_norm), na.rm = TRUE),
            X_norm = first(X_xnorm),
            segment = first(segment),
            observateur = first(observateur),
            .groups = "drop")

abs <- ggplot(data = datamaxabs, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)
abs

par(mfrow = c(2, 1))
library(patchwork)
sansmax + abs
sansmin + abs
sansmax + sansmin

# deux superposée

datamin <- datamin %>% mutate(YY= Y_max * -1)
datamax <- datamax %>% mutate(type = "max")
datamin <- datamin %>% mutate(type = "min")

ggplot() +
  geom_line(data = datamax, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id)) +
  geom_line(data = datamin, aes(x = X_norm, y = YY, group = interaction(observateur, id, segment), color = id))

ggplot() +
  geom_line(data = datamax, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id))

# Graph VIO
ggplot() +
  geom_line(data = datamaxabs, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id))

# //// III. VARIABILITE INTER-OPERATEUR /// ####

# Même individu mais opérateurs différents

# Calcul courbe moyenne par individu
courbe_moyenne <- datamaxabs %>% 
  group_by(id, trace) %>%
  summarise(
    Y_mean = mean(Y_max, na.rm = TRUE),
    X_norm = first(X_norm),
    .groups = "drop"
  )

# Graph VIO + moyennes par individu 
plot_VIO_moy <- ggplot() +
  geom_line(data = datamaxabs, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment), color = id), alpha = 0.4) +
  geom_line(data = courbe_moyenne, aes(x = X_norm, y = Y_mean, group = id, color = id), linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  coord_fixed(ratio = 1)
plot_VIO_moy

# 
data_var_inter <- datamaxabs %>%
  left_join(courbe_moyenne %>% select(id, trace, Y_mean),
            by = c("id", "trace")
  )

# Calcul variance sur chaque point
var_inter_point <- data_var_inter %>%
  group_by(id, trace) %>%
  summarise(
    var_inter = var(Y_max - Y_mean, na.rm = TRUE),
    sd_inter  = sd(Y_max - Y_mean,  na.rm = TRUE),
    X_norm    = first(X_norm),
    .groups = "drop"
  )

# Visualisation de la variabilité entre opérateur
plot_var <- ggplot(var_inter_point, aes(x = X_norm, y = var_inter, color = id)) +
  geom_line(linewidth = 1) +
  theme_minimal()
plot_var

# Calcul variabilité par poisson

VIO_par_poisson <- var_inter_point %>%
  group_by(id) %>%
  summarise(
    VIO_mean = mean(var_inter, na.rm = TRUE),
    VIO_max  = max(var_inter,  na.rm = TRUE),
    .groups = "drop"
  )

# # //// IV. GAM /// ####

# Formatage des données
str(datamaxabs)
datamaxabs <- datamaxabs %>% mutate(observateur = as.factor(observateur))
datamaxabs <- datamaxabs %>% mutate(id = as.factor(id))

# Logique hierarchique : 

# modèle null 
m0 <- gam(Y_max ~ s(X_norm), method = "REML", data = datamaxabs)
summary(m0) # R2 = 0,872

# modèle par individu  
m1 <- gam(Y_max ~ s(X_norm, by = id), method = "REML", data = datamaxabs)
summary(m1) # R2 = 0,92

# modèle par individu + effet observateur sur pente : Est ce que la forme de la courbe change selon l'observateur ? 
m2 <- gam(Y_max ~ s(X_norm, by = id) + s(X_norm, by = observateur), method = "REML", data = datamaxabs)
summary(m2) 

gam.vcomp(m2)

# modèle additif avec effets aléatoires individu et observateur : Quelle part de la variabilité totale est due aux observateurs ?
m3 <- gam(Y_max ~ s(X_norm) + s(id, bs = "re") + s(observateur, bs = "re"), method = "REML", data = datamaxabs)
summary(m3) 

gam.vcomp(m3)

v <- gam.vcomp(m3)
str(v)
std_dev <- v[, "std.dev"]

var_comp <- std_dev^2

# Proportion de variance expliquée 
prop <- var_comp / sum(var_comp)
prop

# Variance de Y expliquée par X = 98,6 %
# Variance inter-individus = 0,73 %
# Variance inter-observateurs = 0,043 % -> quasi négligeable
# Variance résiduelle (bruit non expliquée) = 0,65 % 

# "Les différences dues à l’observateur représentent moins de 0.05 % de la variance totale"

# //// VI. ETAPES STANDARDISATION MAT MET /// ####

# 
theme_standardisation_grid <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

coord_std <- coord_fixed(ratio = 1)

# Etape 1 : Brute
n_rows_VIO <- ceiling(n_poissons_VIO / n_cols_VIO)

# Plot de base
par(mfrow = c(1,1))

for (p in unique(data_VIO$id)) {
  
  subset_poisson <- data_VIO %>% 
    filter(id == "088")
  
  plot(subset_poisson$`X (cm)`, subset_poisson$`Y (cm)`,
       asp = 1, type = "n",
       main = paste("Poisson", "088"),
       xlab = "", ylab = "")
  
  for (op in unique(subset_poisson$observateur)) {
    
    subset_op <- subset_poisson %>% 
      filter(observateur == "EVA")
    
    n_VIO <- nrow(subset_op)
    n_traj_VIO <- n_VIO / 15
    
    for (i in 1:n_traj_VIO) {
      
      idx <- ((i - 1) * 15 + 1):(i * 15)
      
      lines(subset_op$`X (cm)`[idx],
            subset_op$`Y (cm)`[idx],
            col = "navy")
    }
  }
}

# Plot clean
etape1 <- ggplot(
  data_VIO %>% filter(id == "088", observateur == "EVA"),
  aes(x = `X (cm)`, y = `Y (cm)`,
      group = interaction(id, observateur, segment))
) +
  geom_path(color = "navy", linewidth = 0.7) +
  labs(x = NULL, y = NULL) +
  coord_std +
  theme_standardisation_grid

# Etape 2 : Translation X 

data_EVA_088 <- data_VIO %>% group_by(observateur, id, segment) %>% filter(observateur == "EVA", id == "088") %>%
  mutate(x_trans = `X (cm)` - `X (cm)`[trace == 15]) %>%
  ungroup()

# Plot de base 
ggplot(data = data_EVA_088, aes(x = x_trans, y = `Y (cm)`, group = interaction(id, observateur, segment))) +
  geom_line(col = "navy")

# Plot clean
etape2 <- ggplot(data_EVA_088,
                 aes(x = x_trans, y = `Y (cm)`,
                     group = interaction(id, observateur, segment))) +
  geom_path(color = "navy", linewidth = 0.7) +
  labs(x = NULL, y = NULL) +
  coord_std +
  theme_standardisation_grid

# Etape 3 : Translation en Y
# prendre le barycentre de plusieurs points (trace) (13, 14 et 15) pour faire la translation et mettre ce barycentre sur l'axe Y en 0 et le reste des points en fonction de ce barycentre sur l'axe des Y

barycentres_EVA_088 <- data_EVA_088 %>% filter(trace %in% c(13, 14, 15)) %>%
  group_by(observateur, id, segment) %>% summarise(
    bary_x = mean(`X (cm)`, na.rm = TRUE),
    bary_y = mean(`Y (cm)`, na.rm = TRUE),
    .groups = "drop")

# 2.2 Jointure pour récupérer les coordonnées des barycentres

data_EVA_088 <- data_EVA_088 %>% left_join(barycentres_EVA_088, by = c("observateur", "id", "segment"))

# 2.3 Translation sur Y : déplacer tous les points pour mettre le barycentre à Y=0

data_EVA_088 <- data_EVA_088 %>% mutate(y_trans = `Y (cm)` - bary_y)

# Plot de base 
ggplot(data = data_EVA_088, aes(x = x_trans, y = y_trans, group = interaction(observateur, id, segment))) +
  geom_line(alpha = 0.6, col = "navy") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot clean
etape3 <- ggplot(data_EVA_088,
                 aes(x = x_trans, y = y_trans,
                     group = interaction(id, observateur, segment))) +
  geom_path(color = "navy", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  coord_std +
  theme_standardisation_grid

# Etape 4 : Rotation 

# ici l'objectif c'est de prendre les points 13 14 15 d'un segment de regarder et de prendre l'angle par rapport au zéro et de remttre l'ensemble des angles des segment au meme angle et ensuite on fait une rotation sur 0 

angles_segments_EVA <- data_EVA_088 %>%
  filter(trace %in% c(12, 14)) %>%
  group_by(observateur, id, segment) %>%
  summarise(
    x_13 = x_trans[trace == 12],
    y_13 = y_trans[trace == 12],
    x_15 = x_trans[trace == 14],
    y_15 = y_trans[trace == 14],
    angle_rad = atan2(y_15 - y_13, x_15 - x_13),
    angle_deg = angle_rad * 180 / pi,
    .groups = "drop"
  )

data_EVA_088 <- data_EVA_088 %>%
  left_join(angles_segments_EVA %>% select(observateur, id, segment, angle_rad, angle_deg),
            by = c("observateur", "id", "segment")
  )

data_EVA_088 <- data_EVA_088 %>%
  mutate(x_rot = x_trans * cos(-angle_rad) - y_trans * sin(-angle_rad),
         y_rot = x_trans * sin(-angle_rad) + y_trans * cos(-angle_rad))

data_EVA_088 <- data_EVA_088 %>%
  arrange(observateur, id, segment, x_rot)

# Plot de base
ggplot(data = data_EVA_088, aes(x = x_rot, y = y_rot, group = interaction(observateur, id, segment))) +
  geom_path(alpha = 0.5, col = "navy") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# Plot clean
etape4 <- ggplot(data_EVA_088,
                 aes(x = x_rot, y = y_rot,
                     group = interaction(id, observateur, segment))) +
  geom_path(color = "navy", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  coord_std +
  theme_standardisation_grid

# Etape 5 : Normalisation taille et recolle sur X

# conversion en cm 

data_EVA_088 <- data_EVA_088 %>%
  group_by(observateur, id, segment) %>%
  mutate(
    # Longueur totale du segment (distance entre min et max de x_rot)
    longueur_segment = max(x_rot) - min(x_rot),
    # Normalisation X : de 0 à -1
    x_norm = (x_rot - min(x_rot) -1) / longueur_segment,
    # Normalisation Y : même facteur de division pour garder les proportions
    y_norm = y_rot / longueur_segment) %>%
  ungroup()

# recollage des x 15 sur 0 

data_EVA_088 <- data_EVA_088 %>% 
  group_by(observateur, id, segment) %>%
  mutate(X_xnorm = x_norm - x_norm[trace == 15]) %>%
  ungroup()

# Plot de base
ggplot(data = data_EVA_088, aes(x = X_xnorm, y = y_norm, group = interaction(observateur, id, segment))) +
  geom_line(alpha = 0.5, col = "navy") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  coord_fixed(ratio = 1)

# Plot clean
etape5 <- ggplot(data_EVA_088,
                 aes(x = X_xnorm, y = y_norm,
                     group = interaction(id, observateur, segment))) +
  geom_path(color = "navy", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  coord_std +
  theme_standardisation_grid

# Etape 6 : Valeurs absolues
# AVEC valeurs absolues

datamaxabs_EVA <- data_EVA_088 %>% 
  group_by(observateur, id, trace) %>%
  summarise(Y_max = max(abs(y_norm), na.rm = TRUE),
            X_norm = first(X_xnorm),
            segment = first(segment),
            observateur = first(observateur),
            .groups = "drop")

# Plot de base
abs_EVA <- ggplot(data = datamaxabs_EVA, aes(x = X_norm, y = Y_max, group = interaction(observateur, id, segment))) +
  geom_line(alpha = 0.5, col = "navy") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  coord_fixed(ratio = 1)

# Plot clean
etape6 <- ggplot(datamaxabs_EVA,
                 aes(x = X_norm, y = Y_max,
                     group = interaction(id, observateur))) +
  geom_line(color = "navy", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  coord_std +
  theme_standardisation_grid

# Graphiques en série 
etape1
etape2
etape3
etape4
etape5
etape6

library(patchwork)

(etape1 | etape2 | etape3) /
  (etape4 | etape5 | etape6)
