################################################################################
#################################### GIFS ######################################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2)
library(gganimate)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

fichiers_AM <- list.files("INPUT/RESULTATS/", pattern = "AM_M", full.names = TRUE)
fichiers_AV <- list.files("INPUT/RESULTATS/", pattern = "AV_M", full.names = TRUE)

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

######

x_lignes <- c(30, 50)
y_lignes <- c(15,25)

data_AM_gif <- data_AM %>%
  mutate(frame = segment)

data_AV_gif <- data_AV %>%
  mutate(frame = segment)

gif_AM <- ggplot(
  data_AM_gif,
  aes(`X (cm)`, `Y (cm)`,
      group = interaction(source, segment))) +
  geom_segment(x = -1,
    xend = 29,
    y = 15,
    yend = 15,
    colour = "white",
    linewidth = 0.1,
    linetype = "dashed") + 
  geom_segment(x = -1,
               xend = 29,
               y = 25,
               yend = 25,
               colour = "white",
               linewidth = 0.1,
               linetype = "dashed") + 
  geom_vline(
    xintercept = x_lignes,
    colour = "white",
    linewidth = 0.2, 
    linetype = "dashed") +
  annotate("rect",
           xmin = 31, xmax = 49,
           ymin = 15, ymax = 25,
           fill = "white",
           colour = "black",
           linewidth = 1,
           alpha = 0.9) +
  # trajectoires
  geom_path(
    linewidth = 0.8,
    colour = "red") +
  coord_equal(xlim = c(0, 80), ylim = c(0, 36)) +
  labs(
    title = "AM",
    subtitle = "Trace {closest_state}") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(
          fill = "grey30", colour = NA),
        plot.background = element_rect(
          fill = "grey30", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "white", size = 10),
        axis.title = element_text(color = "white", size = 11),
        axis.ticks = element_line(color = "white"),
        axis.line  = element_line(color = "white"),
        text = element_text(color = "white"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11)) +
  scale_y_continuous(limits = c(0,40)) +
  transition_states(
    frame,
    transition_length = 0,
    state_length = 1) + 
  annotate(
    "text",
    x = 40, y = 20,
    label = "BOITE",
    colour = "black",
    size = 6,
    fontface = "bold"
  ) +  
  annotate("text", x = 15, y = 2,
           label = "z3",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 40, y = 2,
           label = "z2",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 65, y = 2,
           label = "z1",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 15, y = 20,
           label = "z4",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 15, y = 40,
         label = "z3",
         colour = "white",
         size = 6,
         fontface = "bold")

animate(
  gif_AM,
  nframes = length(unique(data_AM_gif$frame)),
  fps = 5,
  width = 800,
  height = 500,
  renderer = gifski_renderer("AM_enveloppes_MOB.gif"))

gif_AV <- ggplot(
  data_AV_gif,
  aes(`X (cm)`, `Y (cm)`,
      group = interaction(source, segment))) +
  geom_vline(
    xintercept = x_lignes,
    colour = "white",
    linewidth = 0.2, 
    linetype = "dashed") +
  geom_segment(x = -1,
               xend = 29,
               y = 15,
               yend = 15,
               colour = "white",
               linewidth = 0.2,
               linetype = "dashed") + 
  geom_segment(x = -1,
               xend = 29,
               y = 25,
               yend = 25,
               colour = "white",
               linewidth = 0.2,
               linetype = "dashed") + 
  annotate("rect",
           xmin = 31, xmax = 49,
           ymin = 15, ymax = 25,
           fill = "white",
           colour = "black",
           linewidth = 1,
           alpha = 0.9) +
  # trajectoires
  geom_path(
    linewidth = 0.8,
    colour = "red") +
  coord_equal(xlim = c(0, 80), ylim = c(0, 36)) +
  labs(
    title = "AV",
    subtitle = "Trace {closest_state}") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(
          fill = "grey30", colour = NA),
        plot.background = element_rect(
          fill = "grey30", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "white", size = 10),
        axis.title = element_text(color = "white", size = 11),
        axis.ticks = element_line(color = "white"),
        axis.line  = element_line(color = "white"),
        text = element_text(color = "white"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11)) +
  transition_states(
    frame,
    transition_length = 0,
    state_length = 1) + 
  annotate(
    "text",
    x = 40, y = 20,
    label = "BOITE",
    colour = "black",
    size = 6,
    fontface = "bold"
  ) +  
  annotate("text", x = 15, y = 2,
           label = "z3",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 40, y = 2,
           label = "z2",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 65, y = 2,
           label = "z1",
           colour = "white",
           size = 6,
           fontface = "bold") + 
  annotate("text", x = 15, y = 20,
           label = "z4",
           colour = "white",
           size = 6,
           fontface = "bold")

animate(
  gif_AV,
  nframes = length(unique(data_AV_gif$frame)),
  fps = 5,
  width = 800,
  height = 500,
  renderer = gifski_renderer("AV_enveloppes.gif"))
