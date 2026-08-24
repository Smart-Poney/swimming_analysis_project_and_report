#
# 22/04/2026 - 03.acceleration_distance_position_metrics.R
# 
# calculate time-position related metrics: distance, acceleration, efficiency, speed
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))

# library: ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(zoo)

# folder path ----
input_folder <- "data"
dir.create("output", showWarnings = T, recursive= T)
output_folder <- "output"

# list files ----
name <- file.path(output_folder, "coordinates.txt")
coordinates <- read.table(name, header=TRUE)
str(coordinates)
coor <- coordinates %>%
  mutate(
    frame = as.factor(frame),
    fs = as.factor(fs)
  )
length(unique(coor$fs))
length(unique(coor$poisson))

# eng:
# for Salmo trutta, CM is defined as the most anterior part of the visible dorsal fin (Vignon & Aymes, 2020)
# for the rainbow trout, (Webb, 1976) consider it as 38% of the length of the streched fish
# since we only have the midline coordinates, we should consider a percentage of the total lenght maybe linked with the individual weight ?
# WE HAVE 200 EQUALLY DISTANT POINTS, THEREFORE 38% SHOULD CORRESPOND TO THE 76th POINTS !! thanks Lili
# however as Vignon & Aymes mentionned, the midline is rarely with streched fishes so this could induce bias error calcul

# fr:
# le principe de ces calculs repose sur une approximation du CM à 38% de la longueur totale du poisson étiré Webb, 1978
# puisque c'est un ratio on considérera toujours le point 76 comme le CM pour faciliter les calculs
# rappel: le CM correspond au point où s'applique les forces de mouvement pendant le fast-start
# l'echelle sur la vidéo est d'environ 17.6 pixels/cm -> 1px vaut 0.057cm


#
# CALCUL OF DISTANCE ACROSS FRAME
# distance ----
#

dftest <- coor[coor$fs == "002_fs1",] # 200 pts, 11 frames
dfCMtest <- coor[coor$fs == "002_fs1" & coor$pts == 76,]
dfCM <- coor[coor$pts == 76, ] # determination of the CM position
plot(dfCMtest$X ~ dfCMtest$frame)
plot(dfCMtest$Y ~ dfCMtest$frame)

ggplot(dfCMtest, aes(x = frame)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = Y, color = "Y")) +
  labs(y = "Coordonnées", color = "Variable") +
  theme_minimal()

ggplot(dfCMtest, aes(x = X, y = Y, color=frame)) +
  geom_point() +
  geom_path(size = 1) +
  coord_equal() +
  theme_minimal()

ggplot(dfCM, aes(x = X, y = Y, color = fs)) +
  geom_path(size = 0.8, show.legend = FALSE, alpha=0.5) +
  coord_equal() +
  theme_minimal()

ggplot(dfCMtest, aes(x = X, y = Y)) +
  geom_point() +
  transition_time(as.numeric(frame))

# test avec dfCMtest un seul fs
d_tot <- sqrt((dfCMtest$X[11]-dfCMtest$X[1])^2 + (dfCMtest$Y[11]-dfCMtest$Y[1])^2)
dfCMtest$d_travelled <- sqrt(
  (dfCMtest$X - dplyr::lag(dfCMtest$X))^2 +
    (dfCMtest$Y - dplyr::lag(dfCMtest$Y))^2
)
dfCMtest$d_travelled[is.na(dfCMtest$d_travelled)] <- 0
d_real <- sum(dfCMtest$d_travelled, na.rm = TRUE)
d_eff <- d_tot - d_real

# calcul pour tout les fs
dfCM <- dfCM %>%
  arrange(fs, frame) %>%
  group_by(fs) %>%
  mutate(
    d_travelled = sqrt((X - lag(X))^2 + (Y - lag(Y))^2), # calcul de la distance parcourue entre chaque point
    d_travelled = tidyr::replace_na(d_travelled, 0), # remplacer les points de departs NA en 0
    d_cum = cumsum(d_travelled), # calculer la somme cumulé de la distance parcourue entre chaque point
    d_tot = sqrt( (last(X) - first(X))^2 + (last(Y) - first(Y))^2 ) # calculer la distance parcourue effective entre premier point et dernier point
  ) %>%
  ungroup()

#
# FAIRE UN GRAPHE DE d_cum AVEC TRAITEMENT
## visualisation ----
#

dfCM$d_travelled_cm <- dfCM$d_travelled*0.057 # conversion en cm 
dfCM$d_cum_cm <- dfCM$d_cum*0.057 # conversion en cm
dfCM$d_tot_cm <- dfCM$d_tot*0.057 # conversion en cm
dfCM$time <- as.numeric(dfCM$frame)*0.0083 # conversion en s
treat <- readxl::read_excel("supplementary_data/traitement.xlsx")
dfCM <- dfCM %>%
  left_join(treat, by = c("poisson" = "ID")) # ajout de la variable traitement

ggplot(dfCM, aes(x = time, y = d_cum_cm, group = fs, color = traitement)) + 
  geom_line(size = 0.8, alpha = 0.4) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  geom_smooth(aes(group = traitement), se = FALSE, size = 1.2) + # lisser
  stat_summary(aes(group = traitement), fun = mean, geom = "line", linetype="longdash", size = 1) +
  scale_color_manual(values = c("amont" = "#1b9e77", "aval" = "#d95f02")) +
  theme_minimal() + # gère le thème
  ylab("cm parcouru") +
  xlab("temps en s (lag de 0.008s)") +
  ggtitle("Evolution de la somme cumulée de la distance parcourue au cours du temps",
          subtitle = "195 fast-start donc 195 courbes")

#
# CALCUL OF EFFICIENCY ACROSS FRAME
# efficiency ----
# is the fish efficient to go straight from point A to B or does he use way longer path ?
#

dfeff <- dfCM %>%
  select(traitement, poisson, fs, time, d_cum_cm, d_tot_cm) %>%
  group_by(fs) %>%
  slice_tail(n = 1) %>%
  mutate(efficiency = d_tot_cm / d_cum_cm) %>%
  ungroup()
summary(dfeff)
hist(dfeff$efficiency)

ggplot(dfeff, aes(x = as.numeric(fs), y = efficiency, color = traitement)) + 
  geom_point() +
  geom_smooth( data=dfeff[dfeff$traitement=="aval",] , method = "lm", se = TRUE,color = "blue") +
  geom_smooth( data=dfeff[dfeff$traitement=="amont",] , method = "lm", se = TRUE,color = "orange") +
  theme_minimal() + # gère le thème
  ylab("efficiency") +
  xlab("fast-start (ordre importe peu)") +
  ggtitle("Efficacite par poisson par fast start",
          subtitle = "195 fast-start donc 195 points")

#
# CALCUL OF SPEED ACROSS FRAME
# speed ----
#

dfspeed <- dfCM %>%
  select(traitement, poisson, fs, time, d_travelled_cm) %>%
  arrange(fs, time) %>%
  group_by(fs) %>%
  mutate(
    dt = time - lag(time),
    speed = d_travelled_cm / dt,
    speed = replace_na(speed, 0)
  ) %>%
  ungroup()
plot(dfspeed$speed[dfspeed$fs=="002_fs1"] ~ dfspeed$time[dfspeed$fs=="002_fs1"])

ggplot(dfspeed, aes(x = time, y = speed, color = traitement)) + 
  geom_point() +
  geom_smooth( data=dfspeed[dfspeed$traitement=="aval",] , method = "lm", se = TRUE) +
  geom_smooth( data=dfspeed[dfspeed$traitement=="amont",] , method = "lm", se = TRUE) +
  theme_minimal() + # gère le thème
  ylab("speed") +
  xlab("fast-start (ordre importe peu)") +
  ggtitle("Vitesse par poisson par fast start",
          subtitle = "195 fast-start donc 195 points")


#
# CALCUL OF ACCELERATION ACROSS FRAME
# accelereation ----
#

dfaccel <- dfCM %>%
  select(traitement, poisson, fs, time, d_travelled_cm) %>%
  arrange(fs, time) %>%
  group_by(fs) %>%
  mutate(
    time = time*1000, # pour passer en ms, plus de sens pour vitesse et accélération
    dt = time - lag(time), #dt constant ~0.0083s ou 1/120s
    speed = d_travelled_cm / dt,
  ) %>%
  mutate(
    accel = (speed - lag(speed)) / dt,
  ) %>%
  ungroup()

df_peak_acc <- dfaccel %>%
  group_by(fs) %>%
  slice_max(accel, n = 1, with_ties = FALSE) %>%
  ungroup()

df_summary <- dfaccel %>%
  group_by(fs) %>%
  summarise(
    acc_max = max(accel, na.rm = TRUE),
    acc_min = min(accel, na.rm = TRUE),
    time_acc_max = time[which.max(accel)],
    time_acc_min = time[which.min(accel)]
  )


plot(dfaccel$accel[dfaccel$fs=="002_fs1"] ~ dfaccel$time[dfaccel$fs=="002_fs1"], type="l")

ggplot(dfaccel, aes(x = time, y = accel, group=fs, color = traitement)) + 
  geom_line() +
  theme_minimal() + # gère le thème
  ylab("accel(cm/ms²") +
  xlab("temps en ms") +
  ggtitle("Accélération par poisson par fast start")

ggplot(dfaccel, aes(x = time, y = accel, color = traitement)) + 
  geom_point() +
  geom_smooth( data=dfaccel[dfaccel$traitement=="aval",] , method = "loess", se = T) +
  geom_smooth( data=dfaccel[dfaccel$traitement=="amont",] , method = "loess", se = T) +
  theme_minimal() + # gère le thème
  ylab("accel(cm/ms²)") +
  xlab("fast-start (ordre importe peu)") +
  ggtitle("Accélération par poisson par fast start")

ggplot(dfaccel, aes(x = time, y = accel, group = fs)) +
  geom_line(alpha = 0.2) +
  geom_point(data = df_peak_acc, color = "red", size = 2) +
  theme_minimal()

df_peak_acc %>%
  ggplot(aes(x = traitement, y = accel, fill = traitement)) +
  geom_boxplot() +
  theme_minimal()

write.table(dfCM, "output/dfCM.txt", row.names=FALSE)
write.table(dfeff, "output/dfeff.txt", row.names=FALSE)
write.table(dfaccel, "output/dfaccel.txt", row.names=FALSE)

## FIN SCRIPT 04_acceleration_distance_position_metrics.R

