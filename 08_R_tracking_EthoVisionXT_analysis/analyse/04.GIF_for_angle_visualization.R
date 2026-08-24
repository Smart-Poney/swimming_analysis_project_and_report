#
# 10/04/26 - 04.GIF_for_angle_visualization.R
#
# To validate the creation of angular metrics via GIF
#
# auteur: FG
# project: tracking_EthoVisionXT_analysis
# latest modification: 30/06/26
#

# Use Ctrl + Maj + o to navigate
to_not_erase <- c("cdn3","trk3","stats3", "head_angle", "tail_angle","curvature", "time_limit", "indiv", "RUN", "to_not_erase")
rm(list=setdiff(ls(), to_not_erase))
graphics.off()

# library
library(dplyr)
library(animation)
library(gganimate)
library(ggplot2)
source("R/coef_calculus.R")
source("R/ask.R")
source("R/moveGIF.R")

# ask time and individual ----
# ask the operator for time limit and individual choosen
if (!exists("time_limit")) {
  time_limit <- ask_time_limit(head_angle$time)
}

if (!exists("indiv")) {
  indiv <- ask_individual(head_angle$ID)
}
#time_limit <- ask_time_limit(head_angle$time)
#indiv <- ask_individual((head_angle$ID))
head_angle$ID <- as.factor(head_angle$ID)

# dataset ----
n <- nrow(head_angle[head_angle$ID == indiv & head_angle$time <= time_limit,])
sum(is.na(head_angle$X1[head_angle$ID == indiv]))
summary((head_angle[head_angle$ID == indiv,]))
head_df <- head_angle[!(head_angle$ID == indiv & is.na(head_angle$X1)), ] # pour enlever les lignes NA de coordonnées
tail_df <- tail_angle[!(tail_angle$ID == indiv & is.na(tail_angle$X1)), ]
head_df <- head_df[head_df$ID == indiv & head_df$time <= time_limit, ] # pour enlever trop de lignes
tail_df <- tail_df[tail_df$ID == indiv & tail_df$time <= time_limit, ]
Xmax <- max(head_df$X1[head_df$ID == indiv])
Xmin <- min(head_df$X1[head_df$ID == indiv])
Ymax <- max(head_df$Y1[head_df$ID == indiv])
Ymin <- min(head_df$Y1[head_df$ID == indiv])
index <- seq(1, nrow(head_df[head_df$ID == indiv & head_df$time <= time_limit, ]), by = 1) # pour éviter de sortir un GIF de 50 000 frames utiliser by = 100, avec by = 1 durée = 5min
name <- paste0(indiv, "_angle_visualisation_", time_limit, "s.gif")

# GIF creation ----
saveGIF({
  
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (k in index) {  
    
    # points et droite pour tête-centre
    plot(head_df$X1[k], head_df$Y1[k], xlim=c(Xmax + 10,Xmin - 10), ylim=c(Ymax + 10, Ymin - 10),
         main = paste("Step", k, "| Courbure =", round(curvature[k], 2), "deg"))
    points(head_df$X2[k], head_df$Y2[k])
    
    b1 <- (head_df$Y2[k] - head_df$Y1[k]) / (head_df$X2[k] - head_df$X1[k])
    a1 <- head_df$Y1[k] - b1 * head_df$X1[k]
    abline(a = a1, b = b1, col = "red")
    
    # segment
    segments(head_df$X1[k], head_df$Y1[k],
             head_df$X2[k], head_df$Y2[k],
             col = "green", lwd = 2)
    
    # points et droite pour centre-queue
    points(tail_df$X1[k], tail_df$Y1[k], xlim=c(Xmax + 5,Xmin - 5), ylim=c(Ymax + 5, Ymin - 5), main=c("step",k))
    points(tail_df$X2[k], tail_df$Y2[k])
    
    b1 <- (tail_df$Y1[k] - tail_df$Y2[k]) / (tail_df$X1[k] - tail_df$X2[k])
    a1 <- tail_df$Y2[k] - b1 * tail_df$X2[k]
    abline(a = a1, b = b1, col = "blue")
    
    # segment
    segments(tail_df$X2[k], tail_df$Y2[k],
             tail_df$X1[k], tail_df$Y1[k],
             col = "lightblue", lwd = 2)
    
    # vecteurs courbure
    v1x <- head_df$X1[k] - head_df$X2[k]
    v1y <- head_df$Y1[k] - head_df$Y2[k]
    v2x <- tail_df$X1[k] - tail_df$X2[k]
    v2y <- tail_df$Y1[k] - tail_df$Y2[k]
    
    scale <- 2 # normalisation
    arrows(head_df$X2[k], head_df$Y2[k],
           head_df$X2[k] + v1x*scale,
           head_df$Y2[k] + v1y*scale,
           col="grey30", length=0.1)
    arrows(head_df$X2[k], head_df$Y2[k],
           head_df$X2[k] + v2x*scale,
           head_df$Y2[k] + v2y*scale,
           col="grey30", length=0.1)
    text(x = head_df$X1[k],
         y = head_df$Y1[k] + 2,
         labels = paste0("angle = ", round(curvature[k], 2)),
         col = "black")
    
    # Legende
    legend("topright",
           legend = c("Centre","Tête","Queue","Segment tête-centre","Segment centre-queue",
                      "Droite tête-centre","Droite centre-queue","vecteurs (courbure)"),
           col = c("black","red","blue","green","lightblue",
                   "red","blue","grey30"),
           pch = c(16,16,16, NA, NA, NA, NA, NA),
           lty = c(NA,NA,NA, 1,1,1,1,1),
           lwd = c(NA,NA,NA, 2,2,1,1,1),
           bty = "n",
           cex = 0.8)
    
    setTxtProgressBar(pb, k)
  }
  close(pb)
  
}, movie.name = name, interval = 0.01) # 0.05s correspond à 20 fps ; 0.0083s correspond normalement à 120 fps mais limite de saveGIF() à 100


# test gganimate et ggplot 2 ----
library(ggplot2)
library(gganimate)

# 
# clean and sort ----
#
head_angle_clean <- head_angle %>%
  filter(!(ID == indiv & is.na(X1))) %>%
  filter(ID == indiv, time <= time_limit)

tail_angle_clean <- tail_angle %>%
  filter(!(ID == indiv & is.na(X1))) %>%
  filter(ID == indiv, time <= time_limit)

#
# alignement----
#
df <- head_angle_clean %>%
  inner_join(tail_angle_clean, by = c("ID", "time"),
             suffix = c("_head", "_tail"))
#
# curvature ----
#
angle_diff <- function(a1, a2) {
  d <- a2 - a1
  atan2(sin(d), cos(d))
}

df <- df %>%
  mutate(
    # joint = point commun head/tail
    joint_x = X2_head,
    joint_y = Y2_head,
    
    # angle joint -> tête
    angle_head_rad = atan2(
      Y1_tail - joint_y,
      X1_tail - joint_x
    ),
    
    # angle joint -> queue
    angle_tail_rad = atan2(
      Y1_head - joint_y,
      X1_head - joint_x
    ),
    
    curvature = angle_diff(angle_head_rad, angle_tail_rad),
    curvature_deg = abs(curvature * 180 / pi)
  )

#
# arcs ----
# 
make_arc <- function(x0, y0, theta1, theta2, r = 1, n = 30) {
  
  # normaliser angle
  d <- atan2(sin(theta2 - theta1), cos(theta2 - theta1))
  theta_seq <- seq(theta1, theta1 + d, length.out = n)
  
  data.frame(
    x = x0 + r * cos(theta_seq),
    y = y0 + r * sin(theta_seq)
  )
}

arcs <- df %>%
  rowwise() %>%
  do({
    arc <- make_arc(
      x0 = .$joint_x,
      y0 = .$joint_y,
      theta1 = .$angle_head_rad,
      theta2 = .$angle_tail_rad,
      r = 1.5
    )
    arc$time <- .$time
    arc
  }) %>%
  ungroup()

#
# limits -----
#


x_range <- c( range(c(df$X1_head, df$X2_head, df$X2_tail))[1]-10, range(c(df$X1_head, df$X2_head, df$X2_tail))[2]+10)
y_range <- c( range(c(df$Y1_head, df$Y2_head, df$Y2_tail))[1]-10, range(c(df$Y1_head, df$Y2_head, df$Y2_tail))[2]+10)

#
# plot ----
#
p <- ggplot(df) +
  
  # segments réels
  geom_segment(aes(x = X1_head, y = Y1_head,
                   xend = X2_head, yend = Y2_head),
               color = "green", linewidth = 1) +
  
  geom_segment(aes(x = X2_tail, y = Y2_tail,
                   xend = X1_tail, yend = Y1_tail),
               color = "lightblue", linewidth = 1) +

  
  # points
  geom_point(aes(x = X1_head, y = Y1_head), color = "black", size = 5) + # centre
  geom_point(aes(x = X1_tail, y = Y1_tail), color = "red", size = 5) +   # tête
  geom_point(aes(x = X2_tail, y = Y2_tail), color = "blue", size = 5) +  # queue
  
  # vecteurs courbure
  geom_segment(aes(
    x = X1_head - (X2_head - X1_head)*10,
    y = Y1_head - (Y2_head - Y1_head)*10,
    xend = X1_head + (X2_head - X1_head)*10,
    yend = Y1_head + (Y2_head - Y1_head)*10
  ),
  color = "red",
  alpha = 0.7) +
  
  geom_segment(aes(
    x = X1_tail - (X2_tail - X1_tail)*10,
    y = Y1_tail - (Y2_tail - Y1_tail)*10,
    xend = X1_tail + (X2_tail - X1_tail)*10,
    yend = Y1_tail + (Y2_tail - Y1_tail)*10
  ),
  color = "blue",
  alpha = 0.7) +
  
  # texte courbure
  geom_text(aes(
    x = X1_head,
    y = Y1_head + 2,
    label = paste0("curv=", round(curvature_deg, 1), "°")
  )) +
  
  # ajout des arcs
  geom_path(data = arcs,
            aes(x = x, y = y, group = interaction(time)),
            color = "purple", linewidth = 1) +
  
  # axes fixes
  coord_cartesian(xlim = x_range , ylim = y_range) +
  
  # titre
  labs(
    title = paste0("Individu ", indiv,
                   " | Temps: ",df$time," s",
                   " | Courbure: ",round(df$curvature_deg,1),"°")
  ) +
  
  theme_minimal()

#
# GIF creation ----
#
anim <- p +
  transition_time(time) +
  ease_aes('linear')

#
# export ----
#
name <- paste0(indiv, "_angle_visualisation_gganimate_", time_limit, "s.gif")

animate(anim,
        nframes = min(300, nrow(df)),  # limite sécurité
        fps = 20,
        width = 600,
        height = 600,
        renderer = gifski_renderer(name))

# déplacer les GIF
moveGIF() # deplace les GIFs du repertoire courant dans le sous dossier GIF

## END OF SCRIPT 04.GIF_for_angle_visualization.R
