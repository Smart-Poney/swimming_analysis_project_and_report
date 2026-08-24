#
# 18/03/2026 - 02.angular_ratio_comparison.R
# 
# To calculate the angular ratio between head-tail (curvature) and compare it between each frame and fast-start
# to calcuate angle achieved by each segment (SGM) in each frame
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# use Maj + Ctrl + o to navigate
rm(list=setdiff(ls(), "RUN"))

# library: ----
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(patchwork)
library(mgcv)
source("R/coef_calculus.R")

# folder path ----
input_folder <- "data"
dir.create("output", showWarnings = T, recursive= T)
output_folder <- "output"

# list files ----
name <- file.path(output_folder, "coordinates.txt")
coordinates <- read.table(name, header=TRUE)

#
# TO CALCULATE ANGLES FOR N DIFFERENTS SEGMENTS (see Python_segmentation_algorithm)
# find the source for this info + estimate the number of segments needed and were to start and end each of them
# joints summary ----

joints <- read.csv("supplementary_data/joints_summary.csv", header = TRUE)
head(joints)
joints$nb_joints <- rowSums(!is.na(joints[, -1]))
mean_nb_joints <- mean(joints$nb_joints)
mean_nb_joints
col_means <- colMeans(joints[, -1], na.rm = TRUE)
mean_row <- data.frame(ID = "MEAN", t(col_means))
mean_row$nb_joints <- mean(joints$nb_joints)
joints_final <- rbind(mean_row, joints)
print(round(col_means, 2), digits=1)
mean_joint <- round(col_means, digits=0)
saveRDS(mean_joint, "output/mean_joint.rds") # sauvegarder pour PLOT.R

# we choose the maximum of segments recreated using SGM
# here we have 11 joints -> 12 segments and their mean position

# definition of the segments ----
seg1  <- coordinates[coordinates$pts  < mean_joint[1], ]
seg2  <- coordinates[coordinates$pts >= mean_joint[1]  & coordinates$pts < mean_joint[2], ]
seg3  <- coordinates[coordinates$pts >= mean_joint[2]  & coordinates$pts < mean_joint[3], ]
seg4  <- coordinates[coordinates$pts >= mean_joint[3]  & coordinates$pts < mean_joint[4], ]
seg5  <- coordinates[coordinates$pts >= mean_joint[4]  & coordinates$pts < mean_joint[5], ]
seg6  <- coordinates[coordinates$pts >= mean_joint[5]  & coordinates$pts < mean_joint[6], ]
seg7  <- coordinates[coordinates$pts >= mean_joint[6]  & coordinates$pts < mean_joint[7], ]
seg8  <- coordinates[coordinates$pts >= mean_joint[7]  & coordinates$pts < mean_joint[8], ]
seg9  <- coordinates[coordinates$pts >= mean_joint[8]  & coordinates$pts < mean_joint[9], ]
seg10 <- coordinates[coordinates$pts >= mean_joint[9]  & coordinates$pts < mean_joint[10], , ]
seg11 <- coordinates[coordinates$pts >= mean_joint[10] & coordinates$pts < mean_joint[11], ]
seg12 <- coordinates[coordinates$pts >= mean_joint[11], ]
sum(is.na(seg3))

# we calculate the angle achieved by each one for each frame 'coef_calculus.R'
seg1 <- coef_calculus(seg1)
seg2 <- coef_calculus(seg2)
seg3 <- coef_calculus(seg3)
seg4 <- coef_calculus(seg4)
seg5 <- coef_calculus(seg5)
seg6 <- coef_calculus(seg6)
seg7 <- coef_calculus(seg7)
seg8 <- coef_calculus(seg8)
seg9 <- coef_calculus(seg9)
seg10 <- coef_calculus(seg10)
seg11 <- coef_calculus(seg11)
seg12 <- coef_calculus(seg12)
sum(is.na(seg3))

# we test if everything went as expected with one individual 'f'
f <- "002_fs1"
dftest <- rbind(seg1[seg1$fs == f,], seg2[seg2$fs == f,], seg3[seg3$fs == f,],
                seg4[seg4$fs == f,], seg5[seg5$fs == f,], seg6[seg6$fs == f,],
                seg7[seg7$fs == f,], seg8[seg8$fs == f,], seg9[seg9$fs == f,],
                seg10[seg10$fs == f,], seg11[seg11$fs == f,], seg12[seg12$fs == f,])
dftest <- dftest[order(dftest$fs, dftest$frame, dftest$pts), ] # dftest ne contient que le fast start 002_fs1
write.table(dftest, "output/dfsegmentedvisu.txt", row.names = FALSE) # sauvegarder pour PLOT.R
str(mean_joint)

# ENG - REALISE A PLOT TO BETTER VISUALIZE ANGLES AND SEGMENTS CALCULATED
# FR  - FAIRE UN PLOT POUR VISUALISER LES ANGLES ET LES SEGMENTS 
# plot angles and segments ----
#
# dataset <- dftest

graphics.off()
k <- 1 # we use the first frame, can be changed to verified each frame

# fish body midline
FishPlot <- function() {
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # premier plot avec une seule frame du premier fast start
       xlim = c(min(dftest$X[dftest$frame == k])-20 , (max(dftest$X[dftest$frame == k])+20)),
       ylim = c(min(dftest$Y[dftest$frame == k])-50 , (max(dftest$Y[dftest$frame == k])+50)),
       main = "Fish position in space (pixels), fs = 002_fs1, frame = 1", xlab = "", ylab="")
  points(x = dftest$X[1], y = dftest$Y[1], col="green", pch = 15, cex = 1.5)
  points(x = dftest$X[dftest$frame == k], y = dftest$Y[dftest$frame == k], pch = 1, lwd = 2, col ="black") # tout semble bien positionné
  legend("topright",legend = c("Head", "bodymidline"),col = c("green", "black"),
         pch = c(15, 1),pt.cex = c(1.5, 1),bty = "n")
}

# fish body midline + axes
FishSegPlot <- function() {
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # premier plot avec une seule frame du premier fast start
       xlim = c(min(dftest$X[dftest$frame == k])-20 , (max(dftest$X[dftest$frame == k])+20)),
       ylim = c(min(dftest$Y[dftest$frame == k])-50 , (max(dftest$Y[dftest$frame == k])+50)),
       main = "Fish position in space (pixels) with correct angles for the 12 segments, fs = 002_fs1, frame = 1", , xlab = "", ylab="")
  colors <- rainbow(12)
  for (i in 1:12) {
    
    j <- (mean_joint[i] - 1) # par exemple, premier segment, on prend la position du premier joint -1, donc point n°27, on fait passer une abline avec l'angle calculé par ce point
    theta_deg <- dftest$angle[j]
    theta <- theta_deg * pi / 180
    X <- dftest$X[j]
    Y <- dftest$Y[j]
    m <- tan(theta)
    abline(a = Y - m*X, b = m, col = colors[i], lwd = 2)
    
  }
  points(x = dftest$X[1], y = dftest$Y[1], col="green", pch = 15, cex = 1.5)
  points(x = dftest$X[dftest$frame == k], y = dftest$Y[dftest$frame == k], pch = 1, lwd = 2, col ="black") # tout semble bien positionné
  legend("bottomright",legend = c("Head", "body midline", paste("Segment", 1:12)),
         col = c("green", "black", colors), pch = c(15, 1, rep(NA,12)), pt.cex = c(1.5, 1), bty = "n",
         lwd=c(NA, NA, rep(2,12)))
}


# fish body midline segmented
SegPlot <- function() {
  colors <- rainbow(12)
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # visualiser seuelement les angles
       xlim = c(min(dftest$X[dftest$frame == k])-20 , (max(dftest$X[dftest$frame == k])+20)),
       ylim = c(min(dftest$Y[dftest$frame == k])-50 , (max(dftest$Y[dftest$frame == k])+50)),
       main = "Segments modelisation in space (pixels), fs = 002_fs1, frame = 1", , xlab = "", ylab="")
  points(x=dftest$X[c(1,27)],   y=dftest$Y[c(1,27)],     type = "l", col = colors[1],  lwd=2)
  points(x=dftest$X[c(28,53)],   y=dftest$Y[c(28,53)],   type = "l", col = colors[2],  lwd=2)
  points(x=dftest$X[c(54,76)],   y=dftest$Y[c(54,76)],   type = "l", col = colors[3],  lwd=2)
  points(x=dftest$X[c(77,98)],   y=dftest$Y[c(77,98)],   type = "l", col = colors[4],  lwd=2)
  points(x=dftest$X[c(99,119)],  y=dftest$Y[c(99,119)],  type = "l", col = colors[5],  lwd=2)
  points(x=dftest$X[c(120,139)], y=dftest$Y[c(120,139)], type = "l", col = colors[6],  lwd=2)
  points(x=dftest$X[c(140,156)], y=dftest$Y[c(140,156)], type = "l", col = colors[7],  lwd=2)
  points(x=dftest$X[c(157,170)], y=dftest$Y[c(157,170)], type = "l", col = colors[8],  lwd=2)
  points(x=dftest$X[c(171,179)], y=dftest$Y[c(171,179)], type = "l", col = colors[9],  lwd=2)
  points(x=dftest$X[c(180,185)], y=dftest$Y[c(180,185)], type = "l", col = colors[10], lwd=2)
  points(x=dftest$X[c(186,190)], y=dftest$Y[c(186,190)], type = "l", col = colors[10], lwd=2)
  points(x=dftest$X[c(191,200)], y=dftest$Y[c(191,200)], type = "l", col = colors[10], lwd=2)
  points(x=dftest$X[1],   y=dftest$Y[1], pch=18, col = "red",  cex=2)
  legend("topright",legend = c(paste("Segment", 1:12), "Head"),col = c(colors, "Red"),lwd = c(rep(2, 12), NA), bty = "n",cex = 0.8, pch = c(rep(NA,12), 18), pt.cex = 1.5)
  
}

FishPlot()
FishSegPlot()
SegPlot()

# ENG - CALCUL OF THE TIME RELATED METRICS USING CUMSUM(DIFF(ANGLE))
# FR  - CALCUL DES VARIABLES SENSIBLES AU TEMPS CUMSUM(DIFF(ANGLE))
# time-related metrics definition ----
#
# dataset <- df et dftest2

df <- rbind(seg1, seg2, seg3, seg4, seg5, seg6, seg7, seg8, seg9, seg10, seg11, seg12)
df <- df[order(df$fs, df$pts, df$frame), ]
df <- df[df$pts %in% c(1, mean_joint[1:11]), ]
table(df$pts) # on doit logiquemement avoir, un point et un angle par frame par fs
df <- df %>%
  mutate(seg = dplyr::recode(as.character(pts),
                      "1"   = "segment_01_pts_1-27",
                      "28"  = "segment_02_pts_28-53",
                      "54"  = "segment_03_pts_54-76",
                      "77"  = "segment_04_pts_77-98",
                      "99"  = "segment_05_pts_99-119",
                      "120" = "segment_06_pts_120-139",
                      "140" = "segment_07_pts_140-156",
                      "157" = "segment_08_pts_157-170",
                      "171" = "segment_09_pts_171-179",
                      "180" = "segment_10_pts_180-185",
                      "186" = "segment_11_pts_186-190",
                      "191" = "segment_12_pts_191-200",
))

head(df)
df <- df %>%
  group_by(fs, seg) %>% # abs(angle) permet de gérer que les angles peuvent être négatifs (-180/180) mais avoir un très petit décalage en terme de mouvement
  mutate (angle_diff = c(NA, diff(abs(angle)))) %>% # on rajoute un NA car impossible de calculer N-1 à t0
  mutate (angle_cum_diff = cumsum(abs(replace(angle_diff, is.na(angle_diff), 0)))) %>% # on rajoute replace() pour gérer le NA obtenu avec diff()
  ungroup()
write.table(df, "output/cumdiffseg.txt", row.names = FALSE) # sauvegarder pour PLOT.R

# angle calculated + difference along time + cumulated sum of difference along time for a single fast-start
AngleVarPlot <- function() {
  plot(df$angle[df$fs=="002_fs1"], main = "diff entre valeurs brutes, diff() et cumsum(diff()) pour un fast-start. 11 frames par segment",
       xlim=c(1,145), ylim=c(-190,600), ylab = "angle en degrés")
  points(df$angle_diff[df$fs=="002_fs1"], col="green")
  points(df$angle_cum_diff[df$fs=="002_fs1"], col="red")
  abline(v = c(1,12,23,34,45,56,67,78,89,100,111,122), col = "darkblue", lwd = 1, lty=2)
  abline(h = c(-180, 180), col = "grey", lty = 1)
  legend("topright", legend =c("angle (deg)", "diff(angle)", "cumsum(diff(angle))", "frame 1", "bornes (deg)"), pch=c(1,1,1,NA, NA), lty=c(NA,NA,NA,2, 1),  col = c("black", "green", "red", "darkblue", "black"))
  
}
AngleVarPlot()

# angle calculated + difference along time + cumulated sum of difference along time for a single fast-start and segment
FSAngleVarPlot <- function() {
  plot(df$angle[df$fs=="002_fs1" & df$seg == "segment_04_pts_77-98"] ~ df$frame[df$fs=="002_fs1" & df$seg == "segment_04_pts_77-98"],
       xlim=c(0,13), ylim=c(-180,300), type = "b",
       main = "diff entre valeurs brutes, diff() et cumsum(diff()) pour le 4e segment du fast-start 002_fs1",
       sub = "angle range values from 180 to -180 using atan2, diff correspond to diff(abs(angle)) to manage small movements that generates high differencies",
       xlab = "Frame", ylab = "Angle (deg)")
  points(df$angle_diff[df$fs=="002_fs1" & df$seg == "segment_04_pts_77-98"], col="green", type = "b")
  points(df$angle_cum_diff[df$fs=="002_fs1" & df$seg == "segment_04_pts_77-98"], col="red", type = "b")
  legend("topright", legend =c("angle (deg)", "diff(angle)", "cumsum(diff(angle))"), pch=1,  col = c("black", "green", "red"))
}
FSAngleVarPlot()

# cumulated sum of difference along time for a single fast-start
dftest2 <- df[df$fs=="002_fs1",]
FSAngleCumPlot <- ggplot(dftest2, aes(x=frame, y=angle_cum_diff, group = seg, color = seg)) +  
  geom_line() +
  ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps pour un fast start",
          subtitle = "decoupage en 12 segments donc 12 courbes différentes, fs=002_fs1")

# cumulated sum of difference along time for every fast-start
AngleCumPlot <- ggplot(df, aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  #facet_wrap(~fs) + # pour séparer par fs plus lisible
  scale_color_viridis_d() + # pour des couleurs plus propres
  theme_minimal() + # gère le thème
  ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps",
          subtitle = "decoupage en 12 segments donc 12 courbes différentes par fast-start (195*12 -> 2340 courbes)")

# separer entre les 5 premiers segments et les 5 derniers
p0 <- ggplot(df[df$pts < 120, ], aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 600)) +
  ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps pour les 5 premiers segments")
p100 <- ggplot(df[df$pts >= 120, ], aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 600)) +
  ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps pour les 7 derniers segments")
SepAngleCumPlot <- ggarrange(p0, p100, ncol=2, nrow=1) 

# test pour representer la densite
p120 <- ggplot(df[df$pts >= 120, ], aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) +
  geom_line(size = 0.7, alpha = 0.6) +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 600)) +
  labs(
    title = "Évolution cumulée de l'angle",
    subtitle = "7 derniers segments",
    x = "Frame",
    y = "Différence d'angle cumulé",
    color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )
# df$frame >= 7 car le fast-start le plus court a 7 frames et on souhaite voir la densité le plus tard possible (cumsum)
max_df <- df %>% # permet de créer un data set avec seuelemnt les valeurs max de angle_cum_diff, une valeur par fs, par segment
  group_by(fs) %>%
  filter(frame == max(frame)) %>%
  ungroup() 

dens <- ggplot(max_df[max_df$pts >= 120, ], aes(x = angle_cum_diff, fill = factor(seg))) + 
  geom_density(alpha = 0.5) +
  coord_flip(xlim = c(0, 600)) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )
 

p120 <- p120 + theme(plot.margin = margin(5, 0, 5, 5))
dens <- dens + theme(plot.margin = margin(5, 5, 5, 0), legend.position = "none")
AngleDensPlot <- p120 + dens + 
  plot_layout(widths = c(4, 1), guides = "collect") & theme(legend.position = "right")

FSAngleCumPlot
AngleCumPlot
SepAngleCumPlot
AngleDensPlot

# ENG - REALISE A GRAPH WITH THE TREATMENT AS FACTOR 
# FR  - FAIRE UN GRAPHE AVEC LE TRAITEMENT
# graph with treatment ----
#
# dataset <- df2


treat <- readxl::read_excel("supplementary_data/traitement.xlsx")
df2 <- df %>%
  left_join(treat, by = c("poisson" = "ID"))
write.table(df2, "output/cumdiffseg2.txt", row.names = FALSE) # sauvegarder pour PLOT.R

max_df <- max_df %>%
  left_join(treat, by = c("poisson" = "ID"))
write.table(max_df, "output/max_cumdiffseg.txt", row.names = FALSE) # sauvegarder pour PLOT.R

cols <- scales::viridis_pal()(length(unique(df2$traitement)))
names(cols) <- unique(df2$traitement)
xlim <- range(df$frame, na.rm = TRUE)
ylim <- range(df$angle_cum_diff, na.rm = TRUE)

ptreat <- ggplot(df2, aes(x = frame, y = angle_cum_diff, group = interaction(fs, pts), color = traitement)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  scale_color_manual(values = cols) + # pour des couleurs plus propres
  labs(
    title = "Évolution cumulée de l'angle en fonction du traitement",
    x = "Frame",
    y = "Difference d'angle cumulée",
    color = "Segment"
  ) +
  theme_minimal() # gère le thème

dens1 <- ggplot(max_df, aes(x = frame, fill = traitement)) + 
  geom_boxplot(alpha = 0.4, data=max_df) + # faire un df avec les données(max) finales par individu <- max_df
  scale_fill_manual(values = cols) +
  theme_void() + 
  theme(legend.position = "none")

dens2 <- ggplot(max_df, aes(x = angle_cum_diff, fill = traitement)) + 
  #geom_density(alpha = 0.4, data=dftest) + # lisser ?
  stat_density(bw=15, trim=F) + # standardiser pas même effectifs
  scale_fill_manual(values = cols) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

ptreat <- ptreat +
  coord_cartesian(xlim = xlim, ylim = ylim)
dens1 <- dens1 + 
  coord_cartesian(xlim = xlim)
dens2 <- dens2 +
  coord_flip(xlim = ylim)

AngleTreatPlot <- dens1 + patchwork::plot_spacer() + ptreat + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4), guides = "collect") & theme(legend.position = "right")
  

AngleTreatPlot
summary(max_df$frame[max_df$traitement=="amont"]) # les boxplots sont bien positionnés
summary(max_df$frame[max_df$traitement=="aval"])

# EXAMPLE WITH A SINGLE FISH
# graph with a single fish ----
#
#dataset <- fish

fish <- df[df$poisson==27,]

pfish <- ggplot(fish, aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = fs)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  #scale_color_viridis_d() + # pour des couleurs plus propres
  labs(
    title = "Évolution cumulée de l'angle en fonction du fats-start pour le poisson 27",
    x = "Frame",
    y = "Difference d'angle cumulée",
  ) +
  theme_minimal() # gère le thème

dens3 <- ggplot(fish, aes(x = frame, fill = fs)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none")

dens4 <- ggplot(fish, aes(x = angle_cum_diff, fill = fs)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

AngleFSFishPlot <- dens3 + patchwork::plot_spacer() + pfish + dens4 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

AngleFSFishPlot

# EXAMPLE WITH A SINGLE SEGMENT
# graph with a single segment ----
#
# dataset <- segmented
segmented <- df2[df2$seg=="segment_04_pts_77-98",]

ptreat <- ggplot(segmented, aes(x = frame, y = angle_cum_diff, group = interaction(poisson, fs), color = traitement)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  scale_color_viridis_d() + # pour des couleurs plus propres
  geom_point() +
  theme_minimal() # gère le thème

dens1 <- ggplot(segmented, aes(x = frame, fill = traitement)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none")

dens2 <- ggplot(segmented, aes(x = angle_cum_diff, fill = traitement)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

dens1 + patchwork::plot_spacer() + ptreat + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

# ENG - TO CALCULATE THE CURVATURE RATIO BETWEEN EXTREM PARTS SNOUT (FIRST 40 POINTS) AND TAIL (LAST 40 POINTS) per frame
# FR  - CALCULER LE RATIO DE COURBURE ENTRE LES EXTREMITES NEZ (40 PREMIERS POINTS) ET LA QUEUE (40 DERNIERS POINTS) par image
# curvature ratio snout/tail ----
#
# dataset <- snout et tail et coef

# coordinates separation into 1/5 and 5/5
snout <- coordinates[coordinates$pts <= 40, ]
tail <- coordinates[coordinates$pts >= 161, ]
sum(is.na(snout))

# coefficeient calculus for the 40 first points and 40 last points
snout <- coef_calculus(snout)
tail <- coef_calculus(tail)
sum(is.na(snout))

NA_table <- rbind(snout[is.na(snout$angle), ], tail[is.na(tail$angle), ]) # besoin de gérer les valeurs de X constante sur 40 points
table(NA_table$fs) #pb avec 5 fast start dont la ligne est stricte verticale (X constant), soit angle = 0°

snout_coef <- snout[snout$pts == 1, ]
snout_coef <- snout_coef[ , c("poisson", "fs", "frame", "angle")]
snout_coef$part <- "snout"

tail_coef <- tail[tail$pts == 161, ]
tail_coef <- tail_coef[ , c("poisson", "fs", "frame", "angle")]
tail_coef$part <- "tail"

coef <- rbind(snout_coef, tail_coef)
sum(is.na(coef))

coef <- coef %>%
  pivot_wider(
    names_from = part,
    values_from = angle
  ) %>%
  mutate(
    ratio = angle_diff(snout, tail)
  )

coef <- coef[ ,c("poisson", "fs", "frame", "ratio")]

treatment <- read_excel("supplementary_data/traitement.xlsx")
colnames(treatment) <- c("poisson", "traitement")
coef$poisson <- as.numeric(coef$poisson)
coef <- coef %>%
  left_join(treatment, by = "poisson")
coef$poisson <- as.factor(coef$poisson)
coef$fs <- as.factor(coef$fs)
coef$frame <- as.factor(coef$frame)
coef$traitement <- as.factor(coef$traitement)
colnames(coef) <- c("poisson", "fs", "frame", "ratio", "treatment")
coef <- coef[ ,c("treatment", "poisson", "fs", "frame", "ratio")]
summary(coef)
str(coef)

# visualiser
FSRatioSnoutTailPlot <- function () {
  ghjk <- coordinates[coordinates$fs=="002_fs1" & coordinates$frame==1,]
  plot(ghjk$X,ghjk$Y, main = paste0("ratio = ", round(coef$ratio[1],2), 
                                    ", snout angle = ",round(snout$angle[1],2),
                                    ", tail angle = ", round(tail$angle[1],2), 
                                    ". Angles calculated from X axis using atan2()"))
  theta_deg <- snout$angle[1]
  theta <- theta_deg * pi / 180
  X <- snout$X[1]
  Y <- snout$Y[1]
  m <- tan(theta)
  abline(a = Y - m*X, b = m, col = "red", lwd = 2)
  theta_deg2 <- tail$angle[1]
  theta2 <- theta_deg2 * pi / 180
  X2 <- tail$X[1]
  Y2 <- tail$Y[1]
  m2 <- tan(theta2)
  abline(a = Y2 - m2*X2, b = m2, col = "green", lwd = 2)
  coef$ratio[1]
}

RatioSnoutTailPlot <- ggplot(coef, aes(x = frame, y = ratio, group = fs, color = treatment)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  #facet_wrap(~fs) + # pour séparer par fs plus lisible
  scale_color_viridis_d() + # pour des couleurs plus propres
  theme_minimal() + # gère le thème
  ggtitle("Evolution du ratio entre angle de la tête (0-40) et de la queue (161-200) au cours du temps",
          subtitle = "plus le ratio est faible, plus les segements sont parralèles, forme de S ou non-courbé" )
summary(coef)
boxplot(coef$ratio, coef$treatment)

FSRatioSnoutTailPlot()
RatioSnoutTailPlot

write.table(snout, "output/snout.txt", row.names = FALSE) # sauvegarder pour PLOT.R
write.table(tail, "output/tail.txt", row.names = FALSE) # sauvegarder pour PLOT.R
write.table(coef, "output/coefPLOT.txt", row.names = FALSE) # sauvegarder pour PLOT.R

# ENG - TO CALCULATE THE CURVATURE RATIO BETWEEN FIRST BODY PART ANTERIOR (FIRST 100 POINTS) AND LAST BODY PART POSTERIOR (LAST 100 POINTS) per frame
# FR  - CALCULER LE RATIO DE COURBURE ENTRE LES DEUX PARTIES DU CORPS ANTERIEURE (100 PREMIERS POINTS) ET POSTERIEURE (100 DERNIERS POINTS) par image
# curvature ratio head/tail ----
#
# dataset <- ant et post et coef2

# coordinates sepration into 1/2 and 2/2
ant <- coordinates[coordinates$pts <= 100, ]
post <- coordinates[coordinates$pts >= 101, ]
sum(is.na(ant))

# coefficeient calculus for the 100 first points and 100 last points
ant <- coef_calculus(ant)
post <- coef_calculus(post)
sum(is.na(ant))

NA_table <- rbind(ant[is.na(ant$angle), ], post[is.na(post$angle), ]) # besoin de gérer les valeurs de X constante sur 40 points
table(NA_table$fs) #pb avec 5 fast start dont la ligne est stricte verticale (X constant), soit angle = 0°

ant_coef <- ant[ant$pts == 1, ]
ant_coef <- ant_coef[ , c("poisson", "fs", "frame", "angle")]
ant_coef$part <- "ant"

post_coef <- post[post$pts == 101, ]
post_coef <- post_coef[ , c("poisson", "fs", "frame", "angle")]
post_coef$part <- "post"

coef2 <- rbind(ant_coef, post_coef)
sum(is.na(coef2))

coef2 <- coef2 %>%
  pivot_wider(
    names_from = part,
    values_from = angle
  ) %>%
  mutate(
    ratio = angle_diff(ant, post)
  )

coef2 <- coef2[ ,c("poisson", "fs", "frame", "ratio")]

treatment <- read_excel("supplementary_data/traitement.xlsx")
colnames(treatment) <- c("poisson", "traitement")
coef2$poisson <- as.numeric(coef2$poisson)
coef2 <- coef2 %>%
  left_join(treatment, by = "poisson")
coef2$poisson <- as.factor(coef2$poisson)
coef2$fs <- as.factor(coef2$fs)
coef2$frame <- as.factor(coef2$frame)
coef2$traitement <- as.factor(coef2$traitement)
colnames(coef2) <- c("poisson", "fs", "frame", "ratio", "treatment")
coef2 <- coef2[ ,c("treatment", "poisson", "fs", "frame", "ratio")]
summary(coef2)
str(coef2)

# visualiser
FSRatioAntPostPlot <- function () {
  ghjk <- coordinates[coordinates$fs=="002_fs1" & coordinates$frame==1,]
  plot(ghjk$X,ghjk$Y, main = paste0("ratio = ", round(coef2$ratio[1],2), 
                                    ", Anterior angle = ",round(ant$angle[1],2),
                                    ", posterior angle = ", round(post$angle[1],2), 
                                    ". Angles calculated from X axis using atan2()"))
  theta_deg <- ant$angle[1]
  theta <- theta_deg * pi / 180
  X <- ant$X[1]
  Y <- ant$Y[1]
  m <- tan(theta)
  abline(a = Y - m*X, b = m, col = "red", lwd = 2)
  theta_deg2 <- post$angle[1]
  theta2 <- theta_deg2 * pi / 180
  X2 <- post$X[1]
  Y2 <- post$Y[1]
  m2 <- tan(theta2)
  abline(a = Y2 - m2*X2, b = m2, col = "green", lwd = 2)
  coef2$ratio[1]
}

RatioAntPostPlot <- ggplot(coef2, aes(x = frame, y = ratio, group = fs, color = treatment)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
  geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
  #facet_wrap(~fs) + # pour séparer par fs plus lisible
  scale_color_viridis_d() + # pour des couleurs plus propres
  theme_minimal() + # gère le thème
  ggtitle("Evolution du ratio entre angle partie antérieure (0-100) et partie postérieure (101-200) au cours du temps",
          subtitle = "plus le ratio est faible, plus les segements sont parralèles, forme de S ou non-courbé" )
summary(coef2)

FSRatioAntPostPlot()
RatioAntPostPlot

# save
write.table(ant, "output/ant.txt", row.names = FALSE) # sauvegarder pour PLOT.R
write.table(post, "output/post.txt", row.names = FALSE) # sauvegarder pour PLOT.R
write.table(coef2, "output/coefPLOT2.txt", row.names = FALSE) # sauvegarder pour PLOT.R


# TEST ANOVA to detect a possible difference between treatment groups
# test ANOVA ---- 
#
# dataset <- df_wide

df_wide <- df2 %>%
  dplyr::select(fs, frame, pts, angle_cum_diff) %>%
  pivot_wider(
    names_from = pts,
    values_from = angle_cum_diff
  )
names(df_wide)[-(1:2)] <- paste0("cumsumdiff_angle_", names(df_wide)[-(1:2)])
coef <- coef %>%
  mutate(
    frame = as.numeric(as.character(frame)),
    fs = as.character(fs)
  )
angular <- coef %>%
  left_join(df_wide, by = c("fs", "frame"))
summary(angular)
cor(angular$ratio, angular$`cumsumdiff_angle_1`, use="complete.obs")

hist(coef$ratio)
shapiro.test(coef$ratio) # shapiro sensible, pas adapté aux grands échantillons, W = 0.9912, p-value = 4.05e-11
bartlett.test(ratio ~ treatment, data=coef) # homoscédasticité ok p-value = 0.1013
boxplot(ratio~treatment, data=coef)
boxplot(ratio~poisson, data=coef)

model <- lm(ratio ~ treatment + poisson + fs +  frame, data=coef, na.action=na.omit)
aov <- anova(model)
#aov <- anova(lm(ratio ~ treatment * poisson * fs * frame, data=coef, na.action=na.omit))   trop lourd
aov 
# aov semble montrer un effet significatif du traitement, (et fs et poisson, logiquement) mais aucune analyse des interactions
# treatment est significatif mais Sum Sq << face à la variance expliquée par poisson ou fs
par(mfrow=c(2,2))
plot(model)
dev.off()
qqnorm(residuals(model))
qqline(residuals(model)) # noramlité des résidus ok
plot(model, 1) # homoscédasticité des résidus ok

#
# list of the plots ----
#

dev.off()
FishPlot()
FishSegPlot()
SegPlot()
AngleVarPlot()
FSAngleCumPlot
AngleCumPlot
SepAngleCumPlot
AngleDensPlot
AngleTreatPlot
AngleFSFishPlot
FSRatioSnoutTailPlot()
RatioSnoutTailPlot
FSRatioAntPostPlot()
RatioAntPostPlot

#
# SAVE ----
#

coef <- coef %>%
  dplyr::select(treatment, poisson, fs, frame, ratio) %>%
  rename( ratio_snout_tail = ratio ) %>%
  mutate( ratio_ant_post = coef2$ratio)

write.table(coef, "output/curve_ratio.txt", row.names = FALSE)  
write.table(df2, "output/segments.txt", row.names=FALSE)


## FIN SCRIPT 02_angular_ratio_comparison.R

