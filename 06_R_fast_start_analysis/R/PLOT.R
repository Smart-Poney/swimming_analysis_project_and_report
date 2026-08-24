#' PLOT.R 
#'
#'contain useful plot function for the project, ggplot2, or plot, or more
#'should contain only the function and can be called throughout the script to visualize what we are working with
#'
#' dependencies :
#' library(ggplot2)
#' library(dplyr)
#' library(ggpubr)
#' library(patchwork)
#' library(viridis)
#' library(scales)
#'
#' @param taxon data-frames containing either coordinates for fish position in space/time, or time-dependant variables 
#'
#' @returns plot or ggplot for visualization
#' @export
#'
#' @examples
#' 
#' dftest <- read.table("output/dfsegmentedvisu.txt", header = TRUE)
#' mean_joint <- readRDS("output/mean_joint.rds")
#' df <- read.table("output/cumdiffseg.txt", header = TRUE)
#' FishPlot()
#' FishSegPlot()
#'

# load les corrects dataset
correct_load <- function() {
  assign("dftest", read.table("output/dfsegmentedvisu.txt", header = TRUE), envir = .GlobalEnv)
  assign("mean_joint", readRDS("output/mean_joint.rds"), envir = .GlobalEnv)
  assign("df", read.table("output/cumdiffseg.txt", header = TRUE), envir = .GlobalEnv)
  assign("max_df", read.table("output/max_cumdiffseg.txt", header = TRUE), envir = .GlobalEnv)
  assign("df2", read.table("output/cumdiffseg2.txt", header = TRUE), envir = .GlobalEnv)
  assign("coef", read.table("output/coefPLOT.txt", header = TRUE), envir = .GlobalEnv)
  assign("snout", read.table("output/snout.txt", header = TRUE), envir = .GlobalEnv)
  assign("tail", read.table("output/tail.txt", header = TRUE), envir = .GlobalEnv)
  assign("coef2", read.table("output/coefPLOT2.txt", header = TRUE), envir = .GlobalEnv)
  assign("ant", read.table("output/ant.txt", header = TRUE), envir = .GlobalEnv)
  assign("post", read.table("output/post.txt", header = TRUE), envir = .GlobalEnv)
  assign("coordinates", read.table("output/coordinates.txt", header = TRUE), envir = .GlobalEnv)
  assign("treat", read.table("output/originxtreatment.txt", header = TRUE), envir = .GlobalEnv)
  assign("segm", read.table("output/segm.txt", header = TRUE), envir = .GlobalEnv)
  assign("segm2", read.table("output/segm2.txt", header = TRUE), envir = .GlobalEnv)
  assign("dfcum", read.table("output/dfdcum.txt", header = TRUE), envir = .GlobalEnv)
  assign("dfcum2", read.table("output/dfdcum2.txt", header = TRUE), envir = .GlobalEnv)
  assign("dfa", read.table("output/dfa.txt", header = TRUE), envir = .GlobalEnv)
  assign("dfa2", read.table("output/dfa2.txt", header = TRUE), envir = .GlobalEnv)
  assign("dfe", read.table("output/dfefficiency.txt", header = TRUE), envir = .GlobalEnv)
  assign("snail", read.table("output/snail.txt", header = TRUE), envir = .GlobalEnv) # pose problème si dataset pas déjà existant
  assign("snail2", read.table("output/snail2.txt", header = TRUE), envir = .GlobalEnv)
  assign("antpost", read.table("output/antpost.txt", header = TRUE), envir = .GlobalEnv)
  assign("antpost2", read.table("output/antpost2.txt", header = TRUE), envir = .GlobalEnv)
  
}

# poisson seulement
FishPlot <- function(cloud) {
  k <- as.numeric(cloud)
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # premier plot avec une seule frame du premier fast start
       xlim = c(min(dftest$X[dftest$frame == k])-20 , (max(dftest$X[dftest$frame == k])+20)),
       ylim = c(min(dftest$Y[dftest$frame == k])-50 , (max(dftest$Y[dftest$frame == k])+50)),
       main = paste0("Fish position in space (pixels), fs = 002_fs1, frame = ", k), xlab = "", ylab="")
  points(x = dftest$X[dftest$frame == k][1], y = dftest$Y[dftest$frame == k][1], col="green", pch = 15, cex = 1.5)
  points(x = dftest$X[dftest$frame == k], y = dftest$Y[dftest$frame == k], pch = 1, lwd = 2, col ="black") # tout semble bien positionné
  legend("topright",legend = c("Head", "bodymidline"),col = c("green", "black"),
         pch = c(15, 1),pt.cex = c(1.5, 1),bty = "n")
}

# poisson et droites
FishSegPlot <- function(cloud) {
  k <- as.numeric(cloud)
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # premier plot avec une seule frame du premier fast start
       xlim = c(min(dftest$X[dftest$frame == k])-20 , (max(dftest$X[dftest$frame == k])+20)),
       ylim = c(min(dftest$Y[dftest$frame == k])-50 , (max(dftest$Y[dftest$frame == k])+50)),
       main = paste0("Fish position in space (pixels) with correct angles for the 12 segments, fs = 002_fs1, frame = ", k), , xlab = "", ylab="")
  colors <- rainbow(12)
  for (i in 1:12) {
    
    j <- (mean_joint[i] - 1) # par exemple, premier segment, on prend la position du premier joint -1, donc point n°27, on fait passer une abline avec l'angle calculé par ce point
    theta_deg <- dftest$angle[dftest$frame == k][j]
    theta <- theta_deg * pi / 180
    X <- dftest$X[dftest$frame == k][j]
    Y <- dftest$Y[dftest$frame == k][j]
    m <- tan(theta)
    abline(a = Y - m*X, b = m, col = colors[i], lwd = 2)
    
  }
  points(x = dftest$X[dftest$frame == k][1], y = dftest$Y[dftest$frame == k][1], col="green", pch = 15, cex = 1.5)
  points(x = dftest$X[dftest$frame == k], y = dftest$Y[dftest$frame == k], pch = 1, lwd = 2, col ="black") # tout semble bien positionné
  legend("bottomright",legend = c("Head", "body midline", paste("Segment", 1:12)),
         col = c("green", "black", colors), pch = c(15, 1, rep(NA,10)), pt.cex = c(1.5, 1), bty = "n",
         lwd=c(NA, NA, rep(2,12)))
}


# segments
SegPlot <- function(cloud) {
  k <- as.numeric(cloud)
  colors <- rainbow(12)
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # visualiser seuelement les angles
       xlim = c(min(dftest$X[dftest$frame == k])-20 , (max(dftest$X[dftest$frame == k])+20)),
       ylim = c(min(dftest$Y[dftest$frame == k])-50 , (max(dftest$Y[dftest$frame == k])+50)),
       main = paste0("Segments modelisation in space (pixels), fs = 002_fs1, frame = ", k), , xlab = "", ylab="")
  points(x=dftest$X[dftest$frame == k][c(1,27)],   y=dftest$Y[dftest$frame == k][c(1,27)],     type = "l", col = colors[1],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(28,53)],   y=dftest$Y[dftest$frame == k][c(28,53)],   type = "l", col = colors[2],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(54,76)],   y=dftest$Y[dftest$frame == k][c(54,76)],   type = "l", col = colors[3],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(77,98)],   y=dftest$Y[dftest$frame == k][c(77,98)],   type = "l", col = colors[4],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(99,119)],  y=dftest$Y[dftest$frame == k][c(99,119)],  type = "l", col = colors[5],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(120,139)], y=dftest$Y[dftest$frame == k][c(120,139)], type = "l", col = colors[6],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(140,156)], y=dftest$Y[dftest$frame == k][c(140,156)], type = "l", col = colors[7],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(157,170)], y=dftest$Y[dftest$frame == k][c(157,170)], type = "l", col = colors[8],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(171,179)], y=dftest$Y[dftest$frame == k][c(171,179)], type = "l", col = colors[9],  lwd=2)
  points(x=dftest$X[dftest$frame == k][c(180,185)], y=dftest$Y[dftest$frame == k][c(180,185)], type = "l", col = colors[10], lwd=2)
  points(x=dftest$X[dftest$frame == k][c(186,190)], y=dftest$Y[dftest$frame == k][c(186,190)], type = "l", col = colors[11], lwd=2)
  points(x=dftest$X[dftest$frame == k][c(191,200)], y=dftest$Y[dftest$frame == k][c(191,200)], type = "l", col = colors[12], lwd=2)
  points(x=dftest$X[dftest$frame == k][1],   y=dftest$Y[dftest$frame == k][1], pch=18, col = "red",  cex=2)
  legend("topright",legend = c(paste("Segment", 1:12), "Head"),col = c(colors, "Red"),lwd = c(rep(2, 12), NA), bty = "n",cex = 0.8, pch = c(rep(NA,12), 18), pt.cex = 1.5)
  
}

# valeurs cumsum, diff, et brutes
AngleVarPlot <- function() {
  plot(df$angle[df$fs=="002_fs1"], main = "diff entre valeurs brutes, diff() et cumsum(diff()) pour un fast-start. 11 frames par segment",
       xlim=c(1,145), ylim=c(-190,600), ylab = "angle en degrés")
  points(df$angle_diff[df$fs=="002_fs1"], col="green")
  points(df$angle_cum_diff[df$fs=="002_fs1"], col="red")
  abline(v = c(1,12,23,34,45,56,67,78,89,100,111,122), col = "darkblue", lwd = 1, lty=2)
  abline(h = c(-180, 180), col = "grey", lty = 1)
  legend("topright", legend =c("angle (deg)", "diff(angle)", "cumsum(diff(angle))", "frame 1", "bornes (deg)"), pch=c(1,1,1,NA, NA), lty=c(NA,NA,NA,2, 1),  col = c("black", "green", "red", "darkblue", "black"))
  
}

# valeurs cumsum, diff, et brutes unique fs
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

# Evolution des valeurs en fonction du temps unique fs
FSAngleCumPlot <- function(cloud) {
  ggplot(df[df$fs==cloud,], aes(x=frame, y=angle_cum_diff, group = seg, color = seg)) +  
    geom_line() +
    ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps pour un fast start",
            subtitle = "decoupage en 12 segments donc 12 courbes différentes, fs=002_fs1")
}

# Evolution des valeurs en fonction du temps tous les fs
AngleCumPlot <- function() {
  ggplot(df, aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
    #facet_wrap(~fs) + # pour séparer par fs plus lisible
    scale_color_viridis_d() + # pour des couleurs plus propres
    theme_minimal() + # gère le thème
    ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps",
            subtitle = "decoupage en 12 segments donc 12 courbes différentes par fast-start (195*12 -> 2340 courbes)")
}

# separer entre les 5 premiers segments et les 5 derniers
SepAngleCumPlot <- function() {
  p0 <- ggplot(df[df$pts < 120, ], aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 600)) +
    ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps pour les 5 premiers segments")
  p100 <- ggplot(df[df$pts >= 120, ], aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = factor(seg))) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 600)) +
    ggtitle("Evolution de la somme cumulée de la diffférence d'angle au cours du temps pour les 7 derniers segments")
  ggpubr::ggarrange(p0, p100, ncol=2, nrow=1) 
  
}


# Pour representer la densite
AngleDensPlot <- function() {
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
  
  p120 + dens + patchwork::plot_layout(widths = c(4, 1), guides = "collect") & theme(legend.position = "right") # plotter
}

# cumdiffseg2 avec le traitement
AngleTreatPlot <- function() {
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
  
  dens1 + patchwork::plot_spacer() + ptreat + dens2 + # plotter
    patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4), guides = "collect") & theme(legend.position = "right")
}

# un seul poisson
AngleFSFishPlot <- function(cloud) {
  pfish <- ggplot(df[df$poisson==cloud,], aes(x = frame, y = angle_cum_diff, group = interaction(fs, seg), color = fs)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
    #scale_color_viridis_d() + # pour des couleurs plus propres
    labs(
      title = "Évolution cumulée de l'angle en fonction du fats-start pour le poisson 27",
      x = "Frame",
      y = "Difference d'angle cumulée",
    ) +
    theme_minimal() # gère le thème
  
  dens3 <- ggplot(df[df$poisson==cloud,], aes(x = frame, fill = fs)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    theme(legend.position = "none")
  
  dens4 <- ggplot(df[df$poisson==cloud,], aes(x = angle_cum_diff, fill = fs)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    theme(legend.position = "none") + 
    coord_flip()
  
  dens3 + patchwork::plot_spacer() + pfish + dens4 + # plotter
    patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
}

# un seul segment
AngleSegPlot <- function(cloud){
  ptreat <- ggplot(df2[df2$seg==cloud,], aes(x = frame, y = angle_cum_diff, group = interaction(poisson, fs), color = traitement)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
    scale_color_viridis_d() + # pour des couleurs plus propres
    geom_point() +
    theme_minimal() # gère le thème
  
  dens1 <- ggplot(df2[df2$seg==cloud,], aes(x = frame, fill = traitement)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    theme(legend.position = "none")
  
  dens2 <- ggplot(df2[df2$seg==cloud,], aes(x = angle_cum_diff, fill = traitement)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    theme(legend.position = "none") + 
    coord_flip()
  
  dens1 + patchwork::plot_spacer() + ptreat + dens2 + # plotter
    patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
}

# ratio snout tail unique fs
FSRatioSnoutTailPlot <- function (cloud, nimbostratus) {
  
  ghjk <- coordinates[coordinates$fs==cloud & coordinates$frame==nimbostratus,]
  snouted <- snout[snout$fs==cloud & snout$frame==nimbostratus,]
  tailed <- tail[tail$fs==cloud & tail$frame==nimbostratus,]
  coefed <- coef[coef$fs==cloud & coef$frame==nimbostratus,]
  
  plot(ghjk$X,ghjk$Y, asp=1, main = paste0("ratio = ", round(coefed$ratio[1],2), 
                                    ", snout angle = ",round(snouted$angle[1],2),
                                    ", tail angle = ", round(tailed$angle[1],2), 
                                    ". Angles calculated from X axis using atan2()"))
  theta_deg <- snouted$angle[1]
  theta <- theta_deg * pi / 180
  X <- snouted$X[1]
  Y <- snouted$Y[1]
  m <- tan(theta)
  abline(a = Y - m*X, b = m, col = "red", lwd = 2)
  theta_deg2 <- tailed$angle[1]
  theta2 <- theta_deg2 * pi / 180
  X2 <- tailed$X[1]
  Y2 <- tailed$Y[1]
  m2 <- tan(theta2)
  abline(a = Y2 - m2*X2, b = m2, col = "green", lwd = 2)
  coefed$ratio[1]
}

# ratio snout tail tous les fs
RatioSnoutTailPlot <- function() {
  ggplot(coef, aes(x = frame, y = ratio, group = fs, color = treatment)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
    #facet_wrap(~fs) + # pour séparer par fs plus lisible
    scale_color_viridis_d() + # pour des couleurs plus propres
    theme_minimal() + # gère le thème
    ggtitle("Evolution du ratio entre angle de la tête (0-40) et de la queue (161-200) au cours du temps",
            subtitle = "plus le ratio est faible, plus les segements sont parralèles, forme de S ou non-courbé" )
}

# ratio anterior part posterior part unique fs
FSRatioAntPostPlot <- function (cloud, nimbostratus) {
  
  ghjk2 <- coordinates[coordinates$fs==cloud & coordinates$frame==nimbostratus,]
  anted <- ant[ant$fs==cloud & ant$frame==nimbostratus,]
  posted <- post[post$fs==cloud & post$frame==nimbostratus,]
  coefed2 <- coef2[coef2$fs==cloud & coef2$frame==nimbostratus,]
  
  plot(ghjk2$X,ghjk2$Y, asp=1, main = paste0("ratio = ", round(coefed2$ratio[1],2), 
                                    ", snout angle = ",round(anted$angle[1],2),
                                    ", tail angle = ", round(posted$angle[1],2), 
                                    ". Angles calculated from X axis using atan2()"))
  theta_deg <- anted$angle[1]
  theta <- theta_deg * pi / 180
  X <- anted$X[1]
  Y <- anted$Y[1]
  m <- tan(theta)
  abline(a = Y - m*X, b = m, col = "red", lwd = 2)
  theta_deg2 <- posted$angle[1]
  theta2 <- theta_deg2 * pi / 180
  X2 <- posted$X[1]
  Y2 <- posted$Y[1]
  m2 <- tan(theta2)
  abline(a = Y2 - m2*X2, b = m2, col = "green", lwd = 2)
  coefed2$ratio[1]
}

# ratio anterior part posterior part tous les fs
RatioAntPostPlot <- function() {
  ggplot(coef2, aes(x = frame, y = ratio, group = fs, color = treatment)) + # le as.factor(pts) permet d'avoir des valeur continues pour pts par les num de joints
    geom_line(size = 0.8, alpha = 0.5) + # geom_line(size pour plus fin, et alpha pour la transparence, show.legend pour masquer la legende)
    #facet_wrap(~fs) + # pour séparer par fs plus lisible
    scale_color_viridis_d() + # pour des couleurs plus propres
    theme_minimal() + # gère le thème
    ggtitle("Evolution du ratio entre angle partie antérieure (0-100) et partie postérieure (101-200) au cours du temps",
            subtitle = "plus le ratio est faible, plus les segements sont parralèles, forme de Sou non-courbé" )
}

# Barplot of population origin_treatment
Barplot_origin_treat <- function () {
  
  eff <- table(treat$treatment, treat$pool)
  barress <- barplot(eff, ylim=c(0,max(eff)*2), col = c("darkgreen", "blue", "darkred"),
                     main = "Comparison of populations' size")
  legend("topright", legend=c("aval", "amont"), col=c("blue", "darkgreen"), pch=15)
  text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")
  
}

# Plot for segmentation pre - GAM
preGAMPLOTsegm <- function() {
  
  compact_theme <- theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2,2,2,2)
    )
  
  # plot1 effet de origin x treatment
  p1 <- ggplot(segm,aes(x = time, y = Y,color = origin_treatment)) +
    geom_smooth(method = "gam",formula = y ~ s(x),linewidth = 1) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Effet pool × traitement",
      x = "Temps",
      y = "Y",
      color = "Origine-Traitement") + compact_theme
  
  # plot2 effet du segment
  p2 <- ggplot(segm,aes(x = time, y = Y, color = segment)) +
    geom_smooth(method = "gam",formula = y ~ s(x),linewidth = 1,se = FALSE) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Effet du segment",
      x = "Temps",
      y = "Y",
      color = "Segment") + compact_theme +
    theme(legend.text = element_text(size = 6), legend.title.position = "top")
  
  # plot3 classe de poids
  p3 <- ggplot(segm2,aes(x = time, y = Y)) +
    geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Classes de poids",
      x = "Temps",
      y = "Y",
      color = "Poids") + compact_theme
  
  # plot4 classe de taille
  p4 <- ggplot(segm2,aes(x = time, y = Y)) +
    geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Classes de taille",
      x = "Temps",
      y = "Y",
      color = "Taille") + compact_theme
  
  # combination
  final_plot <- ((p1 | p2) / (p3 | p4)) +
    patchwork::plot_layout(widths = c(1,1),heights = c(1,1)) +
    patchwork::plot_annotation(title = "Visualisation des effets des variables explicatives sur l'angle cumulée par segments")
  
  return(final_plot)
}

# Plot for segmentation GAM

# Plot for cumulative distance pre - GAM
preGAMPLOTdcum <- function() {
  
  compact_theme <- theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2,2,2,2)
    )
  
  p1 <- ggplot(na.omit(dfcum), aes(x = time, y = Y, color = origin_treatment)) +
  geom_smooth(se = TRUE) +
    labs(title = "Dynamique temporelle selon origin x treatment",
         x = "Temps",y = "Y") + compact_theme # interaction très intéressante entre treatment et pool 
 
  
  p2 <- ggplot(dfcum2, aes(x = time, y = Y)) + 
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de poids",
         x = "Temps",y = "Y",color = "Classe de poids") + compact_theme # peu de différence selon les classes de poids, peut-être dans la dynamique temporelle après 0.10s
  
  p3 <- ggplot(dfcum2, aes(x = time, y = Y)) +
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de taille",
         x = "Temps",y = "Y",color = "Classe de taille") + compact_theme # idem
  
  # combination
  final_plot <- ((p2 | p3) / (p1)) +
    patchwork::plot_layout(widths = c(1,1),heights = c(1,1)) +
    patchwork::plot_annotation(title = "Visualisation des effets des variables explicatives sur la distance cumulée")
  
  return(final_plot)
}

# Plot for acceleration pre - GAM
preGAMPLOTaccel <- function() {
  
  compact_theme <- theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2,2,2,2)
    )
  
  p1 <- ggplot(na.omit(dfa), aes(x = time, y = Y, color = origin_treatment)) + geom_smooth() +
    labs(title = "Dynamique temporelle selon origin x treatment",
         x = "Temps",y = "Y") + compact_theme # meilleur fit avec une relation non linéaire a priori
  
  p2 <- ggplot(dfa2, aes(x = time, y = Y)) + 
    geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de poids",
         x = "Temps",y = "Y",color = "Classe de poids") + compact_theme
  # peu de différence dans la valeur finale selon les classes de poids, dynamique temporelle des 'lourds' avec beaucoup plus de 'wigliness' mais IC superposés
  p3 <- ggplot(dfa2, aes(x = time, y = Y)) +
    geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de taille",
         x = "Temps",y = "Y",color = "Classe de taille") + compact_theme # idem, moins de wigliness des 'grands'
  
  # combination
  final_plot <- ((p2 | p3) / (p1)) +
    patchwork::plot_layout(widths = c(1,1),heights = c(1,1)) +
    patchwork::plot_annotation(title = "Visualisation des effets des variables explicatives sur l'accélération")
  
  return(final_plot)
}  

# Plot for efficiency pre - ANOVA
preGAMPLOTefficiency <- function() {
  
  p1 <- ggplot(dfe,aes(x = treatment,y = efficiency,fill = treatment)) +
    geom_violin(trim = FALSE,alpha = 0.5) + geom_boxplot(width = 0.15,outlier.shape = NA,alpha = 0.8) +
    geom_jitter(width = 0.08, alpha = 0.2,size = 0.8) +
    facet_wrap(~pool) +
    theme_minimal(base_size = 14) +
    labs(title = "Distribution de l'efficacité", subtitle = "Comparaison des traitements selon l'origine",
         x = "Traitement",y = "Efficiency") +
    theme(legend.position = "none",strip.text = element_text(face = "bold"))
  
  p2 <- ggplot2::ggplot(dfe,aes(x = treatment,y = efficiency,color = pool,group = pool)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se,geom = "errorbar",width = 0.1) +
    theme_minimal(base_size = 14) +
    labs(y = "Efficiency",x = "Traitement",color = "Origine",title = "Interaction traitement × origine" )
  
  # combination
  final_plot <- (p1 | p2) +
    patchwork::plot_layout(widths = c(1,1)) +
    patchwork::plot_annotation(title = "Visualisation des effets des variables explicatives sur l'efficacité")
  
  return(final_plot)
}  

# Plot for ratio snout/tail pre - GAM
preGAMPLOTsnail <- function() {
  
  compact_theme <- theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2,2,2,2)
    )
  
  p1 <- ggplot(snail, aes(x = time, y = Y)) + 
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = origin_treatment),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon pool:treatment",
         x = "Temps",y = "Y",color = "Classe de poids") + compact_theme # diff notable du moins visuellement, lees athas amont sort du lot

  p2 <- ggplot(snail2, aes(x = time, y = Y)) + 
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de poids",
         x = "Temps",y = "Y",color = "Classe de poids") + compact_theme
  # différence dans la valeur finale et dans la dynamique temporelle selon les classes de poids, pas même orientation selon poids !!
  p3 <- ggplot(snail2, aes(x = time, y = Y)) +
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de taille",
         x = "Temps",y = "Y",color = "Classe de taille")  + compact_theme # idem, grosse diff entre les grands et petits, orientation et valeur finale
  
  # combination
  final_plot <- ((p2 | p3) / (p1)) +
    patchwork::plot_layout(widths = c(1,1),heights = c(1,1)) +
    patchwork::plot_annotation(title = "Visualisation des effets des variables explicatives sur le ratio snout/tail")
  
  return(final_plot)
  
}

# Plot for ratio ant/post pre - GAM
preGAMPLOTantpost <- function() {
  
  compact_theme <- theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2,2,2,2)
    )
  
  p1 <- ggplot(antpost, aes(x = time, y = Y)) + 
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = origin_treatment),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon pool:treatment",
         x = "Temps",y = "Y",color = "Classe de poids") + compact_theme # diff notable du moins visuellement, lees athas amont sort du lot

  p2 <- ggplot(antpost2, aes(x = time, y = Y)) + 
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de poids",
         x = "Temps",y = "Y",color = "Classe de poids") + compact_theme
  # différence dans la valeur finale et dans la dynamique temporelle selon les classes de poids, pas même orientation selon poids !!
  p3 <- ggplot(antpost2, aes(x = time, y = Y)) +
    geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
    geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Dynamique temporelle selon les classes de taille",
         x = "Temps",y = "Y",color = "Classe de taille") + compact_theme# idem, grosse diff entre les grands et petits, orientation et valeur finale
  
  # combination
  final_plot <- ((p2 | p3) / (p1)) +
    patchwork::plot_layout(widths = c(1,1),heights = c(1,1)) +
    patchwork::plot_annotation(title = "Visualisation des effets des variables explicatives sur le ratio anterior/posterior")
  
  return(final_plot)
  
}
