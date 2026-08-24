#
# 22/04/2026 - 04.GIF.R
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
library(tidyverse)
library(magick)
library(patchwork)
library(jpeg)
library(ggplot2)
library(cowplot)
library(dplyr)

# folder path ----
input_folder <- "data"
dir.create("output", showWarnings = T, recursive= T)
output_folder <- "output"
coord_path <- "output/coordinates.txt"
image_folder <- "supplementary_data/178_fs1"
output_gif <- "output/faststart_178fs1.gif"

# list files ----
mean_joint <- readRDS("output/mean_joint.rds")
name <- file.path(output_folder, "coordinates.txt")
coordinates <- read.table(name, header=TRUE)
coor <- coordinates %>%
  mutate(
    frame = as.factor(frame),
    fs = as.factor(fs)
  )
dftest <- coor[coor$fs == "002_fs1",] # 200 pts, 11 frames

#coordinates <- read.table("output/dfsegmentedvisu.txt", header = TRUE)
str(coordinates)
coor <- coordinates %>%
  mutate(
    frame = as.factor(frame),
    fs = as.factor(fs)
  )

position <- function(cloud) {
  k <- as.numeric(cloud)
  plot(dftest$Y[dftest$frame == k] ~ dftest$X[dftest$frame == k], type = "n", # premier plot avec une seule frame du premier fast start
       xlim = c(min(dftest$X)-20 , (max(dftest$X)+20)),
       ylim = c(min(dftest$Y)-50 , (max(dftest$Y)+50)),
       main = paste0("Fish position in space (pixels), fs = 207_fs2, frame = ", k), xlab = "", ylab="")
  points(x = dftest$X[dftest$frame == k][1], y = dftest$Y[dftest$frame == k][1], col="green", pch = 15, cex = 1.5)
  points(x = dftest$X[dftest$frame == k], y = dftest$Y[dftest$frame == k], pch = 1, lwd = 2, col ="black") # tout semble bien positionné
  legend("topright",legend = c("Head", "bodymidline"),col = c("green", "black"),
         pch = c(15, 1),pt.cex = c(1.5, 1),bty = "n")
}
nframe <- seq(1:12)
position(1)
position(2)
position(3)
position(4)
position(5)
position(6)
position(7)
position(8)
position(9)
position(10)
position(11)
position(12)
position(13)
position(14)
position(15)
position(16)

# segments ----
segmented <- function(cloud) {
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
segmented(1)
segmented(2)
segmented(3)
segmented(4)
segmented(5)
segmented(6)
segmented(7)

# 
# load coordinates ----
# 

coordinates <- read.table(coord_path, header = TRUE)
coor <- coordinates %>%
  mutate(
    frame = as.numeric(frame),
    fs = as.factor(fs)
  )

#
# select fast-start ----
# use Ctrl + f to change fast-start number through the whole document
#

dftest <- coor %>%
  filter(fs == "178_fs1")
frames <- sort(unique(dftest$frame))
dftest <- dftest %>%
  arrange(frame, pts) %>%
  group_by(frame) %>%
  mutate(
    
    dx = lead(X) - lag(X),
    dy = lead(Y) - lag(Y),
    
    angle = atan2(dy, dx) * 180 / pi
    
  ) %>%
  ungroup()

#
# global limits ----
#

xmin <- min(dftest$X) - 10
xmax <- max(dftest$X) + 10

ymin <- min(dftest$Y) - 10
ymax <- max(dftest$Y) + 10

# colors ----
segment_cols <- scales::hue_pal()(12)

# 
# create png frames ----
#

dir.create("output/temp_frames",
           recursive = TRUE,
           showWarnings = FALSE)


#
# make segment plot ----
#
make_segment_plot <- function(dfk,
                              xmin,
                              xmax,
                              ymin,
                              ymax,
                              segment_cols){
  
  #
  # définition des segments
  #
  
  segment_list <- list(
    c(1,27),
    c(28,53),
    c(54,76),
    c(77,98),
    c(99,119),
    c(120,139),
    c(140,156),
    c(157,170),
    c(171,179),
    c(180,185),
    c(186,190),
    c(191,200)
  )
  
  #
  # construire dataframe segments
  #
  
  seg_df <- data.frame()
  
  for(i in 1:12){
    
    idx <- segment_list[[i]]
    
    #
    # sécurité
    #
    
    idx <- idx[idx <= nrow(dfk)]
    
    if(length(idx) < 2){
      next
    }
    
    tmp <- data.frame(
      X = dfk$X[idx],
      Y = dfk$Y[idx],
      segment = factor(
        paste0("Segment ", i),
        levels = paste0("Segment ", 1:12)
      )
    )
    
    seg_df <- rbind(seg_df, tmp)
  }
  
  #
  # plot
  #
  
  ggplot() +
    
    #
    # segments colorés
    #
    
    geom_path(
      data = seg_df,
      aes(
        X,
        Y,
        colour = segment,
        group = segment
      ),
      linewidth = 0.8
    ) +
    
    #
    # tête
    #
    
    annotate(
      "point",
      x = dfk$X[1],
      y = dfk$Y[1],
      colour = "red",
      shape = 18,
      size = 4
    ) +
    
    #
    # couleurs
    #
    
    scale_colour_manual(values = segment_cols) +
    
    #
    # axes
    #
    
    coord_fixed(
      xlim = c(xmin, xmax),
      ylim = c(ymax, ymin)
    ) +
    
    #
    # thème
    #
    
    theme_classic(base_size = 14) +
    
    theme(
      legend.position = "right",
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    ) +
    
    #
    # labels
    #
    
    labs(
      title = "Body segment modelization",
      x = "X position (pixels)",
      y = "Y position (pixels)",
      colour = ""
    )
}


#
# loop over frames ----
#

for(k in frames){
  
  # 
  # LOAD REAL IMAGE
  # 
  img_name <- sprintf(
    "178_fs1-%05d.jpg",
    k
  )
  
  img_path <- file.path(image_folder, img_name)
  
  #
  # LOAD IMAGE WITH MAGICK
  #
  
  img <- magick::image_read(img_path)
  
  #
  # Image dimensions
  #
  
  info <- magick::image_info(img)
  
  #
  # Convert to raster for ggplot
  #
  
  img_raster <- as.raster(img)
  
  #
  # LEFT PANEL = REAL FRAME
  #
  
  p1 <- ggdraw() +
    draw_image(img_path) +
    draw_label(
      paste0("Real frame : ", k),
      x = 0.5,
      y = 0.97,
      size = 16,
      fontface = "bold"
    ) +
    
    theme_void() +
    
    ggtitle(
      paste0(
        "Real frame : ",
        k
      )
    )
  
  #
  # RIGHT PANEL = COORDINATES
  #
  
  dfk <- dftest %>%
    filter(frame == k)
  
  p2 <- ggplot(dfk, aes(X, Y)) +
    
    geom_path(
      linewidth = 1.2,
      colour = "black"
    ) +
    
    geom_point(
      size = 0.6,
      colour = "black"
    ) +
    
    annotate(
      "point",
      x = dfk$X[1],
      y = dfk$Y[1],
      colour = "forestgreen",
      size = 4,
      shape = 15
    ) +
    
    coord_fixed(
      xlim = c(xmin, xmax),
      ylim = c(ymax, ymin)
    ) +
    
    theme_classic(base_size = 16) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    ) +
    
    labs(
      title = paste0(
        "Tracked coordinates : frame ",
        k
      ),
      x = "X position (pixels)",
      y = "Y position (pixels)"
    )
  
  #
  # 3rd Panel
  #
  
  p3 <- make_segment_plot(
    dfk = dfk,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    segment_cols = segment_cols
  )
  
  #
  # COMBINE PANELS
  #
  
  final_plot <- p1 + p2 + p3 +
    patchwork::plot_layout(widths = c(1,1,1)) +
    patchwork::plot_annotation(
      title = "Fast-start kinematics reconstruction",
      theme = theme(
        plot.title = element_text(
          size = 20,
          face = "bold",
          hjust = 0.5
        )
      )
    )
  
  #
  # Save frame
  #
  
  ggsave(
    filename = sprintf(
      "output/temp_frames/frame_%03d.png",
      k
    ),
    plot = final_plot,
    width = 16,
    height = 6,
    dpi = 250
  )
}

#
# GIF ----
#

frame_files <- list.files(
  "output/temp_frames",
  full.names = TRUE,
  pattern = ".png$"
)

animation <- image_read(frame_files)
#
# Add pause on last frame
#

last_frame <- animation[length(animation)]

pause <- image_join(
  rep(last_frame, 10)
)

animation <- c(animation, pause)

# Animation speed
animation <- image_animate(
  animation,
  fps = 4,
  loop = 0
)

# Save GIF
image_write(
  animation,
  output_gif
)

cat("GIF saved :", output_gif)

## FIN SCRIPT 04.2_GIF_FULL.R


