#
# 9/06/26 - 01.fast_start_animation_whole_fish.R
#
# Fish silhouette reconstruction from fast-start acquired with MATLAB midlines
# First the body is reconstructed from the midlines, then each frames is plotted
# Finally the final GIF gather all frames for fast start animation or visualization
#
# autor: FG    
# project : fish_silhouette_animation
# latest modification : 22/06/2026
#

rm(list = ls())

#
# library ----
#

library(readxl)
library(dplyr)
library(ggplot2)
library(svglite)
library(gifski)

#
# PARAMETERS ----
#

#name <- "005_fs2"
name <- "037_fs1"

name_file <- sub("","data/area/", name)
name_file <- paste0(name_file, ".xls")
input_file <- name_file
dir_name <- sub("", "output/fish_reconstruction/", name)
output_up_dir <- "output/fish_reconstruction"
output_dir <- dir_name

dir.create(output_dir,recursive = TRUE,showWarnings = FALSE)
dir.create(output_up_dir,recursive = TRUE,showWarnings = FALSE)

dir.create(
  file.path(output_dir,"frames"),
  recursive = TRUE,
  showWarnings = FALSE
)

spar_value <- 0.25

n_interp <- 1000

body_width_ratio <- 0.07

#
# import ----
#

df <- read_excel(
  input_file,
  col_names = FALSE
)

# première ligne = numéro des frames

df <- df[-1,]

df <- as.data.frame(df)

for(k in seq_len(ncol(df))){
  df[[k]] <- as.numeric(df[[k]])
}

n_frames <- ncol(df)/2

cat("Frames :",n_frames,"\n")
cat("Points :",nrow(df),"\n")

#
# width profile ----
#

width_profile <- function(s){
  
  approx(
    x = c(
      0.00,
      0.02,
      0.05,
      0.10,
      0.18,
      0.30,
      0.50,
      0.75,
      0.90,
      1.00
    ),
    y = c(
      0.00,
      0.70,
      1.20,
      1.40,
      1.50,
      1.35,
      1.00,
      0.55,
      0.18,
      0.00
    ),
    xout = s
  )$y
  
}

#
# build silhouette ----
#

build_fish <- function(x,y){
  
  t <- seq_along(x)
  
  sx <- smooth.spline(
    t,
    x,
    spar = spar_value
  )
  
  sy <- smooth.spline(
    t,
    y,
    spar = spar_value
  )
  
  tt <- seq(
    min(t),
    max(t),
    length.out = n_interp
  )
  
  x <- predict(sx,tt)$y
  y <- predict(sy,tt)$y
  
  dx <- c(diff(x),tail(diff(x),1))
  dy <- c(diff(y),tail(diff(y),1))
  
  L <- sqrt(dx^2 + dy^2)
  
  tx <- dx/L
  ty <- dy/L
  
  nx <- -ty
  ny <- tx
  
  s <- seq(
    0,
    1,
    length.out = length(x)
  )
  
  fish_length <- sum(
    sqrt(
      diff(x)^2 +
        diff(y)^2
    )
  )
  
  max_width <- fish_length *
    body_width_ratio
  
  width <- width_profile(s) *
    max_width
  
  # dos légèrement plus fin
  dorsal_width <- width * 0.45
  
  # ventre légèrement plus large
  ventral_width <- width * 0.55
  
  left_x <- x + nx*dorsal_width
  left_y <- y + ny*dorsal_width
  
  right_x <- rev(
    x - nx*ventral_width
  )
  
  right_y <- rev(
    y - ny*ventral_width
  )
  
  body <- data.frame(
    x = c(left_x,right_x),
    y = c(left_y,right_y)
  )
  
  body
  
}

#
# reconstruct frames ----
#

fish_list <- vector(
  "list",
  n_frames
)

for(frame in seq_len(n_frames)){
  
  x <- df[[2*frame-1]]
  y <- df[[2*frame]]
  
  fish <- build_fish(x,y)
  
  fish$frame <- frame
  
  fish_list[[frame]] <- fish
  
}

fish_all <- bind_rows(fish_list)

#
# limits ----
#

xmin <- min(fish_all$x)
xmax <- max(fish_all$x)

ymin <- min(fish_all$y)
ymax <- max(fish_all$y)

#
# overlay figure ----
#

cols <- hcl.colors(
  n_frames,
  palette = "Viridis"
)

overlay_plot <- ggplot() +
  
  geom_polygon(
    data = fish_all,
    aes(
      x,
      y,
      group = frame,
      fill = factor(frame)
    ),
    alpha = 0.35,
    colour = "black",
    linewidth = 0.15
  ) +
  
  scale_fill_manual(
    values = cols
  ) +
  
  coord_equal() +
  
  theme_void() +
  
  theme(
    legend.position = "none"
  )

print(overlay_plot)

#
# export overlay ----
#

ggsave(
  file.path(
    output_dir,
    "fish_overlay.pdf"
  ),
  overlay_plot,
  width = 10,
  height = 8
)

ggsave(
  file.path(
    output_dir,
    "fish_overlay.svg"
  ),
  overlay_plot,
  width = 10,
  height = 8
)

#
# export frames ----
#

for(frame in seq_len(n_frames)){
  
  fish <- fish_all[
    fish_all$frame == frame,
  ]
  
  p <- ggplot() +
    
    geom_polygon(
      data = fish,
      aes(x,y),
      fill = "grey75",
      colour = "black",
      linewidth = 0.3
    ) +
    
    coord_equal(
      xlim = c(xmin,xmax),
      ylim = c(ymin,ymax)
    ) +
    
    theme_void()
  
  ggsave(
    filename = file.path(
      output_dir,
      "frames",
      sprintf(
        "frame_%02d.png",
        frame
      )
    ),
    plot = p,
    width = 8,
    height = 4,
    dpi = 300,
    bg = "white"
  )
  
}

for(frame in seq_len(n_frames)){
  
  fish <- fish_all[
    fish_all$frame == frame,
  ]
  
  p <- ggplot() +
    
    geom_polygon(
      data = fish,
      aes(x,y),
      fill = "grey75",
      colour = "black",
      linewidth = 0.3
    ) +
    
    coord_equal(
      xlim = c(xmin,xmax),
      ylim = c(ymin,ymax)
    ) +
    
    theme_void()
  
  ggsave(
    
    file.path(
      output_dir,
      "frames",
      sprintf(
        "frame_%02d.svg",
        frame
      )
    ),
    
    p,
    
    width = 8,
    height = 4
    
  )}

#
## GIF by indivual ----
#

png_files <- list.files(
  file.path(
    output_dir,
    "frames"
  ),
  pattern = "png$",
  full.names = TRUE
)

png_files <- sort(png_files)

print(basename(png_files))

gifski(
  png_files,
  gif_file = file.path(
    output_dir,
    "fish_animation.gif"
  ),
  delay = 0.08,
  loop = TRUE
)

cat(
  "\nGIF exported:\n",
  file.path(
    output_dir,
    "fish_animation.gif"
  ),
  "\n"
)


#
# grouped animation ----
# pour presentation oral support visuel -> fast start plus long et couleur de fonc correspondante 
#

# individuals to display ---- 

names <- c(
  "008_fs2",
  "021_fs3",
  "180_fs2",
  "039_fs1"
)

# DOSSIER DE SORTIE -

output_dir_group <- "output/fish_reconstruction/animation_groupee"

dir.create(
  output_dir_group,
  recursive=TRUE,
  showWarnings=FALSE
)

dir.create(
  file.path(output_dir_group,"frames"),
  recursive=TRUE,
  showWarnings=FALSE
)

# POSITIONNEMENT AUTOMATIQUE
# (modifier spacing si besoin)

spacing <- 350

n_ind <- length(names)

offsets <- data.frame(
  
  name=names,
  
  dx=seq(
    0,
    by=spacing,
    length.out=n_ind
  ),
  
  dy=rep(0,n_ind)
  
)

# IMPORT DE TOUS LES INDIVIDUS

fish_all_group <- list()

global_n_frames <- Inf

for(i in seq_along(names)){
  
  name <- names[i]
  
  input_file <- paste0(
    "data/area/",
    name,
    ".xls"
  )
  
  df <- read_excel(
    input_file,
    col_names=FALSE
  )
  
  df <- df[-1,]
  df <- as.data.frame(df)
  
  for(k in seq_len(ncol(df))){
    df[[k]] <- as.numeric(df[[k]])
  }
  
  n_frames_i <- ncol(df)/2
  
  global_n_frames <- min(
    global_n_frames,
    n_frames_i
  )
  
  fish_list <- vector(
    "list",
    n_frames_i
  )
  
  for(frame in seq_len(n_frames_i)){
    
    x <- df[[2*frame-1]]
    y <- df[[2*frame]]
    
    fish <- build_fish(
      x,
      y
    )
    
    fish$x <- fish$x +
      offsets$dx[i]
    
    fish$y <- fish$y +
      offsets$dy[i]
    
    fish$frame <- frame
    fish$id <- name
    
    fish_list[[frame]] <- fish
    
  }
  
  fish_all_group[[i]] <- bind_rows(
    fish_list
  )
  
}

fish_all_group <- bind_rows(
  fish_all_group
)

# limits ----

xmin <- min(fish_all_group$x)-150
xmax <- max(fish_all_group$x)+150

ymin <- min(fish_all_group$y)-150
ymax <- max(fish_all_group$y)+150

# EXPORT PNG

for(frame in seq_len(global_n_frames)){
  
  fish <- fish_all_group[
    fish_all_group$frame==frame,
  ]
  
  p <- ggplot()+
    
    geom_polygon(
      data=fish,
      aes(
        x,
        y,
        group=id
      ),
      fill="grey75",
      colour="black",
      linewidth=0.3
    )+
    
    coord_equal(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    )+
    
    theme_void()
  
  ggsave(
    
    filename=file.path(
      output_dir_group,
      "frames",
      sprintf(
        "frame_%03d.png",
        frame
      )
    ),
    
    plot=p,
    
    width=14,
    height=10,
    dpi=300,
    bg="#e9ede8"
    
  )
  
}

# EXPORT SVG

for(frame in seq_len(global_n_frames)){
  
  fish <- fish_all_group[
    fish_all_group$frame==frame,
  ]
  
  p <- ggplot()+
    
    geom_polygon(
      data=fish,
      aes(
        x,
        y,
        group=id
      ),
      fill="grey75",
      colour="black",
      linewidth=0.3
    )+
    
    coord_equal(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    )+
    
    theme_void()
  
  ggsave(
    
    filename=file.path(
      output_dir_group,
      "frames",
      sprintf(
        "frame_%03d.svg",
        frame
      )
    ),
    
    plot=p,
    
    width=14,
    height=10
    
  )
  
}

## GIF grouped ----

png_files <- list.files(
  file.path(
    output_dir_group,
    "frames"
  ),
  pattern="png$",
  full.names=TRUE
)

png_files <- sort(
  png_files
)

gifski(
  png_files,
  gif_file=file.path(
    output_dir_group,
    "fish_animation_groupee.gif"
  ),
  delay=0.10,
  loop=TRUE
)

cat(
  "\nGIF exporté :\n",
  file.path(
    output_dir_group,
    "fish_animation_groupee.gif"
  ),
  "\n"
)


## END SCRIPT 01_fast_start_animation_whole_fish.R
