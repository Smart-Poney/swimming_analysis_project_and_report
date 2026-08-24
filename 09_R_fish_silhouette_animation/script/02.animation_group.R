#
# 10/06/26 - 02.animation_group.R
#
# Fish silhouette reconstruction from tracking coordinates acquired with EthoVisionXT
# First the body is reconstructed from the three-points
# Finally the final GIF gather all frames for behavior animation or visualization
#
# autor: FG    
# project : fish_silhouette_animation
# latest modification : 22/06/2026
#

rm(list=ls())
# library ----
library(readxl)
library(dplyr)
library(ggplot2)
library(gifski)
library(svglite)
library(zoo)
library(viridis)

# nom des individus ----
#
# Les fichiers devront être présents dans :
# data/raw/
#
# Exemple :
# Raw data-FG_calibration_comportement-Trial    52.xlsx
# Raw data-FG_calibration_comportement-Trial   117.xlsx

names <- c(
  "133",
  " 20",
  " 29",
  " 43",
  " 52",
  " 77",
  " 97",
  "117",
  "159",
  " 38",
  " 61",
  " 84",
  "104",
  "127"
)

#
# PARAMETRES ----
#

frame_start <- 1500 # Nombre maximum d'images utilisées
frame_end <- 2000
frame_step <- 1
frame_interpolation <- 4

# background_colour <- "#e9ede8" # Apparence
background_colour <- "#195334" #"black" # Apparence 
fish_fill <- "grey75"
fish_border <- "white" 

figure_width <- 19 # Export et paramètres de GIF 
figure_height <- 11
dpi_export <- 300
gif_delay <- round(0.04/frame_interpolation, digits=2) # 25fps = 0.04s

n_backbone <- 40 # nombre de points créés entre
n_interp <- 1000 # interpolation finale
spar_value <- 0.25 # lissage spline

body_width_ratio <- 0.07 # largeur relative

# espacement automatique ----
spacing_x <- 10
spacing_y <- 10

smooth_window <- 5 # paramètre de lissage

# paramètres de trainée
n_trail <- 6        # nombre de silhouettes visibles
trail_step   <- 4      # espacement entre deux silhouettes
alpha_max <- 1      # silhouette actuelle
alpha_min <- 0.10   # silhouette la plus ancienne

# output name ----
output_dir <-
  "output/animation_groupee_raw"
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_dir,"frames"),recursive=TRUE,showWarnings=FALSE)

##
# Function definition ----
##

# width profile 
width_profile <- function(s){
  approx(
    x=c(
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
    y=c(
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
    xout=s
  )$y
}

# distance
distance2D <- function(
  x1,
  y1,
  x2,
  y2
){sqrt((x2-x1)^2+(y2-y1)^2)}

# renvoie le minimum en ignorant les NA
safe_min <- function(x){
  min(x,na.rm=TRUE)
}

safe_max <- function(x){
  max(x,na.rm=TRUE)
}

# Fonction utilitaire
safe_numeric <- function(x){
  suppressWarnings(as.numeric(x))
}

# Placement automatique
compute_offsets <- function(n){
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n/ncol)
  grid <- expand.grid(
    row=1:nrow,
    col=1:ncol
  )
  grid <- grid[1:n,]
  grid$dx <- (grid$col-1)*spacing_x
  grid$dy <- -(grid$row-1)*spacing_y
  grid
}

# Interpolation de frame
# pour passer à une animation plus fluide 
interpolate_fish <- function(fish,factor = 4){
  n <- nrow(fish)
  res <- list()
  k <- 1
  for(i in 1:(n-1)){
    
    A <- fish[i,]
    B <- fish[i+1,]
    
    for(alpha in seq(0,1,length.out=factor+1)[-(factor+1)]){
      tmp <- A
      cols <- c(
        "nose_x","nose_y",
        "center_x","center_y",
        "tail_x","tail_y"
      )
      for(cl in cols){
        tmp[[cl]] <-
          (1-alpha)*A[[cl]]+
          alpha*B[[cl]]
      }
      res[[k]] <- tmp
      k <- k+1
    }
  }
  res[[k]] <- fish[n,]
  out <- bind_rows(res)
  out$frame <- seq_len(nrow(out))
  out
}

# CONSTRUCTION DE LA COLONNE VERTEBRALE
# INTERPOLATION ENTRE DEUX POINTS
interpolate_segment <- function(
    x1,
    y1,
    x2,
    y2,
    n
){
  
  data.frame(
    x=seq(
      x1,
      x2,
      length.out=n
    ),
    
    y=seq(
      y1,
      y2,
      length.out=n
    )
    
  )
  
}

# RECONSTRUCTION
# Queue --- Centre --- Tête
build_backbone <- function(
    
  nose_x,
  nose_y,
  
  center_x,
  center_y,
  
  tail_x,
  tail_y
  
){
  n1 <- floor(n_backbone/2)
  n2 <- n_backbone-n1+1
  
  seg1 <-
    interpolate_segment(
      tail_x,
      tail_y,
      center_x,
      center_y,
      n1)
  
  seg2 <-
    interpolate_segment(
      center_x,
      center_y,
      nose_x,
      nose_y,
      n2)

  backbone <-rbind(seg1,seg2[-1,])
  
  # spline
  t <- seq_len(nrow(backbone))
  sx <-smooth.spline(t,backbone$x, spar=spar_value)
  sy <-smooth.spline(t,backbone$y,spar=spar_value)
  tt <-seq( min(t),max(t),length.out=n_interp)
  xx <- predict(sx,tt)$y
  yy <- predict(sy,tt)$y
  data.frame(
    x=xx,
    y=yy
  )
}

# CALCUL DES NORMALES 
compute_normals <- function(backbone){
  dx <- c(
    diff(backbone$x),
    tail(diff(backbone$x),1)
  )
  dy <- c(
    diff(backbone$y),
    tail(diff(backbone$y),1)
  )
  L <- sqrt(dx^2+dy^2)
  L[L==0] <- 1
  tx <- dx/L
  ty <- dy/L
  nx <- -ty
  ny <- tx
  
  backbone$tx <- tx
  backbone$ty <- ty
  
  backbone$nx <- nx
  backbone$ny <- ny
  
  backbone
}

# ABSCISSE CURVILIGNE 
compute_curvilinear <- function(backbone){
  ds <- c(
    
    0,
    
    sqrt(
      
      diff(backbone$x)^2+
        
        diff(backbone$y)^2
      
    )
    
  )
  
  s <- cumsum(ds)
  
  if(max(s)>0){
    
    s <- s/max(s)
    
  }
  
  backbone$s <- s
  
  backbone
  
}

# CONSTRUCTION COMPLETE
build_backbone_complete <- function(
    nose_x,
    nose_y,
    center_x,
    center_y,
    tail_x,
    tail_y,
    n_points = 40
){
  
  xs <- c(nose_x,center_x,tail_x)
  ys <- c(nose_y,center_y,tail_y)
  
  if(any(!is.finite(xs)) || any(!is.finite(ys))){
    return(NULL)
  }
  
  sx <- spline(
    x = 1:3,
    y = xs,
    n = n_points,
    method = "natural"
  )
  
  sy <- spline(
    x = 1:3,
    y = ys,
    n = n_points,
    method = "natural"
  )
  
  x <- sx$y
  y <- sy$y
  
  dx <- c(diff(x), tail(diff(x),1))
  dy <- c(diff(y), tail(diff(y),1))
  
  L <- sqrt(dx^2 + dy^2)
  L[L == 0] <- 1e-8
  
  tx <- dx / L
  ty <- dy / L
  
  nx <- -ty
  ny <- tx
  
  ds <- c(
    0,
    cumsum(
      sqrt(diff(x)^2 + diff(y)^2)
    )
  )
  if(max(ds) > 0){
    s <- ds / max(ds)
  }else{
    s <- rep(0, length(ds))
  }
  backbone <- data.frame(
    x = x,
    y = y,
    s = s,
    tx = tx,
    ty = ty,
    nx = nx,
    ny = ny
  )
  return(backbone)
}


# offsets
offsets <- compute_offsets(length(names))
print(offsets)

# raw file lecture ----
input_dir <- "data/raw"
find_column <- function(df, pattern){
  idx <- grep(
    pattern,
    names(df),
    ignore.case = TRUE
  )
  if(length(idx)==0){
    stop(
      paste(
        "Impossible de trouver :",pattern))
  }
  idx[1]
}

# LISTE DES INDIVIDUS
raw_data_list <- list()

for(i in seq_along(names)){
  cat("\n----------------------------\n")
  cat("Lecture :",names[i],"\n")
  file <- file.path(
    input_dir,
    paste0(
      "Raw data-FG_calibration_comportement-Trial   ",
      names[i],
      ".xlsx"
    )
  )
  
  if(!file.exists(file)){
    stop(
      paste(
        "Fichier absent :",
        file
      )
    )
  }
  
  ## import ----

  df <- read_excel(
    file,
    skip = 47,
    col_names = TRUE,
    guess_max = 100000
  )
  df <- as.data.frame(df)
  
  # DETECTION DES COLONNES
  xnose <-
    find_column(
      df,
      "X.*nose"
    )
  ynose <-
    find_column(
      df,
      "Y.*nose"
    )
  xcenter <-
    find_column(
      df,
      "X.*center"
    )
  ycenter <-
    find_column(
      df,
      "Y.*center"
    )
  xtail <-
    find_column(
      df,
      "X.*tail"
    )
  ytail <-
    find_column(
      df,
      "Y.*tail"
    )

  # EXTRACTION
  fish <- data.frame(
    nose_x=
      safe_numeric(
        df[[xnose]]
      ),
    nose_y=
      safe_numeric(
        df[[ynose]]
      ),
    center_x=
      safe_numeric(
        df[[xcenter]]
      ),
    center_y=
      safe_numeric(
        df[[ycenter]]
      ),
    tail_x=
      safe_numeric(
        df[[xtail]]
      ),
    tail_y=
      safe_numeric(
        df[[ytail]]
      )
  )

  # SUPPRESSION DES LIGNES INVALIDES
  fish$nose_x <- na.approx(fish$nose_x,na.rm=FALSE)
  fish$nose_y <- na.approx(fish$nose_y,na.rm=FALSE)
  
  fish$center_x <- na.approx(fish$center_x,na.rm=FALSE)
  fish$center_y <- na.approx(fish$center_y,na.rm=FALSE)
  
  fish$tail_x <- na.approx(fish$tail_x,na.rm=FALSE)
  fish$tail_y <- na.approx(fish$tail_y,na.rm=FALSE)
  
  fish <- fish[complete.cases(fish),]
  n_total <- nrow(fish)
  
  n_keep <- floor(n_total/3)
  
  # time parameter ----
  frame_end <- min(frame_end,nrow(fish))
  fish <- fish[seq(
      frame_start,
      frame_end,
      by=frame_step),
  ]
  rownames(fish) <- NULL
  
  fish$frame <-seq_len(nrow(fish))
  fish$id <-names[i]
  cat("Frames conservées :",nrow(fish),"\n")
  
  fish$nose_x   <- rollmean(fish$nose_x,   smooth_window, fill="extend")
  fish$nose_y   <- rollmean(fish$nose_y,   smooth_window, fill="extend")
  
  fish$center_x <- rollmean(fish$center_x, smooth_window, fill="extend")
  fish$center_y <- rollmean(fish$center_y, smooth_window, fill="extend")
  
  fish$tail_x   <- rollmean(fish$tail_x,   smooth_window, fill="extend")
  fish$tail_y   <- rollmean(fish$tail_y,   smooth_window, fill="extend")
  
  fish <- interpolate_fish(
    fish,
    factor = frame_interpolation   # à modifier selon la fluidité souhaitée
  )
  
  raw_data_list[[i]] <- fish
}

# NOMBRE COMMUN DE FRAMES
n_frames <- min(sapply(raw_data_list,nrow))
cat("\nNombre commun de frames :",n_frames,"\n")

# TRONCATURE 

for(i in seq_along(raw_data_list)){
  raw_data_list[[i]] <-
    raw_data_list[[i]][seq_len(n_frames),]
}

# test on the first individual ----

test_backbone <-
  build_backbone_complete(
    raw_data_list[[1]]$tail_x[1],
    raw_data_list[[1]]$tail_y[1],
    raw_data_list[[1]]$center_x[1],
    raw_data_list[[1]]$center_y[1],
    raw_data_list[[1]]$nose_x[1],
    raw_data_list[[1]]$nose_y[1]
  )
cat("\nPoints reconstruits :",nrow(test_backbone),"\n")

build_fish <- function(backbone){
  L <- sum(sqrt(diff(backbone$x)^2+diff(backbone$y)^2))
  max_width <- L*body_width_ratio
  width <- width_profile(backbone$s)*max_width
  dorsal_width <- width*0.45
  ventral_width <- width*0.55
  left_x <- backbone$x+backbone$nx*dorsal_width
  left_y <- backbone$y+backbone$ny*dorsal_width
  right_x <- rev(backbone$x-backbone$nx*ventral_width)
  right_y <- rev(backbone$y-backbone$ny*ventral_width)
  data.frame(x=c(left_x,right_x),y=c(left_y,right_y)
  )
}

fish_all <- list()

for(i in seq_along(raw_data_list)){
  cat("\nConstruction :",names[i],"\n")
  fish_frames <- vector("list",n_frames)
  
  dx <- offsets$dx[i]
  dy <- offsets$dy[i]
  
  for(frame in seq_len(n_frames)){
  
    if(frame>nrow(raw_data_list[[i]]))
      next
    tmp <- raw_data_list[[i]][frame,]
    if(any(is.na(c(
      tmp$nose_x,
      tmp$nose_y,
      tmp$center_x,
      tmp$center_y,
      tmp$tail_x,
      tmp$tail_y
    ))))
      next
    
    backbone <- build_backbone_complete(
      
      tail_x=tmp$tail_x,
      tail_y=tmp$tail_y,
      
      center_x=tmp$center_x,
      center_y=tmp$center_y,
      
      nose_x=tmp$nose_x,
      nose_y=tmp$nose_y,
      
      n_points=40
      
    )
    
    if(is.null(backbone))
      next
    
    fish <- build_fish(backbone)
    
    fish$x <- fish$x+dx
    fish$y <- fish$y+dy
    
    fish$frame <- frame
    fish$id <- names[i]
    
    fish_frames[[frame]] <- fish
    
  }
  
  fish_all[[i]] <- bind_rows(fish_frames)
  
}

fish_all <- bind_rows(fish_all)

# limites (modifiable) ----
xmin <- safe_min(fish_all$x)
xmax <- safe_max(fish_all$x)

ymin <- safe_min(fish_all$y)
ymax <- safe_max(fish_all$y)

cat("\n")

cat("Nombre total de polygones :",length(unique(paste(fish_all$id,fish_all$frame))),"\n")

cat("xmin =",xmin,"\n")
cat("xmax =",xmax,"\n")
cat("ymin =",ymin,"\n")
cat("ymax =",ymax,"\n")

# OVERLAY

overlay_plot <-
  
  ggplot()+
  
  geom_polygon(
    
    data=fish_all,
    
    aes(
      
      x,
      y,
      
      group=interaction(id,frame),
      
      fill=id
      
    ),
    
    alpha=0.20,
    
    colour="black",
    
    linewidth=0.10
    
  )+
  
  coord_equal(
    
    xlim=c(xmin,xmax),
    
    ylim=c(ymin,ymax)
    
  )+
  
  theme_void()+
  
  theme(
    
    legend.position="none"
    
  )

print(overlay_plot)

ggsave(
  
  file.path(
    
    output_dir,
    
    "fish_overlay_raw.pdf"
    
  ),
  
  overlay_plot,
  
  width=12,
  
  height=9
  
)

ggsave(
  
  file.path(
    
    output_dir,
    
    "fish_overlay_raw.svg"
    
  ),
  
  overlay_plot,
  
  width=12,
  
  height=9
  
)

# frame export ----
cat("\nExport des frames...\n")

dir.create(
  file.path(output_dir,"frames"),
  recursive=TRUE,
  showWarnings=FALSE
)

# palette (modifiable) ----
# fish_colours <- rep(fish_fill,length(names)) # même couleur
# fish_colours <- rainbow(length(names)) # palette rainbow
fish_colours <- hcl.colors(length(names),palette = "Dark 3") # palette nuance gris
#fish_colours <- c("#898989","#E0E0E0","#666666","#3D3D3D",
#                  "#0F0F0F","#4F4F4F","#D9D9D9","#C8C8C8")
#fish_colours <- viridis(length(names),option="C") # C = plasma


names(fish_colours) <- names

for(frame in seq_len(n_frames)){
  
  cat(
    "\rFrame",
    frame,
    "/",
    n_frames
  )
  
  #fish <- fish_all[
  #  fish_all$frame==frame,
  #]
  
  frames_keep <-
    frame -
    (0:(n_trail-1))*trail_step
  
  frames_keep <-
    frames_keep[
      frames_keep>=1
    ]
  
  fish <- fish_all[
    fish_all$frame %in% frames_keep,
  ]
  
  fish$alpha <- alpha_min +
    (alpha_max-alpha_min)*
    (
      fish$frame-min(frames_keep)
    )/
    (
      max(frames_keep)-min(frames_keep)+1e-8
    )
  
  p <-
    
    ggplot()+
    
    geom_polygon(
      data=fish,
      aes(x,y,
        group=interaction(id,frame),
        fill=id,
        alpha=alpha),
      colour=fish_border,linewidth=0.30
    )+
    
    scale_fill_manual(
      values=fish_colours
    )+
    
    scale_alpha_identity() +
    
    coord_equal(
      
      xlim=c(xmin,xmax),
      
      ylim=c(ymin,ymax),
      
      expand=FALSE
      
    )+
    
    theme_void()+
    
    theme(
      
      legend.position="none",
      
      plot.background=
        
        element_rect(
          
          fill=background_colour,
          
          colour=background_colour
          
        ),
      
      panel.background=
        
        element_rect(
          
          fill=background_colour,
          
          colour=background_colour
          
        )
      
    )
  
  ggsave(
    
    filename=file.path(
      
      output_dir,
      
      "frames",
      
      sprintf(
        
        "frame_%05d.png",
        
        frame
        
      )
      
    ),
    
    plot=p,
    
    width=figure_width,
    
    height=figure_height,
    
    dpi=dpi_export,
    
    bg=background_colour
    
  )
  
}

cat("\n")
# SVG export ----

cat("\nExport SVG...\n")

for(frame in seq_len(n_frames)){
  cat("\rSVG",frame,"/",n_frames)
  fish <- fish_all[fish_all$frame==frame,]
  
  p <-
    ggplot()+
    geom_polygon(data=fish,aes(x,y,group=interaction(id,frame),fill=id ),
                 colour=fish_border,linewidth=0.30)+
    scale_fill_manual(values=fish_colours)+
    coord_equal(xlim=c(xmin,xmax),ylim=c(ymin,ymax),
      expand=FALSE
    )+
    theme_void()+
    theme(legend.position="none"
    )
  
  ggsave(
    filename=file.path(output_dir,"frames",
      sprintf("frame_%05d.svg",frame
      )
    ),
    plot=p,
    width=figure_width,
    height=figure_height,
    bg="transparent"
  )
}

cat("\n")

## check ----
png_files <- list.files(
  file.path(
    output_dir,
    "frames"
  ),
  pattern="png$",
  full.names=TRUE
)

png_files <- sort(png_files)
cat("\nNombre de PNG :",length(png_files),"\n")

if(length(png_files)!=n_frames){
  warning("Attention : certaines images sont manquantes.")
}

# GIF ----

cat("\n")
cat("----------------------------------\n")
cat("CREATION DU GIF\n")
cat("----------------------------------\n")

png_files <- list.files(
  file.path(
    output_dir,
    "frames"
  ),
  pattern="\\.png$",
  full.names=TRUE
)

png_files <- sort(png_files)

if(length(png_files)==0){
  stop("Aucune image PNG n'a ete trouvee.")
}

gif_name <- file.path(
  output_dir,"fish_animation_raw.gif")
cat("Nombres d'images :",length(png_files),"\n")
cat("Delai :",gif_delay," seconde(s)\n")

gifski(
  png_files,
  gif_file=gif_name,
  delay=gif_delay,
  loop=TRUE,
  progress=TRUE
)

cat("\n")
cat("----------------------------------\n")
cat("GIF CREE\n")
cat("----------------------------------\n")
cat(gif_name,"\n")

# DUREE APPROXIMATIVE 
duration <- length(png_files)*gif_delay
cat("\nDuree approximative : ",round(duration,2)," secondes\n")

# informations ----
cat("\n")
cat("Nombre d'individus : ")
cat(length(names))
cat("\n")
cat("Nombre de frames : ")
cat(n_frames)
cat("\n")
cat("Resolution export : ")
cat(figure_width,"x",figure_height,"\n")
cat("\n")

# final frame export ----
last_frame <- fish_all[
  fish_all$frame==n_frames,
]

p_final <-
  ggplot()+
  geom_polygon(data=last_frame,
    aes(x,y,
      group=interaction(id,frame),
      fill=id
    ),
    colour=fish_border,
    linewidth=0.30
  )+
  scale_fill_manual(
    values=fish_colours
  )+
  coord_equal(xlim=c(xmin,xmax),ylim=c(ymin,ymax),
    expand=FALSE
  )+
  theme_void()+
  theme(
    legend.position="none",
    plot.background=
      element_rect(
        fill=background_colour,
        colour=background_colour
      ),
    panel.background=
      element_rect(
        fill=background_colour,
        colour=background_colour
      )
  )

ggsave(filename=file.path(output_dir,"last_frame.png"),
  plot=p_final,
  width=figure_width,
  height=figure_height,
  dpi=dpi_export,
  bg=background_colour
)

# end ----

cat("\n")
cat("==================================\n")
cat("PROGRAMME TERMINE AVEC SUCCES\n")
cat("==================================\n")
cat("\n")

## END OF SCRIPT 02_animation_group.R
