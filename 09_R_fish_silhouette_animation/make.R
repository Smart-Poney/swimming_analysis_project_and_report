#
# 22/06/2026 - make.R 
# 
# create directory and call rendering functions
#
# autor: FG
# project: R_fish_silhouette_animation
# latest modification: 22/06/2026
#

rm(list=ls())

# create directories ----
reps <- c("data", "script", "R", "output", "supplementary_data")
lapply(reps, dir.create, showWarnings = TRUE)
rm(reps)

# lister les scripts
files <- list.files(path = ".", pattern = "^[0-9]+.*\\.R$", full.names = TRUE, recursive = T)
file.rename(files, file.path("script", basename(files)))
list.files("script", pattern="\\.R$")

# charger package et fonctions
devtools::install_deps(upgrade =  'never')
devtools::load_all()

# lancer les analyses
source("R/RUN.R")
RUN("script/01_fast_start_animation_whole_fish.R") # paramètres à modifier directement dans le script
RUN("script/02_animation_group.R") # paramètres à modifier directement dans le script
