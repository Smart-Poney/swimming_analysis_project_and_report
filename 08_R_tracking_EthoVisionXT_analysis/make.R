#
# 31/03/2026 - make.R 
# 
# create directory and call analysis functions
#
# autor : FG
# project : R analyse tracking EthoVisionXT
# latest modification: 01/07/26
#

rm(list=ls())
graphics.off()

# create directories ----
reps <- c("data", "analyse", "R", "output", "figure", "supplementary_data", "GIF")
lapply(reps, dir.create, showWarnings = TRUE)
rm(reps)

# charger package et fonctions
devtools::install_deps(upgrade =  'never')
devtools::load_all()

# lancer les analyses
source("R/RUN.R")
RUN("analyse/01.import_and_formate_data.R") # si les 4 premiers fichiers n'existent pas déjà, pb avec cdn dans script1, besoin de lancer deux fois, pb avec import() et read.table() qui ne renvoie pas les memes colonnes dans env.
RUN("analyse/02.data_exploration.R")
RUN("analyse/03.metrics_creation.R")

# animation facultative aux analyses
time_limit <- ask_time_limit(head_angle$time) # individual to visualize via 04.GIF
indiv <- ask_individual((head_angle$ID))      # duration of the animation via 04.GIF
if (ask_yes_no("Launch GIF creation script (04) ?")) { 
  RUN("analyse/04.GIF_for_angle_visualization.R") # can be time-consuming and requires computing ressources
}

RUN("analyse/05.ordination_forest.R")
#RUN("analyse/06.statistic_models.R") # script pour potentielles analyses non effectuées

