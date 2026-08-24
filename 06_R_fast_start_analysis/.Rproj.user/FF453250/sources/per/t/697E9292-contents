##############################################
# 18/03/2026 - make.R 
# 
# create directory and call analysis functions
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#############################################
rm(list=ls())

# create directories ----
reps <- c("data", "analyse", "R", "output", "figure", "supplementary_data", "models")
lapply(reps, dir.create, showWarnings = TRUE)
rm(reps)

# charger package et fonctions
devtools::install_deps(upgrade =  'never')
devtools::load_all()

# lancer les analyses
source("R/RUN.R")
RUN("analyse/01.import_data.R")
RUN("analyse/02.angular_ratio_comparison.R")
RUN("analyse/03.acceleration_distance_position_metrics.R")
RUN("analyse/04.GIF.R")
RUN("analyse/05.statistic_models_REML.R") #long time processing ~50 minutes
RUN("analyse/06.statistic_models_results.R")
RUN("analyse/07.statistic_models_ML.R") #long time processing ~50 minutes ?
RUN("analyse/08.GAM_table.R")
RUN("analyse/09.statistical_table_behaviour_fast_start_steady.R")
