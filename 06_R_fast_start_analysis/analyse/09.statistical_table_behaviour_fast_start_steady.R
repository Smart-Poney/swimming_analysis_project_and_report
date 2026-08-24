#
# 02/06/2026 - 09.statistical_table_behaviour_fast_start_steady.R
# 
# To create a table to compare fast_start, behaviour, steady, and morphology (M. Descat)
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))
devtools::install_deps(upgrade =  'never')

# library: ----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(readr)
source("R/PLOT.R")

# load data ----
dfCM <- read.table("output/dfCM.txt", header=TRUE)
dfaccel <- read.table("output/dfaccel.txt", header=TRUE)
dfeff <- read.table("output/dfeff.txt", header=TRUE)
seg <- read.table("output/segments.txt", header=TRUE)
dfcoef <- read.table("output/curve_ratio.txt", header=TRUE)
correct_load() # fonction de PLOT.R pour importer les datasets nécessaires aux plot
origin <- readxl::read_excel("supplementary_data/origin.xlsx", na = "NA")
treatment <- readxl::read_excel("supplementary_data/traitement.xlsx", na = "NA")
bio <- readxl::read_excel("supplementary_data/biometrie.xlsx")
bio$id <- as.factor(bio$id)
stat_bev <- read.table("supplementary_data/statistics_behaviour.txt", header = TRUE) # output of "~Projet\R analyse tracking EthoVisionXT\analyse\05_ordination_forest.R"
load("supplementary_data/dataAMAV03.Rdata") # s'appelle directement data
info_ellerby <- read_excel("supplementary_data/info_ellerby.xlsx")
bief <- read_excel("supplementary_data/bief.xlsx")

# summary ----
#
# on souhaite créer un tableau avec une valeur pour chaque métrique par individu.
# chaque individu a déjà un valeur de : ID, poids, taille, pool, traitement.
# on veut ajouter :
#
# Steady Analysis : variance(Y_max), max(Y_max) température auge, bief auge, frequence_battement, duree_sequence, saut_boite_auge
#
# Unsteady Analysis : 
#         Behaviour : température ellerby, et toutes les métriques contenues dans 'stat_bev'
#        Fast-start : variance(distance parcourue), max(distance parcourue), variance(acceleration), max(acceleration),
#                     variance(segmentation), max(segmentation), variance(ratio), max(ratio), et efficiency
#
#
#

# traitement, pool, et biométrie
identif <- bio %>%
  dplyr::select(ID = id, weight, height) %>%
  mutate(ID = as.factor(ID)) %>%
  distinct()

origin2 <- origin %>%
  mutate(ID = as.factor(ID)) %>%
  distinct()

treatment2 <- treatment %>%
  mutate(ID = as.factor(ID)) %>%
  distinct()

bief2 <- bief %>%
  mutate(ID = as.factor(ID)) %>%
  distinct()

identif <- identif %>%
  left_join(origin2, by = "ID") %>%
  left_join(treatment2, by = "ID") %>%
  left_join(bief2, by = "ID")
#

stat_steady <- data %>%
  mutate(ID = as.factor(id)) %>%
  group_by(ID) %>%
  summarise(variance_amplitude_max_par_segments = var(Y_max, na.rm = TRUE),
    maximum_amplitude_max_par_segments  = max(Y_max, na.rm = TRUE),
    temperature_auge = mean(temperature, na.rm = TRUE),
    frequence_battement = mean(frequence_battement,na.rm = TRUE),
    duree_sequence = mean(duree_sequence,na.rm = TRUE),
    saut_boite_auge = max(saut_boite,na.rm = TRUE),
    .groups = "drop")

names(stat_steady)[names(stat_steady) != "ID"] <- paste0(names(stat_steady)[names(stat_steady) != "ID"],"_steady")

dist <- dfCM %>%
  mutate(ID = as.factor(poisson)) %>%
  group_by(ID) %>%
  summarise(variance_distance_fast_start = var(d_cum_cm,na.rm = TRUE),
    maximum_distance_fast_start = max(d_cum_cm,na.rm = TRUE),
    .groups = "drop")

accel <- dfaccel %>%
  dplyr::select(poisson, accel) %>%
  mutate( ID = as.factor(poisson)) %>%
  group_by(ID) %>%
  summarise(
    variance_acceleration_fast_start = var(accel,na.rm = TRUE),
    maximum_acceleration_fast_start = max(accel,na.rm = TRUE),
    .groups = "drop")

ratio1 <- dfcoef %>%
  dplyr::select(poisson, fs, frame, ratio_snout_tail) %>%
  mutate( ID = as.factor(poisson)) %>%
  group_by(ID) %>%
  summarise(
    variance_ratio_snout_tail_fast_start = var(ratio_snout_tail, na.rm = TRUE),
    maximum_ratio_snout_tail_fast_start = max(ratio_snout_tail, na.rm = TRUE),
    .groups = "drop")

ratio2 <- dfcoef %>%
  dplyr::select(poisson, fs, frame, ratio_ant_post) %>%
  mutate( ID = as.factor(poisson)) %>%
  group_by(ID) %>%
  summarise(
    variance_ratio_ant_post_fast_start = var(ratio_ant_post, na.rm = TRUE),
    maximum_ratio_ant_post_fast_start =max(ratio_ant_post, na.rm = TRUE),
    .groups = "drop")

eff <- dfeff %>%
  mutate(ID = as.factor(poisson)) %>%
  group_by(ID) %>%
  summarise(
    mean_efficiency_fast_start = mean(efficiency, na.rm = TRUE),
    variance_efficiency_fast_start = var(efficiency, na.rm = TRUE),
    maximum_efficiency_fast_start = max(efficiency, na.rm = TRUE),
    .groups = "drop")

segmentation <- seg %>%
  mutate(ID = as.factor(poisson)) %>%
  group_by(ID) %>%
  summarise(
    variance_angle_cum_diff_segmentation_fast_start = var(angle_cum_diff, na.rm = TRUE),
    maximum_angle_cum_diff_segmentation_fast_start = max(angle_cum_diff, na.rm = TRUE),
    .groups = "drop")

ellerby <- info_ellerby %>%
  mutate( ID = as.factor(ID),
          'Temperature_Ellerby' = t_aqua_ellerby)

stat_bev$ID[stat_bev$ID == "63_bis"] <- "63"
stat_bev <- stat_bev |> dplyr::select(!c(height, weight, pool, treatment))
stat_bev <- stat_bev %>%
  mutate(ID = as.factor(ID)) %>%
  group_by(ID) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character), first),
    .groups = "drop"
  )

names(stat_bev) <- names(stat_bev) |> gsub("\\.", "_", x = _) |> gsub(" ", "_", x = _) |> make.names(unique = TRUE)
names(stat_bev)[names(stat_bev) != "ID"] <- paste0(names(stat_bev)[names(stat_bev) != "ID"],"_behaviour")

# verification ----
str(identif)
str(stat_bev)
str(stat_steady)
str(dist)
str(accel)
str(eff)
str(ratio1)
str(ratio2)
str(segmentation)
str(ellerby)

# left join ----
stat_individual <- identif %>%
  left_join(stat_steady, by = "ID") %>%
  left_join(dist,by = "ID") %>%
  left_join(accel,by = "ID") %>%
  left_join(ratio1,by = "ID") %>%
  left_join(ratio2,by = "ID") %>%
  left_join(eff,by = "ID") %>%
  left_join(ellerby,by = "ID") %>%
  left_join(stat_bev,by = "ID") %>%
  left_join(segmentation,by= "ID")

sort(
  colSums(is.na(stat_individual)),
  decreasing = TRUE
)

# save ----
write.csv(stat_individual,"output/stat_individual.csv",row.names = FALSE)
write.table(stat_individual,"output/stat_individual.txt")
saveRDS(stat_individual,"output/stat_individual.rds")

## FIN SCRIPT 06_Statistical_Table_behaviour_fast_start_steady.R

