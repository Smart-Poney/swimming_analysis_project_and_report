#
# 31/03/26 - 01.import_and_formate_data.R
#
# Import and formate data acquired with EthoVision XT 18
#
# auteur: FG
# project: tracking_EthoVisionXT_analysis
# latest modification: 30/06/26
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))

# library: ----
library(readxl)
library(dplyr)
library(readr)
source("R/goodloop.R")

# folder path ----
input_folder <- "data"
dir.create("output", showWarnings = T, recursive= T)
output_folder <- "output"
interpolated_input_folder <- "data_interpolated"
supp_input_folder <- "supplementary_data"

#
# TO IMPORT ETHOVISION DATA ----
#

# helping functions ----

import <- function(cloud) {
  
  tot <- as.numeric(length(cloud))
  pb <- txtProgressBar(min = 0, max = tot, style = 3)
  k <- 1
  list_df <- vector("list", length(cloud))
  for (f in 1:tot) {
    filepath <- cloud[f]
    setTxtProgressBar(pb, k)
    df <- readxl::read_excel(filepath, skip = 47, col_names = FALSE, .name_repair = "minimal", na = "-")
    Cnames <- paste0(df[1,], " (", df[2,], ")")
    Cnames[15:18] <- paste0(df[1, 15:18], " (0-1)")
    colnames(df) <- Cnames
    df <- df[-c(1,2),]
    index <- gsub("Raw data-FG_calibration_comportement-Trial", "", basename(filepath))
    index <- as.numeric(gsub("\\.xlsx$", "", index))
    df$trial_name <- index
    df <- df[, c("trial_name",Cnames)]
    list_df[[f]] <- df
    k <- k + 1
  }
  rm(tot,k,f,index,filepath,names)
  close(pb)
  xy <- bind_rows(list_df)
  return(xy)
}


get_missing_values <- function(cloud) {
  
  pb <- txtProgressBar(min = 0, max = length(cloud), style = 3)
  list_df <- vector("list", length(cloud))
  for (f in seq_along(cloud)) {
    
    filepath <- cloud[f]
    setTxtProgressBar(pb, f)
    df <- read_excel(filepath,col_names = FALSE,.name_repair = "minimal")
    df <- df[, 1:2]
    colnames(df) <- c("key", "value")
    df$value <- trimws(df$value)
    df$key <- trimws(df$key) # retire les espaces dans les valeurs
    
    trial_name <- df$value[df$key == "Trial name"][1] #pb sans [1] R semble lire 2 fois 'trial name'
    missed <- df$value[df$key == "Missed samples"][1]
    interpolated <- df$value[df$key == "Interpolated samples"][1]
    start <- which(df$key == "User-defined Independent Variable")[1]
    sub_df <- df[(start + 1):nrow(df), ]
    end <- which(sub_df$key == "" | is.na(sub_df$key))[1]
    if (!is.na(end)) {
      sub_df <- sub_df[1:(end - 1), ]
    }
    
    row <- as.list(sub_df$value)
    names(row) <- sub_df$key
    row$`trial_name` <- trial_name
    row$`missed_samples` <- missed
    row$`interpolated_samples` <- interpolated
    
    list_df[[f]] <- row
  }
  
  close(pb)
  info <- bind_rows(list_df)
  #v <- rep(c(TRUE,FALSE), as.numeric(length(cloud))) # si jamais les lignes sont doublons
  #tracking_info <- tracking_info[v, ]
  info <- info %>%
    mutate(across(c(6,7,8,9),~ parse_number(.x, locale = locale(decimal_mark = ","))
    ))
  return(info)
}

# import ----

name <- file.path(output_folder, "coordinates.txt")
if (file.exists(name)) {
  print("lecture fichier de coordonnees")
  coordinates <- read.table(name, header=TRUE)
} else {
  files <- base::list.files(input_folder, full.names = TRUE)
  coordinates <- import(files)
  write.table(coordinates, name, row.names = FALSE)
}

name <- file.path(output_folder, "tracking_info.txt")
if (file.exists(name)) {
  print("lecture fichier d'informations")
  tracking_info <- read.table(name, header=TRUE)
} else {
  files <- base::list.files(input_folder, full.names = TRUE)
  tracking_info <- get_missing_values(files)
  write.table(tracking_info, name, row.names = FALSE)
}

name <- file.path(output_folder, "coordinates_interpolated.txt")
if (file.exists(name)) {
  print("lecture fichier de coordonnees interpolees")
  coordinates_interpolated <- read.table(name, header=TRUE)
} else {
  files <- base::list.files(interpolated_input_folder, full.names = TRUE)
  coordinates_interpolated <- import(files)
  write.table(coordinates_interpolated, name, row.names = FALSE)
}

name <- file.path(output_folder, "tracking_info_interpolated.txt")
if (file.exists(name)) {
  print("lecture fichier d'informations interpolees")
  tracking_info_interpolated <- read.table(name, header=TRUE)
} else {
  files <- base::list.files(interpolated_input_folder, full.names = TRUE)
  tracking_info_interpolated <- get_missing_values(files)
  write.table(tracking_info_interpolated, name, row.names = FALSE)
}

name <- file.path(supp_input_folder, "Statistics-FG_calibration_comportement.xlsx")
stats <- readxl::read_excel(name, col_names=TRUE)
col_stats <- colnames(stats[-c(1,2)])
colnames(stats) <- c("treatment", "trial_name", col_stats)
stats$trial_name <- as.numeric(gsub("Trial", "", stats$trial_name))

ID_trial <- tracking_info_interpolated[,c("ID", "trial_name")]
ID_trial$trial_name <- as.numeric(gsub("Trial", "", ID_trial$trial_name))
if (file.exists("supplementary_data/ID_trial.txt")) { print("ID_trial deja existant")
  } else {write.table(ID_trial, "supplementary_data/ID_trial.txt", row.names = FALSE)}
stats <- stats %>%
  left_join(ID_trial, by = "trial_name")
rm(ID_trial)

coordinates_interpolated -> cdn
tracking_info_interpolated -> trk
trk$NAA <- as.numeric(gsub("%","", trk$missed_samples))

files <- base::list.files(interpolated_input_folder, full.names = TRUE)
realNAA <- vector("list", dim(trk)[1])
goodloop(files, function(cloud, i) {
         df <- readxl::read_excel(cloud, skip = 47, col_names = FALSE, .name_repair = "minimal", na = "-")
         value <- sum(is.na(df[,3]))/dim(df)[1] *100
         realNAA[[i]] <<- value 
})
trk$realNAA <- as.numeric(realNAA)
trk$interpolated_samples <- as.numeric(trk$NAA - trk$realNAA)
trk$missed_samples <- as.numeric(trk$NAA)
trk$NA_samples <- realNAA
trk <- trk[,c("trial_name","ID", "Heure", "Date", "treatment", "bief", "weight", "height", "t_auge",
              "t_water", "jump", "timed_jump", "missed_samples", "interpolated_samples")]
colnames(trk) <- c("trial_name","ID", "hour", "date", "treatment", "bief", "weight", "height", "t_auge",
                   "t_water", "jump", "timed_jump", "missed_samples", "interpolated_samples") #missed and interpolated samples in percentage %


name <- file.path(output_folder, "coordinates_interpolated.txt")
if (file.exists(name)) {
  
  cdn <- cdn[,c("trial_name", "Aval..NA.", "Amont..NA.", "Trial.time..s.", "Recording.time..s.", # infos trial et traitement
                "Direction..deg.", "Distance.moved..cm.","Acceleration..0.1.", "Turn.angle..0.1.", # metriques
                "Head.direction..deg.", "Mobility....", "Body.angle..deg.", # metriques
                "In.zone..NA.", "In.zone.2..0.1.", "In.zone.3..0.1.", # pour zonation
                "X.center..cm.", "Y.center..cm.", "X.nose..cm.", "Y.nose..cm.", "X.tail..cm.", "Y.tail..cm.")] # coordonnees à la fin du df
 
} else {
  
  cdn <- cdn[,c("trial_name", "Aval (NA)", "Amont (NA)", "Trial time (s)", "Recording time (s)", # infos trial et traitement
                "Direction (deg)", "Distance moved (cm)","Acceleration (0-1)", "Turn angle (0-1)", # metriques
                "Head direction (deg)", "Mobility (%)", "Body angle (deg)", # metriques
                "In zone (NA)", "In zone 2 (0-1)", "In zone 3 (0-1)", # pour zonation
                "X center (cm)", "Y center (cm)", "X nose (cm)", "Y nose (cm)", "X tail (cm)", "Y tail (cm)")] # coordonnees à la fin du df
  
}

colnames(cdn) <- c("trial_name", "aval", "amont", "trial_time", "recording_time", # infos trial et traitement
                   "direction", "distance","acceleration", "turn_angle", # metriques
                   "head_direction", "mobility", "body_angle", # metriques
                   "zone_external", "zone_transition", "zone_center", # pour zonation
                   "X_center", "Y_center", "X_nose", "Y_nose", "X_tail", "Y_tail") # coordonnees à la fin du df

# save trk, cdn, stats ----
write.table(trk, "output/trk.txt", row.names = FALSE)
write.table(cdn, "output/cdn.txt", row.names = FALSE)
write.table(stats, "output/stats.txt", row.names = FALSE)

# some plots ----
par(mfrow=c(2,2))
plot(weight~height, data = tracking_info_interpolated)
hist(trk$missed_samples)
hist(trk$interpolated_samples)
boxplot(trk$t_water, main = "Water temperature °C")
print("distance metric in cm, angular metrics in deg, acceleration in cm^2/s")


## END OF SCRIPT 01.import_and_formate_data.R
