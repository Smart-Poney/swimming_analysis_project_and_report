#
# 03/04/26 - 02.data_exploration.R
#
# Explore data, NA, plots
#
# auteur: FG
# project: tracking_EthoVisionXT_analysis
# latest modification: 30/06/26
#

# Use Ctrl + Maj + o to navigate
to_not_erase <- c("cdn","trk","stats", "RUN", "to_not_erase")
rm(list=setdiff(ls(), to_not_erase))
graphics.off()


# library: ----
library(dplyr)
library(hexbin)
source("R/HighstatLibV6.R") #source A. Jeager <audrey.jeager@univ-reunion.fr>
library(GGally)

# paths ----
input_folder <- "data"
output_folder <- "output"
interpolated_input_folder <- "data_interpolated"
supp_input_folder <- "supplementary_data"

# load files ----
if (exists("trk")) {
  print("files trk ok")
} else {
  trk <- read.table("output/trk.txt", header = TRUE)
}
if (exists("cdn")) {
  print("files cdn ok")
} else {
  cdn <- read.table("output/cdn.txt", header = TRUE)
}
stats <- read.table("output/stats.txt", header = TRUE)
print("files stats ok")

# pre script to avoid reloading data each time
cdn2 <- cdn
trk2 <- trk
stats2 <- stats

#
# EXPLORE TRACKING INFORMATION DATA
# explore trk ----
#

no_data <- trk2$missed_samples - trk2$interpolated_samples
hist(no_data) # less than 0.25 % of samples still missing
trk2[c(1:6)] <- lapply(trk2[c(1:6)], as.factor) # lapply le 'l' tient pour 'liste', marche sur des objets de type liste
trk2$jump[is.na(trk2$jump)] <- 0
trk2$jump <- as.factor(trk2$jump)
summary(trk2)

#
# EXPLORE TRACKING COORDINATES AND VARIABLES DATA
# explore cdn ----
#

table(is.na(cdn2$amont), is.na(cdn2$aval))
cdn2$treatment <- ifelse(!is.na(cdn2$amont), "amont", "aval") # pour avoir une colonne treatment au lieu de 2
cnames <- c("direction", "distance", "acceleration", "turn_angle", "head_direction", "mobility", "body_angle",
            "zone_external", "zone_transition", "zone_center",
            "X_center", "Y_center", "X_nose", "Y_nose", "X_tail", "Y_tail")
cdn2[cnames] <- lapply(cdn2[cnames], as.numeric)
unique(rowSums(cdn2[c("zone_external", "zone_transition", "zone_center")]))
cdn2 <- cdn2 %>%  # pour avoir une colonne zonation au lieu de 3
  mutate(zone = case_when( 
    zone_external == 1 ~ "externe",
    zone_transition == 1 ~ "transition",
    zone_center == 1 ~ "center",
    TRUE ~ NA_character_
  ))
ID_trial <- read.table("supplementary_data/ID_trial.txt", header = TRUE)
attrib <- setNames(ID_trial$ID, ID_trial$trial_name)
cdn2$ID <- attrib[as.character(cdn2$trial_name)]
cdn2[c("trial_name", "treatment", "zone", "ID", "trial_time", "recording_time")] <- lapply(
  cdn2[c("trial_name", "treatment", "zone", "ID", "trial_time", "recording_time")], as.factor)
cdn2 <- subset(cdn2, select= -c(aval, amont, zone_external, zone_transition, zone_center))
cnames <- colnames(cdn2[2:16])
cdn2 <- cdn2[,c("trial_name","ID", "treatment", "zone", cnames)]
unique(cdn2$direction==cdn2$head_direction) # si renvoie TRUE alors les deux colonnes sont égales
cdn2 <- cdn2[ ,names(cdn2) != "direction"]

# analyse exploratoire ----
str(cdn2)
vari <- cdn2[7:12]
summary(vari)
co1 <- cor(vari, use="pairwise.complete.obs", method="pearson") # pariwise.complete.obs uses the non-NA values for cor calculation, complete.obs ignore the entire row whan NA is present
co2 <- cor(vari, use="complete.obs", method="pearson")
abs(co1)>0.7 # 0.7 seuil arbitraire 
abs(co2)>0.7
cor.test(vari$distance, vari$mobility, method="pearson") # cor = 0.689 importante et significative p < 2.2e-16
cor.test(vari$distance, vari$acceleration, method="pearson") # cor = 0.130 faible

# tests de multicolinéarité avec HighstatLibV6 : 
source("R/HighstatLibV6.R")
corvif(vari)  # -> GVIF important donc on enlève et on refait GVIF, valeur arbitraire GVIF > 3 de Jeager ?
plot(hexbin(vari$mobility~vari$distance))
vari <- vari[,-5] # distance parcourue plus intérressante que la mobilité
corvif(vari) # -> aucune variable avec VIF > 3 donc on garde toutes ces variables restantes 
plot(hexbin(cdn2$recording_time~cdn2$distance))

set.seed(1)
sample_cdn2 <- cdn2[sample(nrow(cdn2), 10000, replace = FALSE), ] # temps de calcul trop long avec 1M de lignes, sample de 10 000 pour voir les tendances
vari <- cdn2[8:13]
sample_vari <- vari[sample(nrow(vari), 10000, replace = FALSE), ]
pairs(sample_vari)
plotpairs <- ggpairs(sample_vari)
plotpairs

to_not_erase <- c(to_not_erase, "vari", "plotpairs")

#
# EXPLORE TRACKING STATISTICAL DATA
# explore stats ----
#

str(stats2)
stats2[c("trial_name", "treatment", "ID")] <- lapply(stats2[c("trial_name", "treatment", "ID")], as.factor)
cnames <- colnames(stats2[3:34])
stats2 <- stats2[,c("ID", "trial_name", "treatment", cnames)]
stats2[c(5,7,9,11,13,15)] <- lapply(stats2[c(5,7,9,11,13,15)], as.numeric)
summary(stats2)
source("R/HighstatLibV6.R")
statis <- stats2[4:35]
corvif(statis) # forcement beaucoup de correlations entre moy, max, min, cumul, etc...
boxplot(statis[2:13])
cnames <- colnames(statis[14:32])
colnames(statis) <- c("Distance.moved.Center.point.Total.cm",
                            "External.zone.Frequency",                
                            "External.zone.Cumulative.Duration.s",    
                            "External.zone.Latency.to.First.s",       
                            "External.zone.Cumulative.Duration",    
                            "Transition.zone.Frequency",            
                            "Transition.zone.Cumulative.Duration.s",
                            "Transition.zone.Latency.to.First.s",   
                            "Transition.zone.Cumulative.Duration",
                            "Center.zone.Frequency",                
                            "Center.zone.Cumulative.Duration.s",    
                            "Center.zone.Latency.to.First.s",       
                            "Center.zone.Cumulative.Duration",
                            cnames)

boxplot(statis[c(2,6,10)])
boxplot(statis[c(3,7,11)])
boxplot(statis[c(4,8,12)])
boxplot(statis[c(5,9,13)], outline=FALSE) # outline pour ignorer les valeurs extrèmes visuellement
freq  <- statis[c(2,6,10)]
cum_s <- statis[c(3,7,11)] 
lat   <- statis[c(4,8,12)]
cum   <- statis[c(5,9,13)]
graphics.off()
zonation <- c(freq, cum_s, lat)
log_zonation <- c(log(freq), log(cum_s), log(lat))
boxplot(log_zonation, outline = FALSE, col = c("blue", "green", "purple"), las=2)
legend("bottomleft", legend = c("external zone", "transition zone", "center zone"), pch=15, col = c("blue", "green", "purple"))
boxplot(freq, outline = FALSE, col = c("blue", "green", "purple"), las=1)
boxplot(cum_s, outline = FALSE, col = c("blue", "green", "purple"), las=1)


colnames(statis)
jpeg("figure/statous.jpeg")
par(mfrow=c(2,3))
hist(statis[,1], main = "distance moved total (cm)", xlab = NA)  # distance moved total
hist(statis[,17], main = "acceleration variance (cm.s)", breaks = 50, xlab = NA) # acceleration variance
hist(statis[,21], main = "turn angle center point relative variance (deg)", breaks = 20, xlab = NA) # turn angle center point relative variance
hist(statis[,24], main = "head direction variance (deg)", breaks = 20, xlab = NA) # head direction variance
hist(statis[,28], main = "mobility variance (?)", breaks = 10, xlab = NA) # mobility variance
hist(statis[,32], main = "body angle variance (deg)", breaks = 20, xlab = NA) # body angle variance
dev.off()

#
# CONVERT THE TWO 63 OBS INTO 2 SEPARATE OBS
# obs 63 ----
#

levels(cdn2$ID) <- c(levels(cdn2$ID), "63_bis")
levels(stats2$ID) <- c(levels(stats2$ID), "63_bis")
levels(trk2$ID) <- c(levels(trk2$ID), "63_bis")
cdn2$ID[cdn2$trial_name == 64] <- "63_bis"
stats2$ID[stats2$trial_name == 64] <- "63_bis"
trk2$ID[trk2$trial_name == 64] <- "63_bis"

#
# SAVE DATA ----
#

write.table(trk2, "output/trk2.txt", row.names = FALSE)
write.table(cdn2, "output/cdn2.txt", row.names = FALSE)
write.table(stats2, "output/stats2.txt", row.names = FALSE)

## END OF SCRIPT 02.data_exploration.R
