#
# 08/04/26 - 03.metrics_creation.R
#
# To create metrics and variable of interest
#
# auteur: FG
# project: tracking_EthoVisionXT_analysis
# latest modification: 30/06/26
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))
graphics.off()

# library: ----
library(dplyr)
library(animation)
library(ggplot2)
library(ggExtra)
library(patchwork)
source("R/coef_calculus.R")
source("R/ask.R")
source("R/goodloop.R")

# paths ----
input_folder <- "data"
output_folder <- "output"
interpolated_input_folder <- "data_interpolated"
supp_input_folder <- "supplementary_data"

# load files ----
if (exists("cdn2")) {
  print("files cdn2 ok")
} else {
  cdn2 <- read.table("output/cdn2.txt", header = TRUE)
}
if (exists("trk2")) {
  print("files trk2 ok")
} else {
  trk2 <- read.table("output/trk2.txt", header = TRUE)
}
if (exists("stats2")) {
  print("files stats2 ok")
} else {
  stats2 <- read.table("output/stats2.txt", header = TRUE)
}

# pre script to avoid reloading data each time
trk3 <- trk2
cdn3 <- cdn2
stats3 <- stats2
coor <- cdn2[,c(2,6,13:18)]
head(coor)
#Snoutcoor <- coor[,c(1,2,5,6)]
#Centercoor <- coor[,c(1,2,3,4)]
#Tailcoor <- coor[,c(1,2,7,8)]


#
# TO CALCULATE ANGLES SNOUT-CENTER, CENTER-TAIL, CURVATURE -> SNOUT-CENTER-TAIL
# curvature angles ----
#

head_angle <- cdn3[,c(2,6,13:16)] # df pour le calcul angle tête-centre
str(head_angle)
colnames(head_angle) <- c("ID", "time", "X2", "Y2", "X1", "Y1") # renommer
head_angle <- head_angle[,c("ID", "time", "X1", "Y1", "X2", "Y2")] # ordonner, centre en 2e point toujours
head(head_angle) # check
head_angle <- coef_calculus(head_angle)
plot(head_angle$angle[head_angle$ID==15]~head_angle$time[head_angle$ID==15])

tail_angle <- cdn3[,c(2,6,13:14,17:18)] # df pour le calcul angle centre-queue
str(tail_angle)
colnames(tail_angle) <- c("ID", "time", "X2", "Y2", "X1", "Y1") # renommer
tail_angle <- tail_angle[,c("ID", "time", "X1", "Y1", "X2", "Y2")] # ordonner, centre en 2e point toujours
head(tail_angle) # check
tail_angle <- coef_calculus(tail_angle)
plot(tail_angle$angle[tail_angle$ID==14]~tail_angle$time[tail_angle$ID==14])

curvature <- angle_diff_abs(tail_angle$angle, head_angle$angle)
plot(curvature[1:10000]) # plus on proche de -3.14 ou 3.14, plus on est droit (angle en rad)

cdn3$head_center_angle <-  head_angle$angle
cdn3$center_tail_angle <-  tail_angle$angle
cdn3$curvature_angle <- curvature
plot(cdn3$curvature_angle[1:10000]) # plus on proche de -3.14 ou 3.14, plus on est droit (angle en rad)

df <- cdn3[,c(2,6,13:21)]
df1 <- df[df$ID==15 & df$recording_time==0.550,]
df2 <- df[df$ID==14 & df$recording_time==0.550,]
plot(df1$X_nose~df1$Y_nose, xlim=c(-10,30), ylim=c(-25,40), col="green", cex=1, pch=4)
points(df1$X_center~df1$Y_center, col="green")
points(df1$X_tail~df1$Y_tail, col="darkgreen")
leg <- c(paste("ID 15, head-center", df1$head_center_angle),
         paste("ID 15, tail-center", df1$center_tail_angle),
         paste("ID 15, curvature", df1$curvature_angle),
         paste("ID 14, head-center", df2$head_center_angle),
         paste("ID 14, tail-center", df2$center_tail_angle),
         paste("ID 14, curvature", df2$curvature_angle))
legend("topright", legend = leg, col = c("darkgreen","darkgreen","darkgreen","darkblue","darkblue","darkblue"),
       pch = c(4,1,NA,4,1,NA))
points(df2$X_nose~df2$Y_nose, col="blue",pch=4)
points(df2$X_center~df2$Y_center, col="blue")
points(df2$X_tail~df2$Y_tail, col="darkblue")

#
# TO CALCULATE AN ONDULATORY MOD OF SWIMMING ACROSS TIME WITH THE CURVATURE_ANGLE
# swimming mode using FFT ----
#

colnames(cdn3)
df <- cdn3[,c("ID","treatment", "recording_time", "head_center_angle", "center_tail_angle","curvature_angle")] 

dftest <- df[df$ID==14,]
plot(curvature_angle ~ recording_time, data=dftest)
dftest$diff_curvature_angle <- c(NA, diff(dftest$curvature_angle)) # diff() calcule directement la diff entre chaque valeur n-1,n ; NA gère la première valeur sans n-1

df <- df %>%
  group_by(ID) %>%
  mutate(diff_curvature_angle = c(NA, diff(curvature_angle)))

dftest <- df[df$ID==14,]
plot(diff_curvature_angle ~ recording_time, data=dftest, type = "l") # illisible
plot(diff_curvature_angle[1:100] ~ recording_time[1:100], data=dftest, type = "l") # apparition d'un mode ?

FastFourier <- function(cloud) {
  print("FastFourierTransformation")
  
  tot <- length(unique(df$ID))
  pb <- txtProgressBar(min = 0, max = tot, style = 3)
  
  freq_max1_list <- numeric(length(unique(df$ID)))
  freq_max2_list <- numeric(length(unique(df$ID)))
  freq_max3_list <- numeric(length(unique(df$ID)))
  freq_max4_list <- numeric(length(unique(df$ID)))
  ids <- unique(df$ID)
  
  for (f in seq_along(unique(df$ID))) {
    
    setTxtProgressBar(pb, f)
    
    id <- ids[f]
    signal <- cloud[df$ID == id] # semblable signal periodique
    fft_result <- fft(na.omit(signal))
    
    samplingFrequency <- 1 / by 
    
    freq <- (0:(length(fft_result)-1)) * samplingFrequency / length(fft_result)
    
    N <- length(fft_result)/2
    ind <- 1:ceiling(N)
    af <- abs(fft_result[ind])/N
    i <- which(af %in% rev(sort(af))[1:4]) # find 4 max frequencies
    max_f <- freq[i]
    
    freq_max1_list[f] <- max_f[1]
    freq_max2_list[f] <- max_f[2]
    freq_max3_list[f] <- max_f[3]
    freq_max4_list[f] <- max_f[4]
    
  }
  close(pb)
  return(data.frame(ID = ids,
                    freq1 = freq_max1_list,
                    freq2 = freq_max2_list,
                    freq3 = freq_max3_list,
                    freq4 = freq_max4_list))
}

period <- NULL # periode encore inconnue car signal inconnu
by <- 20 # pour 20 fps

maxf_list <- FastFourier(df$diff_curvature_angle)
maxf_list 
summary(maxf_list) # freq1 mean -> 0.014 Hz
rep <- 1/0.014 # donc répétition d'un signal de nage (courbure du corps) toutes les 1/0.014 = 71.42s ?

stats3 <- stats3 %>%
  left_join(maxf_list, by = "ID")


#
# TEST FFT() POUR UN INDIVIDU AVEC SCRIPT M. VIGNON
# FFT script M. Vignon ----
#

dftest <- df[df$ID==14,]
signal <- dftest$diff_curvature_angle
plot(dftest$recording_time, signal, type ="l")
plot(dftest$recording_time[1:500], signal[1:500], type ="l") # plus lisible
fft_result <- fft(na.omit(signal)) # fft -> transformation de Fourier
sum(is.na(fft_result))
head(fft_result) # containes ireal number

dfe <- cdn3[cdn3$ID ==14,] # test avec individu 14
plot(dfe$head_center_angle ~ dfe$recording_time)
dfe$diff_head <- c(NA, diff(dfe$head_center_angle))
signal <- dfe$diff_head[which(abs(dfe$diff_head) < 1.5)]
plot(signal)

fft_result <- fft(signal) # fft -> transformation de Fourier
plot(Mod(fft_result), type = "l", main = "FFT Magnitudes")
plot(abs(fft_result)[20:200], type = "l")
which.max(abs(fft_result))
summary(fft_result)

samplingFrequency<-1/0.05
freq <- c(0:(length(fft_result)-1))*samplingFrequency/length(fft_result)
plot(freq,abs(fft_result)/(length(fft_result)/2),type="h")
max <- freq[which.max(abs(fft_result))]
plot(freq,abs(fft_result)/(length(fft_result)/2),type="l",xlim=c(0,max+0.05))
abline(v=max,col="red")

N <- length(fft_result)/2
ind <- 1:ceiling(N)
af <- abs(fft_result[ind])/N  ## or Mod(ffts[ind])
i <- which(af %in% rev(sort(af))[1:2]) # find two max frequencies
maxf <- freq[i]
plot(freq,abs(fft_result)/(length(fft_result)/2),type="l",xlim=c(0, (max(maxf)+0.05) ))
abline(v=maxf,col="red") # f en Hz, period = 1/f
period <- 1/maxf

#
# CALCUL OF CUMULATED ANGLES ALONG TIME -> INTEGRATED ANGLES cumsum(diff())
# integrating curvature angles ----
#

dfe <- cdn3[cdn3$ID ==100,]
sum(is.na(dfe$head_center_angle))
# replace () pour gérer les NA au debut de colonne (attention ne gère pas bien les NA en milieu de colonne)
dfe$cum_head_center_angle_test <- c(NA,cumsum(abs(diff(replace(dfe$head_center_angle, is.na(dfe$head_center_angle), 0))))) 
dfe$cum_curvature_angle <- cumsum(abs(replace(dfe$curvature_angle, is.na(dfe$curvature_angle), 0)))
plot(dfe$cum_head_center_angle_test)
plot(dfe$cum_curvature_angle)

df$ID <- as.factor(df$ID)
df <- df %>%
  group_by(ID) %>%
  mutate(cum_curvature_angle = cumsum(abs(curvature_angle)),
         cum_diff_head_center_angle = c(NA,cumsum(abs(diff(replace(head_center_angle, is.na(head_center_angle), 0))))),
         cum_diff_center_tail_angle = c(NA,cumsum(abs(diff(replace(center_tail_angle, is.na(center_tail_angle), 0))))),
         cum_diff_curvature_angle = c(NA,cumsum(abs(diff(replace(curvature_angle, is.na(curvature_angle), 0))))))

ggplot(df, aes(x =recording_time, y =  cum_diff_head_center_angle, group = ID, colour = treatment)) +  geom_line()
ggplot(df, aes(x =recording_time, y =  cum_diff_center_tail_angle, group = ID, colour = treatment)) +  geom_line()
ggplot(df, aes(x =recording_time, y =  cum_diff_curvature_angle, group = ID, colour = treatment)) +  geom_line()

ptreat <- ggplot(df, aes(x =recording_time, y =  cum_diff_head_center_angle, group = ID, colour = treatment)) +  geom_point()

dens1 <- ggplot(df, aes(x = recording_time, fill = treatment)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none")

dens2 <- ggplot(df, aes(x = cum_diff_head_center_angle, fill = treatment)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

# très lourd
dens1 + patchwork::plot_spacer() + ptreat + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

df <- df %>% # calculer le cumul pour avoir une variable sensible au temps
  group_by(ID) %>%
  mutate(
    cum_head_center_angle = cumsum(abs(head_center_angle)),
    cum_center_tail_angle = cumsum(abs(center_tail_angle)))

smoke <- df %>%
  group_by(ID) %>%
  mutate(max_cum_curvature_angle = max(cum_curvature_angle),
         max_cum_head_center_angle = max(cum_head_center_angle),
         max_cum_center_tail_angle = max(cum_center_tail_angle),
         var_curvature_angle = var(curvature_angle),
         var_head_center_angle = var(head_center_angle),
         var_center_tail_angle = var(center_tail_angle)) %>%
  dplyr::select("ID", "max_cum_curvature_angle", "max_cum_head_center_angle", "max_cum_center_tail_angle",
                "var_curvature_angle", "var_head_center_angle", "var_center_tail_angle")

stats3 <- stats3 %>%
  left_join(unique(smoke), by = "ID")

summary(stats3$max_cum_head_center_angle)
summary(stats3$max_cum_center_tail_angle)
summary(stats3$max_cum_curvature_angle)
summary(stats3$var_curvature_angle)

#
# SAVE DATA ----
#

write.table(cdn3, "output/cdn3.txt", row.names = FALSE)
write.table(stats3, "output/stats3.txt", row.names = FALSE)
write.table(trk3, "output/trk3.txt", row.names = FALSE)
write.table(head_angle, "output/head_angle.txt", row.names = FALSE)
write.table(tail_angle, "output/tail_angle.txt", row.names = FALSE)
write.table(curvature, "output/curvature.txt", row.names = FALSE)

## END OF SCRIPT 03.metrics_creation.R


