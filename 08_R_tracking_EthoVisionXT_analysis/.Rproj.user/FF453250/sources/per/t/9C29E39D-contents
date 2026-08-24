#
# 15/05/2026 - 06.statistic_models.R
# 
# Use GAMM model to modelize response traits of treatment, possibly origin
#
# autor : FG
# project: tracking_EthoVisionXT_analysis
# latest modification: 01/07/26
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))

rm(list=ls())
devtools::install_deps(upgrade =  'never')

# library: ----
library(gam)
library(lme4)
library(mgcv)
library(itsadug)
library(ggplot2)
library(dplyr)
library(bbmle)
library(gratia)
library(MASS)
library(readxl)
library(tidyr)
library(car)
library(MuMIn)
source("R/HighstatLibV6.R")
source("R/AICtab.R")

# paths ----
input_folder <- "data"
output_folder <- "output"
interpolated_input_folder <- "data_interpolated"
supp_input_folder <- "supplementary_data"

# load files ----
if (exists("trk3")) {
  print("files trk3 ok")
} else {
  trk3 <- read.table("output/trk3.txt", header = TRUE)
}
if (exists("cdn3")) {
  print("files cdn3 ok")
} else {
  cdn3 <- read.table("output/cdn3.txt", header = TRUE)
}
if (exists("stats3")) {
  print("files stats3 ok")
} else {
  stats3 <- read.table("output/stats3.txt", header = TRUE)
}

origin <- readxl::read_excel("supplementary_data/origin.xlsx", na = "NA")
treatment <- readxl::read_excel("supplementary_data/traitement.xlsx", na = "NA")
bio <- readxl::read_excel("supplementary_data/biometrie.xlsx")
bio$id <- as.factor(bio$id)

# reminder statistical modelling ----
#
# Rappel : AIC = −2log(L)+2k 
#
# avec L <- vraisemblance du modèle et k <- nombre de paramètres
# on compare l'écart relatif d'AIC entre modèle, pas sa valeur absolue
# on souhaite obtenir l'AIC le plus faible ou le plus négatif 
#
# delta(AIC) < 2    -> modèle équivalents, regarder poids d'AIC
# delta(AIC) 2 < 5  -> différence à modérer, regarder le sens biologique
# delta(AIC)> 7+    -> forte différence entre modèle
#
# cf documentation AIC : https://pmarchand1.github.io/ECL7102/notes_cours/8-Selection_modeles.html
# cf documentation GAM : https://pmarchand1.github.io/ECL8202/notes_cours/07-Modeles_additifs_generalises.html#mod%C3%A8les_additifs_g%C3%A9n%C3%A9ralis%C3%A9s
#
# GAM (General Additive Model) mgcv::gam, possibilité d'utiliser mgcv::bam avec grand jeu de données, plus rapide, plus économe
# on utilise s(x) pour modéliser une spline avec un smoothing parameter (se)
# k = nombre de paramètres/courbes nécessaire pour modéliser l'effet d'un facteur
# edf = effective degres of freedom doit normalement être inférieur à k
#
# y ~ s(x, by = z) + z: ajustement indépendant d’une spline de y vs. x pour chaque niveau du facteur z
# y ~ s(x, z, bs = "fs"): ajustement d’une spline de y vs. x pour chaque niveau du facteur z, avec un paramètre de lissage commun.
# y ~ s(x1, x2): spline en deux dimensions avec paramètre de lissage unique.
# y ~ te(x1, x2): spline en deux dimensions avec paramètre de lissage différent dans chaque dimension.
# y ~ s(x, bs = "re") : ajustement d'une spline de y vs. x avec x comme effet alétoire, décale la courbe d'un niveau de x à l'autre mais ne modifie pas la forme de la courbe
#
#

# rajout de la biométrie
bio <- readxl::read_excel("supplementary_data/biometrie.xlsx")
bio <- bio %>%
  mutate(id = as.factor(id), weight = as.numeric(weight), height = as.numeric(height))
# on teste la corrélation entre height et weight
cor.test(bio$weight, bio$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)
# on ne prendra que height pour la suite

# rajout de l'origine
origin <- readxl::read_excel("supplementary_data/origin.xlsx", na = "NA")
origin <- origin %>%
  dplyr::add_row(ID = 60, origin = NA) %>% # pb avec ID = 60 inexistant
  mutate(origin = as.factor(origin),
         ID = as.factor(ID)) %>%
  rename(pool = origin)
ef <- table(origin$pool)
barres <- barplot(ef, ylim=c(0,max(ef)*1.2), col = c("darkgreen", "blue", "darkred"))
text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")

# rajout du traitement
treatment <- readxl::read_excel("supplementary_data/traitement.xlsx", na = "NA")
treatment <- treatment %>%
  mutate(traitement = as.factor(traitement),
         ID = as.factor(ID)) %>%
  rename(treatment = traitement)

# comparaison origine x traitement
treat <- origin %>%
  left_join(treatment, by = "ID", relationship = "many-to-many")
barplot(table(treat$treatment, treat$pool))
sum(is.na(treat))
eff <- table(treat$treatment, treat$pool)
barress <- barplot(eff, ylim=c(0,max(eff)*2), col = c("darkgreen", "blue", "darkred"))
legend("topright", legend=c("aval", "amont"), col=c("blue", "darkgreen"), pch=15)
text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")

# pre script to avoid reloading data each time
cdn5 <- cdn3
trk5 <- trk3
stats5 <- stats3

#dataset
colnames(cdn5)
GAMdata <- cdn5 %>%
  
  dplyr::select(treatment, ID, zone, recording_time, distance, acceleration,
                turn_angle, head_direction, mobility, body_angle, head_center_angle,
                center_tail_angle, curvature_angle) %>%
  
  mutate ( poisson = as.factor(ID),
           treatment = as.factor(treatment),
           recording_time = as.numeric(recording_time),
           zone = as.factor(zone)) %>%
  rename ( time = recording_time ) %>%
  
  left_join(origin, by = c("ID" = "ID")) %>% # ajouter l'origine
  mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
  
  left_join(bio, by = c("ID" = "id"), relationship = "many-to-many") %>% # ajouter la biométrie
  mutate ( weight = as.numeric(weight),
           height = as.numeric(height),) %>%
  
  droplevels() # pour éviter les niveaux fantomes, problem avec left_join() qui garde les niveaux de facteurs des précédents vecteurs

cor.test(GAMdata$weight, GAMdata$height) # cor = 0.961263 
# tests de multicolinéarité avec HighstatLibV6 : 
str(GAMdata)
corvif(GAMdata[, 4:13]) # -> GVIF important donc on enlève et on refait GVIF, valeur arbitraire GVIF > 3 de Jeager ?
cor1 <- cor(GAMdata[, 4:13], use="complete.obs", method="pearson")
cor1
abs(cor1)>0.6 # corrélation >6 entre mobility/distance, head_direction/center_tail_angle, body_angle/curvature_angle

#
# on cherche à expliquer Y une variable réponse continue quantitative 
# ici, on a plusieurs variables à explorer, donc chacune son modèle, en utilisant mgcv:bam pour éviter temps de calculs beaucoup trop longs (1M de lignes)
# les variables à expliquer sont :
#
# ___R calculated earlier (cf 03_metrics_creation) :
# - Curvature
# - Snout angle
# - Tail angle
# - IF POSSIBLE : Swimming modes detected with FFT
#
# ___EthoVision XT ouput :
# - zonation
# - Mobility/activity
# - Distance from point 0
# - Acceleration
#
# les variables explicatives sont :
#
# - time (continue quantitative)
# - genetic pool : Cauterets (wild mediterranean trouts) vs. Less-Athas (reared atlantic trouts), factor with 2 levels
# - treatment : amont (highly fluctuant flow regime) vs. aval (non-fluctuant flow regime),  factor with 2 levels
# - height/weight highly correlated so we'll use only the height (more physical constraint inside the tank), 
# - ID représente l'identifiant du poisson, effet individuel, pas d'effet direct sur Y, donc à considérer en effet aléatoire
#

#
# MODELE LM NAIF, GAM et GAMM pour 'curvature'
# curvature ----
#
# dataset <- GAMdata, origin, bio, et ?

curve <- GAMdata %>%
  dplyr::select(origin_treatment, treatment, poisson, time, pool, height, weight, curvature_angle) %>%
  rename( Y = curvature_angle)

#
## visualiser la donnée ----
#

str(curve)
summary(curve)
x <- sample(nrow(curve), size = 10000, replace = FALSE)
samp <- curve[x,]
plot(samp$Y ~ samp$time)
abline(lm(Y ~ time, data = samp), col="red", lwd=2) # la régression ne traduit pas très bien l'évolution de la distance
plot(samp$Y~samp$treatment) # on observe à priori pas de grandes différences entre les deux traitement
hist(curve$Y) # distribution ok au vu des données
plot(curve$Y~curve$ID) # quelques diffs mais pas d'ordre à priori
ggplot(samp, aes(x = time, y = Y, color = origin_treatment)) + geom_point() + geom_smooth() # interaction ?
ggplot(samp, aes(x = time, y = Y, color = ID)) + geom_point() + geom_smooth()
# visualiser diff potentielles selon classe de taille et de poids
samp2 <- samp %>%
  mutate(weight_class = ggplot2::cut_number(weight, n = 4,labels = c("léger","moyen-","moyen+","lourd")),
         height_class = ggplot2::cut_number(height, n = 4,labels = c("petit","moyen-","moyen+","grand")))
samp2 <- na.omit(samp2)
ggplot(samp2, aes(x = time, y = Y)) + 
  geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de poids",
       x = "Temps",y = "Y",color = "Classe de poids") # différence selon les classes de poids, dans la dynamique temporelle après 0.10s, sinon quasi même croissance
ggplot(samp2, aes(x = time, y = Y)) +
  geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de taille",
       x = "Temps",y = "Y",color = "Classe de taille") # idem, forte corrélation
# seulement des échantillons de N=10 000 parmi les 1 101 800 observations


#
## approche naïve modèle linéaire ----
#

model0 <- lm(Y ~ treatment + height + pool + poisson + time, data=curve, na.action=na.omit)
drop1(model0, test="F") # on garde 
model1 <- lm(Y ~ time, data=curve, na.action=na.omit)
drop1(model1, test="F") # tout semble significatif, on préfère un modèle non linéaire puisque les variables n'ont pas de relation à priori linéaire -> GAM d'abord
# test de GLM, correspond à GAM sans lissage avec fonctions linéaires
model2 <- glm(Y ~ treatment + height + pool + time + poisson, data=curve)

#
## GAM et GAMM ----
# ON UTILISE BAM, 1M de lignes très grand dataset 
#

# on utilise un GAM avec s() pour lisser les fonctions non linéaires
model3 <- mgcv::bam(Y ~ treatment + pool + height + s(time) + poisson, data=curve, method="REML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model4 <- mgcv::bam(Y ~ treatment + pool + height + s(time) + s(poisson, bs = "re"), data = curve, method = "REML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model5 <- mgcv::bam(Y ~ treatment + pool + height + s(time) + s(poisson, bs = "re"), data = curve, method = "REML")
# ajout interaction temps/traitement
model6 <- mgcv::bam( Y ~ treatment + pool + height + s(time, by = treatment) + s(poisson, bs = "re"), data = curve, method = "REML")
# ajout interaction temps/pool
model7 <- mgcv::bam( Y ~ treatment + pool + height + s(time, by = pool) + s(poisson, bs = "re") ,data = curve, method = "REML")
# tester le modèle GAMM avec une meilleure complexité que GAM et Random Effect
model8 <- mgcv::gamm(Y ~ treatment + pool + height + s(time), random = list(poisson = ~1), data = curve, method = "REML")
# tester le modèle GAMM avec une meilleure complexité que GAM et Random Effect et interaction temps/traitement et temps/pool
model9 <- mgcv::gamm(Y ~ treatment + pool + height + s(time, by = treatment), random = list(poisson = ~1, fs = ~1), data = curve, method = "REML")
model10 <- mgcv::gamm(Y ~ treatment + pool + height + s(time, by = pool), random = list(poisson = ~1, fs = ~1), data = curve, method = "REML")


AIC(model0, model1, model2, model3, model4, model5, model6, model7) 
summary(model8$lme) 
summary(model9$lme) 
summary(model10$lme) 
plot(model7, pages = 1)
summary(model7) 

#
## exploration du meilleur modèle ----
#

model11 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(poisson, bs = "re"), data = segm, method = "REML")
model12 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
model13 <- mgcv::bam( Y ~ pool + height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
model14 <- mgcv::bam( Y ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
model15 <- mgcv::bam( Y ~ segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
model16 <- mgcv::bam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
model17 <- mgcv::bam( Y ~ segment + s(time) + s(fs, bs = "re"), data = segm, method = "REML")

plot(model14, pages = 1)
draw(model14, residuals=TRUE)
concurvity(model14) 
gam.check(model14)  
summary(model14)
model14b <- mgcv::bam( sqrt(Y) ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
gam.check(model14b)
concurvity(model14b)
AIC(model14,model14b)
summary(model14)

# comparer poids AIC ----
gam_list <- list(model4  = model4, model5  = model5, model6  = model6, model7  = model7, model11 = model11, 
                 model12 = model12, model13 = model13, model14 = model14, model15 = model15, model16 = model16, model14b = model14b)
mod_list <- list(model0 = model0, model1 = model1, model2 = model2, model3 = model3, model4  = model4,
                 model5  = model5, model6  = model6, model7  = model7, model8  = model8, model9 = model9, model10 = model10,
                 model11 = model11, model12 = model12, model13 = model13, model14 = model14, model15 = model15, model16 = model16,
                 model17 = model17, model14b = model14b)
AICtab(gam_list) # custom function
AICmultitab(mod_list) # custom function
MuMIn::model.sel(mod_list) # package MuMIn
table_model_final_segmentation <- AICmultitab(mod_list) # to save
SEL_model_final_segmentation <- MuMIn::model.sel(mod_list) # to save
#

model_final_segmentation <- # ?

#
# model_final <- 
# model <- mgcv::gam( Y ~ ?, data = curve, method = "REML")
#
# R-sq.(adj) =    Deviance explained = %
#
#
# relation non linéaire du temps justifié avec edf ?
# variabilité vient : de ?
#
# ici amont concernait le traitement avec flucutation environnementale pendant le développement
# ici Cauterets représente la souche sauvage
#

# visualiser les données avec un geom_smooth utilisant gam

ggplot(segm, aes(x = time, y = Y, color = treatment)) + # plus visuel pour le gam, mais parmi tous les segments
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()


## END OF SCRIPT 06.statistic_models.R

