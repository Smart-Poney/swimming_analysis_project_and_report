#
# 04/06/2026 - 07.statistic_models_ML.R
# 
# Use ML instead of REML to cross validate the model selection
# exact copy of '05.statistic_models.R' with use of ML
#
# REML use restricted evaluation of ML and is usually better and more common, but AIC comparison is better with ML -> cross-validation
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))
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
source("R/PLOT.R")
source("R/AICtab.R")

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
info_ellerby <- read_excel("supplementary_data/info_ellerby.xlsx")
bief <- read_excel("supplementary_data/bief.xlsx")


# reminder statistical moddeling ----
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

# rajout de l'origine
origin <- origin %>%
  dplyr::add_row(ID = 60, origin = NA) %>% # pb avec ID = 60 inexistant
  mutate(origin = as.factor(origin),
         ID = as.factor(ID)) %>%
  rename(pool = origin)

summary(origin)
ef <- table(origin$pool)
barres <- barplot(ef, ylim=c(0,max(ef)*1.2), col = c("darkgreen", "blue", "darkred"))
text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")

# comparaison origine x traitement
treatment <- treatment %>%
  mutate(traitement = as.factor(traitement),
         ID = as.factor(ID)) %>%
  rename(treatment = traitement)

dim(origin)
dim(treatment)
match(origin$ID, treatment$ID)

treat <- origin %>%
  left_join(treatment, by = "ID", relationship = "many-to-many")

barplot(table(treat$treatment, treat$pool))
sum(is.na(treat))
eff <- table(treat$treatment, treat$pool)
barress <- barplot(eff, ylim=c(0,max(eff)*2), col = c("darkgreen", "blue", "darkred"))
legend("topright", legend=c("aval", "amont"), col=c("blue", "darkgreen"), pch=15)
text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")

ellerby <- info_ellerby %>%
  dplyr::select( ID, t_aqua_ellerby) %>%
  mutate( ID = as.factor(ID)) %>%
  rename('temperature_ellerby' = t_aqua_ellerby)

bief2 <- bief %>%
  mutate(ID = as.factor(ID)) %>%
  distinct()

#
# MODELE LM NAIF, GAM et GAMM pour le somme cumulée de différence d'angle par segment angle_cum_diff
# cumulated difference of angle (segment) ----
#
# dataset <- seg, origin, bio, et segm

## dataset ----
colnames(seg)
segm <- seg %>%
  dplyr::select(traitement, poisson, fs, frame, seg, angle_cum_diff) %>%
  mutate ( poisson = as.factor(poisson),
           fs = as.factor(fs),
           frame = as.numeric(frame),
           seg = as.factor(seg),
           traitement = as.factor(traitement)) %>%
  rename ( segment = seg,
           treatment = traitement,
           Y = angle_cum_diff,
           time = frame) %>%
  mutate ( poisson_fs = as.factor(interaction(poisson, fs))) %>%
  
  left_join(origin, by = c("poisson" = "ID")) %>% # ajouter l'origine
  mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
  
  left_join(bio, by = c("poisson" = "id"), relationship = "many-to-one") %>% # ajouter la biométrie
  mutate ( weight = as.numeric(weight),
           height = as.numeric(height),) %>%
  
  left_join(ellerby, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter la temperature ellerby
  mutate ( temperature_ellerby = as.factor(temperature_ellerby)) %>%
  
  left_join(bief2, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter le bief
  mutate ( bief = as.factor(bief)) %>%
  
  droplevels() # pour éviter les niveaux fantomes, problem avec left_join() qui garde les niveaux de facteurs des précédents vecteurs

#
## GAM et GAMM ----
#

# NDT: on a remplacé mgcv::gam par mgcv::bam pour un calcul plus rapide, les conclusions ne varient pas avec utilisation de bam()

# on utilise un GAM avec s() pour lisser les fonctions non linéaires (ici le temps)
model3 <- mgcv::bam( Y ~ treatment + pool + height + s(time) + poisson +  fs + segment, data=segm, method="ML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model4 <- mgcv::bam( Y ~ treatment + pool + height + s(time) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm, method = "ML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model5 <- mgcv::bam( Y ~ treatment + pool + height + s(time) + segment + s(poisson, bs = "re") + s(poisson_fs, bs = "re"), data = segm,method = "ML")
# ajout interaction pool/treatment
model6 <- mgcv::bam( Y ~ treatment + pool + origin_treatment + height + s(time) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "ML")
# ajout interaction temps/traitement
model7 <- mgcv::bam( Y ~ treatment + pool + origin_treatment + height + s(time, by = treatment) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "ML")
# ajout interaction temps/pool
model8 <- mgcv::bam( Y ~ treatment + pool + origin_treatment + height + s(time, by = pool) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "ML")
modelbief <- mgcv::bam( Y ~ treatment + pool + bief + origin_treatment + height + s(time, by = pool) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "ML")
modeltemp <- mgcv::bam( Y ~ treatment + pool + temperature_ellerby + origin_treatment + height + s(time, by = pool) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "ML")

#
# exploration du meilleur modèle, model8 AIC = 295420.5, selection des variables
## exploration of the best model ----
#

model9 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model8, model9) # AIC = 108894.9, valeurs d'AIC équivalentes on prend le plus simple, on peut enlever origin_treatment
model10 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(poisson, bs = "re"), data = segm, method = "ML")
AIC(model8,model10) # AIC = 111859.4 on ne peut pas enlever RE(fs) donc il y a une variabilité lié au fs
model11 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model8,model11) # AIC = 108892.4, valeurs d'AIC équivalentes on prend le plus simple donc on retire RE(poisson)
model12 <- mgcv::bam( Y ~ pool + height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model11,model12) # AIC = 108892.4, valeurs d'AIC identiques on prend le plus simple, donc on retire 'treatment'
model13 <- mgcv::bam( Y ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model12,model13) # AIC = 108892.3, valeurs d'AIC identiques on prend le plus simple, donc on retire 'pool'
model14 <- mgcv::bam( Y ~ segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model13,model14) # AIC = 108893.6, valeurs d'AIC identiques on prend le plus simple, donc on retire 'height' mais attention significatif, risque d'underfitting'
model15 <- mgcv::bam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model13,model15) # AIC = 118770.6, cannot remove 'segment'
model16 <- mgcv::bam( Y ~ segment + s(time) + s(fs, bs = "re"), data = segm, method = "ML")
AIC(model13,model16) # AIC = 110749.4, cannot remove time:pool

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
# additional GAMM ----
#

model6g <-  mgcv::gamm( Y ~ segment + s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = segm, method = "ML")
model7g <-  mgcv::gamm( Y ~ segment + s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = segm, method = "ML")
model8g <-  mgcv::gamm( Y ~ segment + s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = segm, method = "ML")
model9g <-  mgcv::gamm( Y ~ segment + s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1, fs = ~1), data = segm, method = "ML")
model10g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1), data = segm, method = "ML")
model11g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height + treatment + pool, random = list(fs = ~1), data = segm, method = "ML")
model12g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height             + pool, random = list(fs = ~1), data = segm, method = "ML")
model13g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height, random = list(fs = ~1), data = segm, method = "ML")
model14g <- mgcv::gamm( Y ~ segment + s(time, by = pool), random = list(fs = ~1), data = segm, method = "ML")
model15g <- mgcv::gamm( Y ~           s(time, by = pool), random = list(fs = ~1), data = segm, method = "ML")
model16g <- mgcv::gamm( Y ~ segment + s(time), random = list(fs = ~1), data = segm, method = "ML")
model13b <- mgcv::bam( sqrt(Y) ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "ML")
model13c <- mgcv::bam( Y ~ height + segment + s(time, by = pool) + s(time) + s(fs, bs = "re"), data = segm, method = "ML") #model final ajout de s(time), trajectoire commune + déviation par pool
model13d <- mgcv::bam( Y ~ height + segment + s(time, pool, bs = "fs") + s(fs, bs = "re"), data = segm, method = "ML") # model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique
## comparer poids AIC ----
gam_list <- list(model3 = model3, model4 = model4, model5 = model5, model6 = model6, model7 = model7,
                 model8 = model8, model9 = model9, model10 = model10, model11 = model11, model12 = model12,
                 model13 = model13, model14 = model14, model15 = model15, model16 = model16,
                 model13c = model13c, model13d = model13d, modelbief = modelbief, modeltemp = modeltemp)
mod_list <- list(model3 = model3, model4 = model4,
                 model5 = model5, model6 = model6, model7 = model7, model8 = model8, model9 = model9,
                 model10 = model10, model11 = model11, model12 = model12, model13 = model13, model14 = model14,
                 model15 = model15, model16 = model16,
                 model7g = model7g, model8g = model8g, model9g = model9g, model10g = model10g, model11g = model11g,
                 model12g = model12g, model13g = model13g, model14g = model14g, model15g = model15g, model16g = model16g,
                 model13c = model13c, model13d = model13d, modelbief = modelbief, modeltemp = modeltemp)
AICtab(gam_list) # custom function
AICmultitab(mod_list) # custom function
MuMIn::model.sel(mod_list) # package MuMIn
ML_table_model_final_segmentation <- AICmultitab(mod_list) # to save
ML_SEL_model_final_segmentation <- MuMIn::model.sel(mod_list) # to save
##

ML_model_final_segmentation <- model13

#
# MODELE LM NAIF, GAM et GAMM pour la distance cumulé par centimètre d_cum_cm
# cumulative distance ----
#
# dataset <- dfCM, origin, bio, et df

## dataset ----
head(dfCM)
df <- dfCM %>%
  dplyr::select(poisson, fs, time, traitement, d_cum_cm)%>%
  mutate(traitement = as.factor(traitement),
         poisson = as.factor(poisson),
         fs = as.factor(fs),
         time = as.numeric(time)) %>%
  rename(treatment = traitement,
         Y = d_cum_cm) %>%
  mutate ( poisson_fs = as.factor(interaction(poisson, fs))) %>%
  
  left_join(origin, by = c("poisson" = "ID")) %>% # ajouter l'origine
  mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
  
  left_join(bio, by = c("poisson" = "id"), relationship = "many-to-one") %>% # ajouter la biométrie
  mutate ( weight = as.numeric(weight),
           height = as.numeric(height),) %>%
  
  left_join(ellerby, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter la temperature ellerby
  mutate ( temperature_ellerby = as.factor(temperature_ellerby)) %>%
  
  left_join(bief2, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter le bief
  mutate ( bief = as.factor(bief)) %>%
  
  droplevels() # pour éviter les niveaux fantomes, problem avec left_join() qui garde les niveaux de facteurs des précédents vecteurs

#
## GAM et GAMM ----
#

# on utilise un GAM avec s() pour lisser les fonctions non linéaires
model4 <- mgcv::gam( Y ~ treatment + pool + height + s(time) + poisson +  fs, data=df, method="ML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model5 <- mgcv::gam( Y ~ treatment + pool + height + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"), data = df, method = "ML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model6 <- mgcv::gam( Y ~ treatment + pool + height + s(time) +s(poisson, bs = "re") + s(poisson_fs, bs = "re"),data = df,method = "ML")
# ajout interaction origin/traitement
model7 <- mgcv::gam( Y ~ treatment + pool + origin_treatment + height + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "ML")
# ajout interaction temps/traitement
model8 <- mgcv::gam( Y ~ treatment + pool + origin_treatment + height + s(time, by = treatment) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "ML")
# ajout interaction temps/pool
model9 <- mgcv::gam( Y ~ treatment + pool + origin_treatment + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "ML")
modelbief <- mgcv::gam( Y ~ treatment + pool + bief + origin_treatment + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "ML")
modeltemp <- mgcv::gam( Y ~ treatment + pool + temperature_ellerby + origin_treatment + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "ML")

#
# exploration du meilleur modèle, model8 AIC = 6420.971, selection des variables
## exploration of the best model ----
#

model10 <- mgcv::gam( Y ~  treatment + pool + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"), data = df, method = "ML")
AIC(model9, model10) # AIC =   on peut enlever origin_treatment
model11 <- mgcv::gam( Y ~ treatment + pool + height + s(time, by = pool) + s(fs, bs = "re"),data = df,method = "ML")
AIC(model9,model11) # AIC = 6420.731, on peut enlever 'RE(poisson)', même AIC mais model12 plus simple
model12 <- mgcv::gam( Y ~ treatment + pool + height + s(time, by = pool) + s(poisson, bs = "re"),data = df,method = "ML")
AIC(model9,model12) # AIC = 7449.553, on ne peut pas enlever 'RE(fs)'
model13 <- mgcv::gam( Y ~ treatment + pool + s(time, by = pool) + s(fs, bs = "re"),data = df,method = "ML")
AIC(model9,model13) # AIC = 6420.788, on peut enlever 'height', même AIC mais model14 plus simple
model14 <- mgcv::gam( Y ~ pool + s(time, by = pool) + s(fs, bs = "re") ,data = df,method = "ML")
AIC(model9,model14) # AIC = 6420.697, on peut enlever 'treatment', même AIC mais model15 plus simple
model15 <- mgcv::gam( Y ~ s(time, by = pool) + s(fs, bs = "re") ,data = df,method = "ML")
AIC(model9,model15) # AIC = 6423.107, AIC peu différent mais ne permet pas de simplifier
model16 <- mgcv::gam( Y ~ s(time) + s(fs, bs = "re") ,data = df,method = "ML")

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
# additional GAMM ----
#

model7g <-  mgcv::gamm( Y ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = df, method = "ML")
model8g <-  mgcv::gamm( Y ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = df, method = "ML")
model9g <-  mgcv::gamm( Y ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = df, method = "ML")
model10g <- mgcv::gamm( Y ~ s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1, fs = ~1), data = df, method = "ML")
model11g <- mgcv::gamm( Y ~ s(time, by = pool)      + height + treatment + pool, random = list(fs = ~1), data = df, method = "ML")
model12g <- mgcv::gamm( Y ~ s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1), data = df, method = "ML")
model13g <- mgcv::gamm( Y ~ s(time, by = pool)               + treatment + pool, random = list(fs = ~1), data = df, method = "ML")
model14g <- mgcv::gamm( Y ~ s(time, by = pool)                           + pool, random = list(fs = ~1), data = df, method = "ML")
model15g <- mgcv::gamm( Y ~ s(time, by = pool), random = list(fs = ~1), data = df, method = "ML")
model16g <- mgcv::gamm( Y ~ s(time), random = list(fs = ~1), data = df, method = "ML")

model15b <- mgcv::gam(sqrt(Y) ~ pool + s(time, by = pool) + s(fs, bs = "re") ,data = df,method = "ML")
model15c <- mgcv::gam( Y ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = df, method = "ML") # model final ajout de s(time), trajectoire commune + déviation par pool
model15d <- mgcv::gam( Y ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = df, method = "ML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique

## comparer poids AIC ----
gam_list <- list(model4  = model4, model5  = model5, model6  = model6, model7  = model7, model8  = model8,
                 model9 = model9, model10 = model10, model11 = model11, model12 = model12, model13 = model13,
                 model14 = model14, model15 = model15, model16 = model16,
                 model15c = model15c, model15d = model15d, modelbief = modelbief, modeltemp = modeltemp)
mod_list <- list(model4  = model4,
                 model5  = model5, model6  = model6, model7  = model7, model8  = model8, model9 = model9, 
                 model10 = model10, model11 = model11, model12 = model12, model13 = model13, model14 = model14,
                 model15 = model15, model16 = model16,
                 model7g = model7g, model8g = model8g, model9g = model9g, model10g = model10g, model11g = model11g,
                 model12g = model12g, model13g = model13g, model14g = model14g, model15g = model15g, model16g = model16g,
                 model15c = model15c, model15d = model15d, modelbief = modelbief, modeltemp = modeltemp)
AICtab(gam_list) # custom function
AICmultitab(mod_list) # custom function
MuMIn::model.sel(mod_list) # package MuMIn
ML_table_model_final_cumulative_distance <- AICmultitab(mod_list) # to save
ML_SEL_model_final_cumulative_distance <- MuMIn::model.sel(mod_list) # to save
##

ML_model_final_cumulative_distance <- model15c

#
# MODELE LM NAIF, GAM et GAMM pour l'accélération accel
# acceleration ----
#
# dataset <- dfaccel, origin et dfa

## datset ----
head(dfaccel)
str(dfaccel)
dfa <- dfaccel %>%
  dplyr::select(poisson, fs, time, traitement, accel)%>%
  mutate(traitement = as.factor(traitement),
         poisson = as.factor(poisson),
         fs = as.factor(fs),
         time = as.numeric(time)) %>%
  rename(treatment = traitement,
         Y = accel) %>%
  mutate ( poisson_fs = as.factor(interaction(poisson, fs))) %>%
  filter ( !is.na(Y)) %>%
  
  left_join(origin, by = c("poisson" = "ID")) %>% # ajouter l'origine
  mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
  
  left_join(bio, by = c("poisson" = "id"), relationship = "many-to-one") %>% # ajouter la biométrie
  mutate ( weight = as.numeric(weight),
           height = as.numeric(height),) %>%
  
  left_join(ellerby, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter la temperature ellerby
  mutate ( temperature_ellerby = as.factor(temperature_ellerby)) %>%
  
  left_join(bief2, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter le bief
  mutate ( bief = as.factor(bief)) %>%
  
  droplevels() # pour éviter les niveaux fantomes, problem avec left_join() qui garde les niveaux de facteurs des précédents vecteurs

#
# GAM et GAMM pour modéliser relation non linéaire 'time' et Random Effect 'poisson' 'fs'
## GAM et GAMM ----
#

# on utilise un GAM avec s() pour lisser les fonctions non linéaires, ici le temps 'time'
model4 <- mgcv::gam(Y ~ treatment + pool + height + s(time) + poisson +  fs, data=dfa, method="ML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model5 <- mgcv::gam(Y ~ treatment + height + pool + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"), data = dfa, method = "ML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model6 <- mgcv::gam(Y ~ treatment  + height + pool + s(time) +s(poisson, bs = "re") + s(poisson_fs, bs = "re"),data = dfa,method = "ML")
# ajout interaction origin/traitement
model7 <- mgcv::gam(Y ~ treatment + pool + origin_treatment + height + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "ML")
# ajout interaction temps/traitement
model8 <- mgcv::gam(Y ~ treatment + height + pool + origin_treatment + s(time, by = treatment) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "ML")
# ajout interaction temps/pool
model9 <- mgcv::gam(Y ~ treatment + height + pool + origin_treatment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "ML")
modelbief <- mgcv::gam(Y ~ treatment + height + bief + pool + origin_treatment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "ML")
modeltemp <- mgcv::gam(Y ~ treatment + height + temperature_ellerby + pool + origin_treatment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "ML")

#
# exploration du meilleur modèle, model5 AIC = -16707.10, séléction des variables
## exploration of the best model ----
#

model10 <- mgcv::gam(Y ~ treatment + height + pool + s(time) + s(poisson, bs = "re"), data = dfa,method = "ML")
AIC(model5,model10) # on peut enlever fs, même AIC, on garde le modèle le plus simple, model12 AIC = -16707.07
model11 <- mgcv::gam(Y ~ treatment + height + pool + s(time), data = dfa,method = "ML")
AIC(model10,model11) # on peut enlever poisson RE aussi, même AIC, on garde le modèle le plus simple, model13 AIC = -16707.13
model12 <- mgcv::gam(Y ~ treatment + pool + s(time), data = dfa,method = "ML")
AIC(model11,model12) # on peut enlever height, même AIC, on garde le modèle le plus simple, model14 AIC = -16708.81
model13 <- mgcv::gam(Y ~ pool + s(time), data = dfa,method = "ML")
AIC(model12,model13) # a discuter pour valeur diff d'AIC, mais on enlève treatment, meilleur/égal AIC dans model15 plus simple, model15 AIC = -16710.66
model14 <- mgcv::gam(Y ~ s(time), data = dfa,method = "ML")
AIC(model13,model14) # on enlève finalement 'pool', meilleur AIC avec seulement s(time), model16 = -16979.16
model15 <- mgcv::gam(Y ~ s(time, by = pool), data = dfa,method = "ML")
AIC(model14,model15) # pas d'amélioration avec pool:time, model17 = -16704.81
model16 <- mgcv::gam(Y ~ s(time, by = treatment), data = dfa,method = "ML")
AIC(model14,model16) # pas d'amélioration avec treatment:time, model17 = -16966.32
AIC(model5,model10, model14, model15, model16) # modèle GAM 16 semble plus pertinent, pas d'interaction, pas de RE, seulement s(time), le plus simple sans effet fixe ni effet aléatoire

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
# additional GAMM ----
#

model5g <-  mgcv::gamm(Y ~ s(time)                 + height + treatment + pool                   , random = list(poisson = ~1, fs = ~1), data = dfa, method = "ML")
model7g <-  mgcv::gamm(Y ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = dfa, method = "ML")
model8g <-  mgcv::gamm(Y ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = dfa, method = "ML")
model9g <-  mgcv::gamm(Y ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = dfa, method = "ML")
model10g <- mgcv::gamm(Y ~ s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1), data = dfa, method = "ML")
model11g <- mgcv::gamm(Y ~ s(time, by = pool)      + height + treatment + pool, data = dfa, method = "ML")
model12g <- mgcv::gamm(Y ~ s(time, by = pool)               + treatment + pool, data = dfa, method = "ML")
model13g <- mgcv::gamm(Y ~ s(time, by = pool)                           + pool, data = segm, method = "ML")
model14g <- mgcv::gamm(Y ~ s(time)                                            , data = segm, method = "ML")
model15g <- mgcv::gamm(Y ~ s(time, by = pool)                                 , data = segm, method = "ML")
model16g <- mgcv::gamm(Y ~ s(time, by = treatment)                            , data = dfa,method = "ML")

model14b <- mgcv::gam( abs(log(Y)) ~ s(time), data = dfa,method = "ML")
model14c <- mgcv::gam(Y ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = df, method = "ML") # model final ajout de s(time), trajectoire commune + déviation par pool
model14d <- mgcv::gam(Y ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = df, method = "ML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique

## comparer poids AIC ----
gam_list <- list(model4  = model4, model5  = model5, model6  = model6, model7  = model7, model8  = model8,
                 model9 = model9, model10 = model10, model11 = model11, model12 = model12, model13 = model13,
                 model14 = model14, model15 = model15, model16 = model16,
                 model14c = model14c, model14d = model14d,modelbief = modelbief, modeltemp = modeltemp)
mod_list <- list(model4  = model4,
                 model5  = model5, model6  = model6, model7  = model7, model8  = model8, model9 = model9, 
                 model10 = model10, model11 = model11, model12 = model12, model13 = model13, model14 = model14,
                 model15 = model15, model16 = model16,
                 model5g = model5g, model7g = model7g, model8g = model8g, model9g = model9g, model10g = model10g,
                 model11g = model11g, model12g = model12g, model13g = model13g, model14g = model14g, model15g = model15g,
                 model16g = model16g, model14c = model14c, model14d = model14d, modelbief = modelbief, modeltemp = modeltemp)
AICtab(gam_list) # custom function
AICmultitab(mod_list) # custom function
MuMIn::model.sel(mod_list) # package MuMIn
ML_table_model_final_acceleration <- AICmultitab(mod_list) # to save
ML_SEL_model_final_acceleration <- MuMIn::model.sel(mod_list) # to save
##

ML_model_final_acceleration <- model14

#
# MODELE LM NAIF, GAM et GAMM pour coef avec ratio snout/tail
# curvature ratio snout/tail ----
#
# dataset <- dfcoef, origin, bio, snail et snail2


## dataset ----
colnames(dfcoef)
snail <- dfcoef %>%
  dplyr::select(treatment, poisson, fs, frame, ratio_snout_tail) %>%
  mutate ( poisson = as.factor(poisson),
           fs = as.factor(fs),
           frame = as.numeric(frame),
           treatment = as.factor(treatment)) %>%
  rename ( time = frame,
           Y = ratio_snout_tail) %>%
  mutate ( poisson_fs = as.factor(interaction(poisson, fs))) %>%
  
  left_join ( origin, by = c("poisson" = "ID")) %>% # ajouter l'origine
  mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
  
  left_join( bio, by = c("poisson" = "id")) %>% # ajouter la biométrie
  mutate ( weight = as.numeric(weight),
           height = as.numeric(height),) %>%
  
  left_join(ellerby, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter la temperature ellerby
  mutate ( temperature_ellerby = as.factor(temperature_ellerby)) %>%
  
  left_join(bief2, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter le bief
  mutate ( bief = as.factor(bief)) %>%
  
  droplevels()

# on transforme abs() pour éviter un signal opposé qui annulerait le signal entre courbes
snail$absY <- abs(snail$Y) # gérer le signal signé unilatéralement, fats-start gauche ou droite

#
## GAM et GAMM ----
#

# on ajoute s(time) pour relation non linéaire du temps avec Y
model4 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + poisson +  fs, data=snail, method="ML") # s() pour lisser la variable non linéaire
# on change poisson et fs en effets aléatoires, les variables structurent les données mais n'ont pas d'effet fixes à priori
model5 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire
# ajout de origin_treatment
model6 <- mgcv::gam( absY ~ s(time) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML")
# on teste les interactions temps:treatment
model7 <- mgcv::gam( absY ~ s(time, by = treatment) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:pool
model8 <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire
modelbief <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire
modeltemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + temperature_ellerby + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:height
model9 <- mgcv::gam( absY ~ s(time, by = height) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire

#
# exploration du meilleur modèle model7 GAM avec s(time, by = pool) et RE(poisson, fs)
## exploration of the best model ----
#

modelbieftemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + bief + temperature_ellerby + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML") # s() pour lisser la variable non linéaire
AIC(model8, modelbief, modeltemp, modelbieftemp) # on garde bief
model10 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML")
AIC(modelbief, model10) # AIC = 22689.05, on prend le plus simple, on retire origin_treatment
model11 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML")
AIC(modelbief, model11) # AIC = 6578.344, valeurs équivalentes, on prend le plus simple, on retire treatment
model12 <- mgcv::gam( absY ~ s(time, by = pool) + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML")
AIC(modelbief, model12) # AIC = 6578.411, valeurs équivalentes, on prend le plus simple, on retire pool
model13 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="ML")
AIC(modelbief, model13) # AIC = 6576.894, valeurs équivalentes, on prend le plus simple, on retire height
model14 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re"), data=snail, method="ML")
AIC(modelbief, model14) # AIC = 6587.468, AIC supérieur il est plus pertinent de garder RE(fs)
model15 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(fs, bs = "re"), data=snail, method="ML")
AIC(modelbief, model15) # AIC = 6561.590, meilleur AIC sans RE(poisson), on peut retirer
model16 <- mgcv::gam( absY ~ s(time) + bief + s(fs, bs = "re"), data=snail, method="ML")
AIC(model15, model16) # AIC = 6685.697, on garde donc s(time, by=pool) et RE(fs)
model17 <- mgcv::gam( absY ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method = "ML")
AIC(model16, model17) # AIC = 22663.02, on peut enlever bief

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
## additional GAMM ----
#

model6g <-  mgcv::gamm(absY ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model7g <-  mgcv::gamm(absY ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model8g <-  mgcv::gamm(absY ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
#model9g <-  mgcv::gamm(absY ~ s(time, by = height)                                + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model10g <- mgcv::gamm(absY ~ s(time, by = pool)      + height + treatment + pool + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model11g <- mgcv::gamm(absY ~ s(time, by = pool)      + height             + pool + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model12g <- mgcv::gamm(absY ~ s(time, by = pool)      + height                    + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model13g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "ML")
model14g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1), data = snail, method = "ML")
model15g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(fs = ~1), data = snail, method = "ML")
model16g <- mgcv::gamm(absY ~ s(time) + bief , random = list(fs = ~1), data = snail, method = "ML")
model17g <- mgcv::gamm(absY ~ s(time, by = pool), random = list(fs = ~1), data = snail, method = "ML" )

model15b <- mgcv::gam( log(absY) ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method="ML")
model15c <- mgcv::gam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method="ML")
model15d <- mgcv::gam(absY ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = snail, method = "ML") # model final ajout de s(time), trajectoire commune + déviation par pool
model15e <- mgcv::gam(absY ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = snail, method = "ML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique

## comparer poids AIC ----
gam_list <- list(model4  = model4, model5  = model5, model6  = model6, model7  = model7, model8  = model8,
                 model9 = model9, model10 = model10, model11 = model11, model12 = model12, model13 = model13,
                 model14 = model14, model15 = model15, model16 = model16, model15d = model15d, model15e = model15e,
                 modelbief = modelbief, modeltemp = modeltemp, modelbieftemp = modelbieftemp, model17 = model17)
mod_list <- list(model4  = model4,
                 model5  = model5, model6  = model6, model7  = model7, model8  = model8, model9 = model9, 
                 model10 = model10, model11 = model11, model12 = model12, model13 = model13, model14 = model14,
                 model15 = model15, model16 = model16,
                 model6g = model6g, model7g = model7g, model8g = model8g, model10g = model10g,
                 model11g = model11g, model12g = model12g, model13g = model13g, model14g = model14g, model15g = model15g,
                 model16g = model16g, model15d = model15d, model15e = model15e, modelbief = modelbief, modeltemp = modeltemp,
                 modelbieftemp = modelbieftemp, model17 = model17, model17g = model17g)
AICtab(gam_list) # custom function
AICmultitab(mod_list) # custom function
MuMIn::model.sel(mod_list) # package MuMIn
ML_table_model_final_snout_tail_ratio <- AICmultitab(mod_list) # to save
ML_SEL_model_final_snout_tail_ratio <- MuMIn::model.sel(mod_list) # to save
##

ML_model_final_snout_tail_ratio <- model15d

#
# MODELE LM NAIF, GAM et GAMM pour coef2 avec ratio ant/post
# curvature ratio anterior/posterior
#
# dataset <- coef2, origin, bio, antpost et antpost2

## dataset ----
colnames(dfcoef)
antpost <- dfcoef %>%
  dplyr::select(treatment, poisson, fs, frame, ratio_ant_post) %>%
  mutate ( poisson = as.factor(poisson),
           fs = as.factor(fs),
           frame = as.numeric(frame),
           treatment = as.factor(treatment)) %>%
  rename ( time = frame,
           Y = ratio_ant_post) %>%
  mutate ( poisson_fs = as.factor(interaction(poisson, fs))) %>%
  
  left_join ( origin, by = c("poisson" = "ID")) %>% # ajouter l'origine
  mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
  
  left_join( bio, by = c("poisson" = "id")) %>% # ajouter la biométrie
  mutate ( weight = as.numeric(weight),
           height = as.numeric(height),) %>%
  
  left_join(ellerby, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter la temperature ellerby
  mutate ( temperature_ellerby = as.factor(temperature_ellerby)) %>%
  
  left_join(bief2, by = c("poisson" = "ID"), relationship = "many-to-one") %>% # ajouter le bief
  mutate ( bief = as.factor(bief)) %>%
  
  droplevels()

# on transforme abs() pour éviter un signal opposé qui annulerait le signal entre courbes
antpost$absY <- abs(antpost$Y) # gérer le signal signé unilatéralement, fats-start gauche ou droite

#
## GAM et GAMM ----
#

# on ajoute s(time) pour relation non linéaire du temps avec Y
model4 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + poisson +  fs, data=antpost, method="ML") # s() pour lisser la variable non linéaire
# on change poisson et fs en effets aléatoires, les variables structurent les données mais n'ont pas d'effet fixes à priori
model5 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire
# ajout de origin_treatment
model6 <- mgcv::gam( absY ~ s(time) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML")
# on teste les interactions temps:treatment
model7 <- mgcv::gam( absY ~ s(time, by = treatment) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:pool
model8 <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire
modelbief <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire
modeltemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + temperature_ellerby + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:height
model9 <- mgcv::gam( absY ~ s(time, by = height) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire

#
# exploration du meilleur modèle model7 GAM avec s(time, by = pool) et RE(poisson, fs)
## exploration of the best model ----
#

modelbieftemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + bief + temperature_ellerby + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML") # s() pour lisser la variable non linéaire
AIC(model8, modelbief, modeltemp, modelbieftemp) # on garde bief
model10 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML")
AIC(modelbief, model10) # AIC = 22689.05, on prend le plus simple, on retire origin_treatment
model11 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML")
AIC(modelbief, model11) # AIC = 6578.344, valeurs équivalentes, on prend le plus simple, on retire treatment
model12 <- mgcv::gam( absY ~ s(time, by = pool) + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML")
AIC(modelbief, model12) # AIC = 6578.411, valeurs équivalentes, on prend le plus simple, on retire pool
model13 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="ML")
AIC(modelbief, model13) # AIC = 6576.894, valeurs équivalentes, on prend le plus simple, on retire height
model14 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re"), data=antpost, method="ML")
AIC(modelbief, model14) # AIC = 6587.468, AIC supérieur il est plus pertinent de garder RE(fs)
model15 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(fs, bs = "re"), data=antpost, method="ML")
AIC(modelbief, model15) # AIC = 6561.590, meilleur AIC sans RE(poisson), on peut retirer
model16 <- mgcv::gam( absY ~ s(time) + bief + s(fs, bs = "re"), data=antpost, method="ML")
AIC(model15, model16) # AIC = 6685.697, on garde donc s(time, by=pool) et RE(fs)
model17 <- mgcv::gam( absY ~ s(time, by = pool) + s(fs, bs = "re"), data=antpost, method = "ML")
AIC(model16, model17) # AIC = 22663.02, on peut enlever bief

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
# additional GAMM ----
#

model6g <-  mgcv::gamm(absY ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model7g <-  mgcv::gamm(absY ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model8g <-  mgcv::gamm(absY ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
#model9g <-  mgcv::gamm(absY ~ s(time, by = height)                                + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model10g <- mgcv::gamm(absY ~ s(time, by = pool)      + height + treatment + pool + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model11g <- mgcv::gamm(absY ~ s(time, by = pool)      + height             + pool + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model12g <- mgcv::gamm(absY ~ s(time, by = pool)      + height                    + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model13g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "ML")
model14g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1), data = antpost, method = "ML")
model15g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(fs = ~1), data = antpost, method = "ML")
model16g <- mgcv::gamm(absY ~ s(time) + bief , random = list(fs = ~1), data = antpost, method = "ML")
model17g <- mgcv::gamm(absY ~ s(time, by = pool), random = list(fs = ~1), data = antpost, method = "ML" )

model15b <- mgcv::gam( log(absY) ~ s(time, by = pool) + s(fs, bs = "re"), data=antpost, method="ML") # modele test transformation de variable
model15c <- mgcv::gam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data=antpost, method="ML") # modele test transformation de variable
model15d <- mgcv::gam(absY ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = antpost, method = "ML") # model final ajout de s(time), trajectoire commune + déviation par pool
model15e <- mgcv::gam(absY ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = antpost, method = "ML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique

## comparer poids AIC ----
gam_list <- list(model4  = model4, model5  = model5, model6  = model6, model7  = model7, model8  = model8,
                 model9 = model9, model10 = model10, model11 = model11, model12 = model12, model13 = model13,
                 model14 = model14, model15 = model15, model16 = model16, model15d = model15d, model15e = model15e,
                 modelbief = modelbief, modeltemp = modeltemp, modelbieftemp = modelbieftemp, model17 = model17)
mod_list <- list(model4  = model4,
                 model5  = model5, model6  = model6, model7  = model7, model8  = model8, model9 = model9, 
                 model10 = model10, model11 = model11, model12 = model12, model13 = model13, model14 = model14,
                 model15 = model15, model16 = model16,
                 model6g = model6g, model7g = model7g, model8g = model8g, model10g = model10g,
                 model11g = model11g, model12g = model12g, model13g = model13g, model14g = model14g, model15g = model15g,
                 model16g = model16g, model15d = model15d, model15e = model15e, modelbief = modelbief, modeltemp = modeltemp,
                 modelbieftemp = modelbieftemp, model17 = model17, model17g = model17g)
AICtab(gam_list) # custom function
AICmultitab(mod_list) # custom function
MuMIn::model.sel(mod_list) # package MuMIn
ML_table_model_final_ant_post_ratio <- AICmultitab(mod_list) # to save
ML_SEL_model_final_ant_post_ratio <- MuMIn::model.sel(mod_list) # to save
##

ML_model_final_ant_post_ratio <- model17

#
# SAVE and PLOT ----
#

# final models ----
ML_model_final_segmentation
ML_model_final_cumulative_distance
ML_model_final_acceleration
ML_model_final_snout_tail_ratio
ML_model_final_ant_post_ratio

# table for model selection ----
ML_table_model_final_segmentation
ML_SEL_model_final_segmentation

ML_table_model_final_cumulative_distance
ML_SEL_model_final_cumulative_distance

ML_table_model_final_acceleration
ML_SEL_model_final_acceleration

ML_table_model_final_snout_tail_ratio
ML_SEL_model_final_snout_tail_ratio

ML_table_model_final_ant_post_ratio
ML_SEL_model_final_ant_post_ratio

# export table for model selection ----
write.table(ML_table_model_final_segmentation, "models/ML_table_model_final_segmentation.txt", row.names = FALSE )
write.table(ML_table_model_final_cumulative_distance, "models/ML_table_model_final_cumulative_distance.txt", row.names = FALSE )
write.table(ML_table_model_final_acceleration, "models/ML_table_model_final_acceleration.txt", row.names = FALSE )
write.table(ML_table_model_final_snout_tail_ratio, "models/ML_table_model_final_snout_tail_ratio.txt", row.names = FALSE )
write.table(ML_table_model_final_ant_post_ratio, "models/ML_table_model_final_ant_post_ratio.txt", row.names = FALSE )
write.table(as.data.frame(ML_SEL_model_final_segmentation), "models/ML_SEL_model_final_segmentation.txt", row.names = FALSE)
write.table(as.data.frame(ML_SEL_model_final_cumulative_distance), "models/ML_SEL_model_final_cumulative_distance.txt", row.names = FALSE)
write.table(as.data.frame(ML_SEL_model_final_acceleration), "models/ML_SEL_model_final_acceleration.txt", row.names = FALSE)
write.table(as.data.frame(ML_SEL_model_final_snout_tail_ratio), "models/ML_SEL_model_final_snout_tail_ratio.txt", row.names = FALSE)
write.table(as.data.frame(ML_SEL_model_final_ant_post_ratio), "models/ML_SEL_model_final_ant_post_ratio.txt", row.names = FALSE)

# export model to avoid long time calculation ----
saveRDS(ML_model_final_segmentation, "models/ML_model_final_segmentation.rds")
saveRDS(ML_model_final_cumulative_distance, "models/ML_model_final_cumulative_distance.rds")
saveRDS(ML_model_final_acceleration, "models/ML_model_final_acceleration.rds")
saveRDS(ML_model_final_snout_tail_ratio, "models/ML_model_final_snout_tail_ratio.rds")
saveRDS(ML_model_final_ant_post_ratio, "models/ML_model_final_ant_post_ratio.rds")

## FIN SCRIPT 05.3_model_selection_validation.R

