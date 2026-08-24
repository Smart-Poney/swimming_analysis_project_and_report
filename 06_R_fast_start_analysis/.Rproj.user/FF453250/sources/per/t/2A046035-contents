#
# 24/04/2026 - 05.statistic_models_REML.R
# 
# We use GA(M)M models to fit the best model to our data, using several explanatory factors, including the treatment
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

# plot ----
FishPlot(4)
FishSegPlot(4)
SegPlot(4)
FSAngleCumPlot("002_fs1")
FSRatioSnoutTailPlot("002_fs1", 4)
RatioSnoutTailPlot()
FSRatioAntPostPlot("002_fs1", 4)
RatioAntPostPlot()

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

## visualiser la donnée ----
FishPlot(5)
FishSegPlot(5)
SegPlot(5) # segmentation faite à partir de Akanyeti method Segmentation_algorithm

AngleVarPlot() 
AngleCumPlot()
SepAngleCumPlot()
AngleDensPlot()
AngleFSFishPlot(27) # chosir un individu, ici 27
AngleSegPlot("segment_01_pts_1-27") # chosir un segment, ici "segment_01_pts_1-27"
AngleTreatPlot() 
# il ne semble pas y avoir de séparation dans la valeur finale (densité similaire), 
# mais dans la manière d'y arriver cela reste possible, différence dans la dynamique temporelle

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
# visualiser la donnée
#

str(segm)
summary(segm)
# on cherche à expliquer Y la variation d'angle cumulée par segment = variable à expliquer continue quantitative
# les variables X explicatives sont, la nature du segment, le temps, le traitement, l'origine, le poids et la taille
# on souhaite ajouter fs et poisson en effet aléatoires avec un effet sur Y
# les deux variables facteur à effet aléatoire hiérachisent et structurent la donnée mais n'ont normalement pas d'effet fixe direct sur Y
plot(segm$Y ~ segm$time) # pour tous les segments
abline(lm(Y ~ time, data = segm), col="red", lwd=2) # la régression ne traduit pas très bien l'évolution de la distance
plot(segm$Y~segm$treatment) # on observe à priori pas de grandes différences entre les deux traitement
hist(segm$Y) # distribution ok au vu des données, mais désaxée vers la gauche
hist(log(segm$Y)) # décalage à droite pas bon
hist(sqrt(segm$Y)) # mieux
hist((segm$Y)^2) # encore plus de décalage à gauche pas bon

# on peut transformer en racine carré sqrt() pour une meilleure déviance dans les GAM et une distribution plus homogène des valeurs
# cependant GAM pas soumis à des CA très strictes donc on préfère garder valeur originale
# NDT: la transformation ne modifie pas ni la séléction de modèle ni les conlusions finales
# segm$sqrtY <- sqrt(segm$Y)

plot(segm$Y~segm$poisson) # quelques diffs mais pas d'ordre à priori
plot(segm$Y~segm$fs) # même chose mais hiérarchisé sous poisson -> création de poisson_fs
ggplot(segm, aes(x = time, y = Y, color = origin_treatment)) + geom_point() + geom_smooth() # interaction très intéressante entre treatment et pool, différences visibles
ggplot(segm, aes(x = time, y = Y, color = segment)) + geom_point() + geom_smooth() # différences visibles entre chaque segment
# visualiser diff potentielles selon classe de taille et de poids
segm2 <- segm %>%
  mutate(weight_class = ggplot2::cut_number(weight, n = 4,labels = c("léger","moyen-","moyen+","lourd")),
         height_class = ggplot2::cut_number(height, n = 4,labels = c("petit","moyen-","moyen+","grand")))
segm2 <- na.omit(segm2)
ggplot(segm2, aes(x = time, y = Y)) + 
  geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de poids",
       x = "Temps",y = "Y",color = "Classe de poids") # différence selon les classes de poids, dans la dynamique temporelle après 0.10s, sinon quasi même croissance
ggplot(segm2, aes(x = time, y = Y)) +
  geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de taille",
       x = "Temps",y = "Y",color = "Classe de taille") # idem
# on teste la corrélation entre height et weight
cor.test(segm$weight, segm$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)
# on ne prendra que height pour la suite


#
## approche naïve modèle linéaire ----
#

model0 <- lm( Y ~ treatment + height + pool + poisson + fs +  time + segment, data=segm, na.action=na.omit)
drop1(model0, test="F") # on garde 
model1 <- lm( Y ~ fs + time + segment, data=segm, na.action=na.omit)
drop1(model1, test="F") # tout semble significatif, on préfère un modèle non linéaire puisque les variables n'ont pas de relation à priori linéaire -> GAM d'abord
# test de GLM, correspond à GAM sans lissage avec fonctions linéaires
model2 <- glm( Y ~ treatment + height + pool + time + poisson +  fs + segment, data=segm)

#
## GAM et GAMM  ----
#

# NDT: on a remplacé mgcv::gam par mgcv::bam pour un calcul plus rapide, les conclusions ne varient pas avec utilisation de bam()

# on utilise un GAM avec s() pour lisser les fonctions non linéaires (ici le temps)
model3 <- mgcv::bam( Y ~ treatment + pool + height + s(time) + poisson +  fs + segment, data=segm, method="REML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model4 <- mgcv::bam( Y ~ treatment + pool + height + s(time) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm, method = "REML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model5 <- mgcv::bam( Y ~ treatment + pool + height + s(time) + segment + s(poisson, bs = "re") + s(poisson_fs, bs = "re"), data = segm,method = "REML")
# ajout interaction pool/treatment
model6 <- mgcv::bam( Y ~ treatment + pool + origin_treatment + height + s(time) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "REML")
# ajout interaction temps/traitement
model7 <- mgcv::bam( Y ~ treatment + pool + origin_treatment + height + s(time, by = treatment) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "REML")
# ajout interaction temps/pool
model8 <- mgcv::bam( Y ~ treatment + pool + origin_treatment + height + s(time, by = pool) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "REML")
modelbief <- mgcv::bam( Y ~ treatment + pool + bief + origin_treatment + height + s(time, by = pool) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "REML")
modeltemp <- mgcv::bam( Y ~ treatment + pool + temperature_ellerby + origin_treatment + height + s(time, by = pool) + segment + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm,method = "REML")

AIC(model0, model1, model2, model3, model4, model5, model6, model7, model8, modelbief, modeltemp) # AIC = 108892.3 pour model8 le plus petit et le plus parcimoneieux
plot(model8, pages = 1)
summary(model8) 
# AIC = 295420.5, Estimate intercept = 135.97, seg12 = 65.67, 
# height significatif, et segment aussi (tous les segments), s(time) bien justifié avec edf=7
# RE (poisson et fs) sont significatifs aussi, il semble y avoir une interaction significative pool:time, 
# mais pas d'effet direct de pool ou traitement

#
# exploration du meilleur modèle, model8 AIC = 295420.5, selection des variables
## exploration of the best model ----
#

model9 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model8, model9) # AIC = 108894.9, valeurs d'AIC équivalentes on prend le plus simple, on peut enlever origin_treatment
model10 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(poisson, bs = "re"), data = segm, method = "REML")
AIC(model8,model10) # AIC = 111859.4 on ne peut pas enlever RE(fs) donc il y a une variabilité lié au fs
model11 <- mgcv::bam( Y ~  treatment + pool + height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model8,model11) # AIC = 108892.4, valeurs d'AIC équivalentes on prend le plus simple donc on retire RE(poisson)
model12 <- mgcv::bam( Y ~ pool + height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model11,model12) # AIC = 108892.4, valeurs d'AIC identiques on prend le plus simple, donc on retire 'treatment'
model13 <- mgcv::bam( Y ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model12,model13) # AIC = 108892.3, valeurs d'AIC identiques on prend le plus simple, donc on retire 'pool'
summary(model13) # toutes les variables sont significatives dans model13, on continue exploration mais on peut s'arrêter ici

model14 <- mgcv::bam( Y ~ segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model13,model14) # AIC = 108893.6, valeurs d'AIC identiques on prend le plus simple, donc on retire 'height' mais attention significatif, risque d'underfitting'
model15 <- mgcv::bam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model13,model15) # AIC = 118770.6, cannot remove 'segment'
model16 <- mgcv::bam( Y ~ segment + s(time) + s(fs, bs = "re"), data = segm, method = "REML")
AIC(model13,model16) # AIC = 110749.4, cannot remove time:pool

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
## additional GAMM ----
#

model6g <-  mgcv::gamm( Y ~ segment + s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = segm, method = "REML")
model7g <-  mgcv::gamm( Y ~ segment + s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = segm, method = "REML")
model8g <-  mgcv::gamm( Y ~ segment + s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = segm, method = "REML")
model9g <-  mgcv::gamm( Y ~ segment + s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1, fs = ~1), data = segm, method = "REML")
model10g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1), data = segm, method = "REML")
model11g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height + treatment + pool, random = list(fs = ~1), data = segm, method = "REML")
model12g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height             + pool, random = list(fs = ~1), data = segm, method = "REML")
model13g <- mgcv::gamm( Y ~ segment + s(time, by = pool)      + height, random = list(fs = ~1), data = segm, method = "REML")
model14g <- mgcv::gamm( Y ~ segment + s(time, by = pool), random = list(fs = ~1), data = segm, method = "REML")
model15g <- mgcv::gamm( Y ~           s(time, by = pool), random = list(fs = ~1), data = segm, method = "REML")
model16g <- mgcv::gamm( Y ~ segment + s(time), random = list(fs = ~1), data = segm, method = "REML")

plot(model13, pages = 1)
draw(model13, residuals=TRUE)
concurvity(model13) # pb avec concurvity s(time):poolCauterets = 0.79659825 pour 'worst' et s(fs) = 1 pour 'worst', le rest ok
gam.check(model13) # p-value significatives, mais edf < k  donc underfitting peu probable, gérer transfo log() pas possible, test avec exp() pas concluant
summary(model13) # on test une trasnfo exp()
model13b <- mgcv::bam( sqrt(Y) ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
gam.check(model13b)
concurvity(model13b)
AIC(model13,model13b)
summary(model13b)

model13c <- mgcv::bam( Y ~ height + segment + s(time, by = pool) + s(time) + s(fs, bs = "re"), data = segm, method = "REML") #model final ajout de s(time), trajectoire commune + déviation par pool
model13d <- mgcv::bam( Y ~ height + segment + s(time, pool, bs = "fs") + s(fs, bs = "re"), data = segm, method = "REML") # model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique
AIC(model13, model13c, model13d) # pas d'amélioration

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
table_model_final_segmentation <- AICmultitab(mod_list) # to save
SEL_model_final_segmentation <- MuMIn::model.sel(mod_list) # to save
##

model_final_segmentation <- model13

## summary of the model ----
#
# model_final <- model13 GAM avec AIC = 295418.1
# model14 <- mgcv::gam( Y ~ height + segment + s(time, by = pool) + s(fs, bs = "re"), data = segm, method = "REML")
#
# R-sq.(adj) = 0.83   Deviance explained = 83.1%
#
# que retenir de summary (model_final) :
# pas d'effet direct du traitement, ni de pool    
# pas de temps diff entre traitements, non prise en compte de l'interaction time:traitement 
#
# effet significatif de la taille Pr(>|t|) = 1.99e-09 ***
# effet significatif du segment  Pr(>|t|) entre 0.0413 * et 2e-16 ***
# effet significatif du temps s(time)
# effet significatif de l'intercation time:pool p-value <2e-16 ***
# effet significatif de l'effet aléatoire s(fs, bs ='re') p-value <2e-16 ***
#
# relation non linéaire du temps justifié avec edf > 7
# donc différence selon le segment (attendu) pas encore possible de différencier quel segment est le plus explicatif
# différence dans la DYNAMIQUE TEMPORELLE
# variabilité vient : de la taile, des segments, et du fast-start, rien de facilement discriminant
#
# ici amont concernait le traitement avec flucutation environnementale pendant le développement
# ici Cauterets représente la souche sauvage
# 
# PAS DE DIFF FINALE ENTRE LES TRAITEMENTS ou ENTRE LES POOLS AVEC diff_cum_angle tous segments
# MAIS DIFFERENCE DANS LA DYNAMIQUE TEMPORELLE
#
# attention il faudrait peut-être considérer les segments individuelement !!
#

# visualiser les données angle_cum_diff avec un geom_smooth utilisant gam
AngleTreatPlot()  
  
ggplot(segm, aes(x = time, y = Y, color = treatment)) + # plus visuel pour le gam, mais parmi tous les segments
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()


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


## visualisation des données ----
str(df) 
# on cherche à expliquer Y la distance cumulée variable à expliquer continue quantitative
# les variables X explicatives sont, le temps, le traitement, l'origine, le poids et la taille
# on souhaite ajouter fs et poisson en effet aléatoires avec un effet sur Y
# les deux variables facteur hiérchisent et structurent la donnée mais n'ont normalement pas d'effet fixe direct sur Y
plot(df$Y~df$time)
abline(lm(Y ~ time, data = df), col="red", lwd=2) # la régression ne traduit pas très bien l'évolution de la distance
plot(df$Y~df$treatment) # on observe à priori pas de grandes différences entre les deux traitement
hist(df$Y) # distribution ok au vu des données
hist(log(df$Y)) # décalage à droite
hist(sqrt(df$Y), breaks=20) # peut être mieux
hist(exp(df$Y)) # pas bon

# on peut transformer en racine carré sqrt() pour une meilleure déviance dans les GAM et une distribution plus homogène des valeurs
# cependant GAM pas soumis à des CA très strictes donc on préfère garder valeur originale
# NDT: la transformation ne modifie pas ni la séléction de modèle ni les conlusions finales
# df$sqrtY <- sqrt(df$Y)

plot(df$Y~df$poisson) # quelques diffs mais pas d'ordre à priori
plot(df$Y~df$fs) # même chose mais hiérarchisé sous poisson -> création de poisson_fs
ggplot(df, aes(x = time, y = Y, color = origin_treatment)) + geom_point() + geom_smooth() # interaction très intéressante entre treatment et pool 

df2 <- df %>%
  mutate(weight_class = ggplot2::cut_number(weight, n = 4,labels = c("léger","moyen-","moyen+","lourd")),
    height_class = ggplot2::cut_number(height, n = 4,labels = c("petit","moyen-","moyen+","grand")))
df2 <- na.omit(df2)

ggplot(df2, aes(x = time, y = Y)) + 
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de poids",
    x = "Temps",y = "Y",color = "Classe de poids") # peu de différence selon les classes de poids, peut-être dans la dynamique temporelle après 0.10s
ggplot(df2, aes(x = time, y = Y)) +
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de taille",
       x = "Temps",y = "Y",color = "Classe de taille") # idem
# on teste la corrélation entre height et weight
cor.test(df$weight, df$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)
# on ne prendra que height pour la suite

#
## approche naïve modèle linéaire ----
#

model0 <- lm( Y ~ time, data = df)
# combinaison linéaire
model1 <- lm( Y ~ treatment + pool + height + poisson + fs + time, data=df, na.action=na.omit)
drop1(model1, test="F")
model2 <- lm( Y ~ fs + time, data=df, na.action=na.omit)
drop1(model2, test="F") # on préfère un modèle non linéaire puisque les variables n'ont pas de relation à priori linéaire -> GAM d'abord
# test de GLM, correspond à GAM sans lissage avec fonctions linéaires
model3 <- glm( Y ~ treatment + pool + height + time + poisson +  fs, data=df)

#
## GAM et GAMM ----
#

# on utilise un GAM avec s() pour lisser les fonctions non linéaires
model4 <- mgcv::gam( Y ~ treatment + pool + height + s(time) + poisson +  fs, data=df, method="REML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model5 <- mgcv::gam( Y ~ treatment + pool + height + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"), data = df, method = "REML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model6 <- mgcv::gam( Y ~ treatment + pool + height + s(time) +s(poisson, bs = "re") + s(poisson_fs, bs = "re"),data = df,method = "REML")
# ajout interaction origin/traitement
model7 <- mgcv::gam( Y ~ treatment + pool + origin_treatment + height + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "REML")
# ajout interaction temps/traitement
model8 <- mgcv::gam( Y ~ treatment + pool + origin_treatment + height + s(time, by = treatment) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "REML")
# ajout interaction temps/pool
model9 <- mgcv::gam( Y ~ treatment + pool + origin_treatment + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "REML")
modelbief <- mgcv::gam( Y ~ treatment + pool + bief + origin_treatment + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "REML")
modeltemp <- mgcv::gam( Y ~ treatment + pool + temperature_ellerby + origin_treatment + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = df,method = "REML")
AIC(model0, model1, model2, model3, model4, model5, model6, model7, model8, model9, modelbief, modeltemp) # AIC = 6420.971 pour model9 le plus petit

#
# exploration du meilleur modèle, model8 AIC = 6420.971, selection des variables
## exploration of the best model ----
#

model10 <- mgcv::gam( Y ~  treatment + pool + height + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"), data = df, method = "REML")
AIC(model9, model10) # AIC =   on peut enlever origin_treatment
model11 <- mgcv::gam( Y ~ treatment + pool + height + s(time, by = pool) + s(fs, bs = "re"),data = df,method = "REML")
AIC(model9,model11) # AIC = 6420.731, on peut enlever 'RE(poisson)', même AIC mais model12 plus simple
model12 <- mgcv::gam( Y ~ treatment + pool + height + s(time, by = pool) + s(poisson, bs = "re"),data = df,method = "REML")
AIC(model9,model12) # AIC = 7449.553, on ne peut pas enlever 'RE(fs)'
model13 <- mgcv::gam( Y ~ treatment + pool + s(time, by = pool) + s(fs, bs = "re"),data = df,method = "REML")
AIC(model9,model13) # AIC = 6420.788, on peut enlever 'height', même AIC mais model14 plus simple
model14 <- mgcv::gam( Y ~ pool + s(time, by = pool) + s(fs, bs = "re") ,data = df,method = "REML")
AIC(model9,model14) # AIC = 6420.697, on peut enlever 'treatment', même AIC mais model15 plus simple
model15 <- mgcv::gam( Y ~ s(time, by = pool) + s(fs, bs = "re") ,data = df,method = "REML")
AIC(model9,model15) # AIC = 6423.107, AIC peu différent mais ne permet pas de simplifier
model16 <- mgcv::gam( Y ~ s(time) + s(fs, bs = "re") ,data = df,method = "REML")

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
## additional GAMM ----
#

model7g <-  mgcv::gamm( Y ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = df, method = "REML")
model8g <-  mgcv::gamm( Y ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = df, method = "REML")
model9g <-  mgcv::gamm( Y ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = df, method = "REML")
model10g <- mgcv::gamm( Y ~ s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1, fs = ~1), data = df, method = "REML")
model11g <- mgcv::gamm( Y ~ s(time, by = pool)      + height + treatment + pool, random = list(fs = ~1), data = df, method = "REML")
model12g <- mgcv::gamm( Y ~ s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1), data = df, method = "REML")
model13g <- mgcv::gamm( Y ~ s(time, by = pool)               + treatment + pool, random = list(fs = ~1), data = df, method = "REML")
model14g <- mgcv::gamm( Y ~ s(time, by = pool)                           + pool, random = list(fs = ~1), data = df, method = "REML")
model15g <- mgcv::gamm( Y ~ s(time, by = pool), random = list(fs = ~1), data = df, method = "REML")
model16g <- mgcv::gamm( Y ~ s(time), random = list(fs = ~1), data = df, method = "REML")

AIC(model9, model15, model16) # AIC équivalents, on utilise le modèle le plus parcimonieux, avec le moins de variables
summary(model15)
draw(model15, pages=1)
draw(model15, residuals = TRUE)
concurvity(model15)
gam.check(model15) 
# edf indiqué entre 5 et 7 pour s(time:pool) inférieur à k donc non-linéarité et pas de underfitting
# p-value positive indiquerait un sous ajustement aux données (underfitting) mais edf >> k pas besoin d'augmenter le nombre de para
# on peut essayer une transformation sur Y pour homogénéiser la variance des résidus (hist semble ok quand même)
model15b <- mgcv::gam(sqrt(Y) ~ pool + s(time, by = pool) + s(fs, bs = "re") ,data = df,method = "REML")
concurvity(model15b)
gam.check(model15b) 
AIC(model15,model15b)
summary(model15b)

model15c <- mgcv::gam( Y ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = df, method = "REML") # model final ajout de s(time), trajectoire commune + déviation par pool
model15d <- mgcv::gam( Y ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = df, method = "REML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique
AIC(model15, model15c, model15d)

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
table_model_final_cumulative_distance <- AICmultitab(mod_list) # to save
SEL_model_final_cumulative_distance <- MuMIn::model.sel(mod_list) # to save
##

model_final_cumulative_distance <- model15c
#model_final_cumulative_distance <- 
#  get(table_model_final_cumulative_distance$model[
#    which.min(
#      ifelse(
#        table_model_final_cumulative_distance$AIC == min(table_model_final_cumulative_distance$AIC),
#        nchar(table_model_final_cumulative_distance$formula),
#        -Inf
#      )
#    )
#  ])

## summary of the model ----
#
# model_final <- model15c GAM avec AIC = -1807.3102 
#
# que retenir de summary (model_final) :
# R-sq.(adj) =  0.97   Deviance explained = 97.3%
#
# pas d'effet du traitement 
# effet direct du pool génétique pas detecté : treatment (p-value=0.528  estimate=0.08783) face à intercept (p-value<2e-16 *** estimate=4.05064)    
# mais temps diff entre pool avec une interaction significative pool:time : 
#                                                    s(time):poolCauterets p-value < 2e-16 ***
#                                                    s(time):poolLeesAthas  p-value < 2e-16 ***
# time semble avoir une relation non-linéaire avec edf~6
# donc différence dans la dynamique temporelle mais pas dans la valeur finale !
#
# concernant les effets aléatoires, pas d'effet poisson, mais variaibilité lié au fast-start pv < 2e-16 ***
# ici amont concernait le traitement avec flucutation environnementale pendant le développement et aval sans fluctuation du régime
# ici Cauterets concernait le pool sauvage méditérrannéen avec un effet du temps régulier croissant, 
# et Lees Athas concernait le pool d'élevage atlantique, moins régulier montée plus forte puis redescente
#
# PAS DE DIFF FINALE ENTRE LES TRAITEMENTS AVEC la distance cumulée en cm 
# PAS DE DIFF FINALE ENTRE LES POOLS AVEC la distance cumulée en cm 
# MAIS DIFF DANS LA DYNAMIQUE TEMPORELLE DEPENDANT DU POOL GENETIQUE
#

# visualiser les données d_cum_cm avec un geom_smooth utilisant gam
ggplot(na.omit(df), aes(x = time, y = Y, color = pool)) + # plus visuel pour le gam, mais parmi tous les segments, ne correspond pas au modèle !!
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()

#
# MODELE LM NAIF, GAM et GAMM pour l'accélération accel
# acceleration ----
#
# dataset <- dfaccel, origin et dfa

## dataset ----
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


## visualisation des données ----
str(dfa) 
# on cherche à expliquer Y l'accélération variable à expliquer continue quantitative mais avec aucune donnée t0 et t1 (forcément)
# les variables X explicatives sont, le temps, le traitement, l'origine, le poids, la taille
# on souhaite ajouter fs et poisson en effet aléatoires avec un effet sur Y
# les deux variables facteur hiérchisent et structurent la donnée mais n'ont normalement pas d'effet fixe direct sur Y
plot(dfa$Y~dfa$time)
abline(lm(Y ~ time, data = dfa), col="red", lwd=2) # la régression ne traduit pas très bien l'évolution de l'accélération
plot(dfa$Y~dfa$treatment) # on observe à priori pas de grandes différences entre les deux traitement
plot(dfa$Y~dfa$pool) # on observe à priori pas de grandes différences entre les deux origines
# representation effet pool::treatement
table(dfa$origin_treatment)
a <- lm(Y ~ time, data = dfa[dfa$origin_treatment == unique(dfa$origin_treatment)[1],]) 
b <- lm(Y ~ time, data = dfa[dfa$origin_treatment == unique(dfa$origin_treatment)[2],]) 
c <- lm(Y ~ time, data = dfa[dfa$origin_treatment == unique(dfa$origin_treatment)[3],]) 
d <- lm(Y ~ time, data = dfa[dfa$origin_treatment == unique(dfa$origin_treatment)[4],]) 
ggplot(dfa, aes(x = time, y = Y, color = origin_treatment)) + 
  geom_point() + # graphique peu parlant, difficile de discriminer les groupes
  geom_abline(intercept = a$coefficients[1], slope = a$coefficients[2], col="#F8766D", lwd=1, linetype = 1) +
  geom_abline(intercept = b$coefficients[1], slope = b$coefficients[2], col="#7CAE00", lwd=1, linetype = 1) +
  geom_abline(intercept = c$coefficients[1], slope = c$coefficients[2], col="#00BFC4", lwd=1, linetype = 1) +
  geom_abline(intercept = d$coefficients[1], slope = d$coefficients[2], col="#C77CFF", lwd=1, linetype = 1)
# representation effet pool:treatement sans fonction linéaire
ggplot(dfa, aes(x = time, y = Y, color = origin_treatment)) + geom_point(alpha=0.2) + geom_smooth() # meilleur fit avec une relation non linéaire a priori
hist(dfa$Y, breaks=50) # distribution ok au vu des données, très bonne normalisation
plot(dfa$Y~dfa$poisson) # quelques diffs mais pas d'ordre à priori
plot(dfa$Y~dfa$fs) # même chose mais hiérarchisé sous poisson -> création de poisson_fs
# comparé à la distance on peut tout de même noter que l'accélération est moins variable à priori selon poisson/fs
acf(dfa$Y) # les deux premières mesures de Y très corrélées, sinon reste en dessous de 0.05
acf(dfa$Y[dfa$poisson == "3"])

dfa2 <- dfa %>%
  mutate(weight_class = ggplot2::cut_number(weight, n = 4,labels = c("léger","moyen-","moyen+","lourd")),
         height_class = ggplot2::cut_number(height, n = 4,labels = c("petit","moyen-","moyen+","grand")))
dfa2 <- na.omit(dfa2)

ggplot(dfa2, aes(x = time, y = Y)) + 
  geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de poids",
       x = "Temps",y = "Y",color = "Classe de poids") 
# peu de différence dans la valeur finale selon les classes de poids, dynamique temporelle des 'lourds' avec beaucoup plus de 'wigliness' mais IC superposés
ggplot(dfa2, aes(x = time, y = Y)) +
  geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de taille",
       x = "Temps",y = "Y",color = "Classe de taille") # idem, moins de wigliness des 'grands'
# on teste la corrélation entre height et weight
cor.test(dfa$weight, dfa$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)

#
## approche naïve modèle linéaire ----
#

model0 <- lm(Y ~ time, data = dfa)
summary(model0) # R2 = 0.09, le temps n'explique que 9% de la variance avec relation linéaire simple
AIC(model0) # AIC = -16837.48
# combinaison linéaire
model1 <- lm(Y ~ treatment + height + pool + poisson + fs +  time, data=dfa, na.action=na.omit)
drop1(model1, test="F")
model2 <- lm(Y ~ fs + time, data=dfa, na.action=na.omit) # ici time semble le plus impactant sur l'accélération, et seul significatif
drop1(model2, test="F") # on préfère un modèle non linéaire puisque les variables n'ont pas de relation à priori linéaire -> GAM d'abord
# test de GLM, correspond à GAM sans lissage avec fonctions linéaires
model3 <- glm(Y ~ treatment + height + pool + time + poisson +  fs, data=dfa)

#
# GAM et GAMM pour modéliser relation non linéaire 'time' et Random Effect 'poisson' 'fs'
## GAM et GAMM ----
#

# on utilise un GAM avec s() pour lisser les fonctions non linéaires, ici le temps 'time'
model4 <- mgcv::gam(Y ~ treatment + pool + height + s(time) + poisson +  fs, data=dfa, method="REML") # s() pour lisser la variable non linéaire
# ajout de poisson, fs en Random Effect bs="re", structure les données mais sert de rep et hierarchise avec fs
model5 <- mgcv::gam(Y ~ treatment + height + pool + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"), data = dfa, method = "REML")
# ajout de poisson_fs pour tester si les données sont trop hierarchisés entre poisson et fs
model6 <- mgcv::gam(Y ~ treatment  + height + pool + s(time) +s(poisson, bs = "re") + s(poisson_fs, bs = "re"),data = dfa,method = "REML")
# ajout interaction origin/traitement
model7 <- mgcv::gam(Y ~ treatment + pool + origin_treatment + height + s(time) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "REML")
# ajout interaction temps/traitement
model8 <- mgcv::gam(Y ~ treatment + height + pool + origin_treatment + s(time, by = treatment) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "REML")
# ajout interaction temps/pool
model9 <- mgcv::gam(Y ~ treatment + height + pool + origin_treatment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "REML")
modelbief <- mgcv::gam(Y ~ treatment + height + bief + pool + origin_treatment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "REML")
modeltemp <- mgcv::gam(Y ~ treatment + height + temperature_ellerby + pool + origin_treatment + s(time, by = pool) + s(poisson, bs = "re") + s(fs, bs = "re"),data = dfa,method = "REML")
AIC(model0, model1, model2, model3, model4, model5, model6, model7, model8, model9, modelbief, modeltemp) # AIC = -16837.48 pour model0 le plus petit avec LM, mais on préfère une relation non linéaire car R2 très faible et visuellement meilleur fit GAM
# model5, model6 AIC = -16707.10 et GAM avec relation non linéaire 's(time)', on garde aussi les RE, 'poisson_fs' ne modifie apparramment rien
# parmi les modèles qui intègrent 'time' en relation non linéaire et poisson et fs en random effect, model8 GAM semblent les plus prometteurs mais attention modèles complexes

#
# exploration du meilleur modèle, model5 AIC = -16707.10, séléction des variables
## exploration of the best model ----
#

model10 <- mgcv::gam(Y ~ treatment + height + pool + s(time) + s(poisson, bs = "re"), data = dfa,method = "REML")
AIC(model5,model10) # on peut enlever fs, même AIC, on garde le modèle le plus simple, model12 AIC = -16707.07
model11 <- mgcv::gam(Y ~ treatment + height + pool + s(time), data = dfa,method = "REML")
AIC(model10,model11) # on peut enlever poisson RE aussi, même AIC, on garde le modèle le plus simple, model13 AIC = -16707.13
model12 <- mgcv::gam(Y ~ treatment + pool + s(time), data = dfa,method = "REML")
AIC(model11,model12) # on peut enlever height, même AIC, on garde le modèle le plus simple, model14 AIC = -16708.81
model13 <- mgcv::gam(Y ~ pool + s(time), data = dfa,method = "REML")
AIC(model12,model13) # a discuter pour valeur diff d'AIC, mais on enlève treatment, meilleur/égal AIC dans model15 plus simple, model15 AIC = -16710.66
model14 <- mgcv::gam(Y ~ s(time), data = dfa,method = "REML")
AIC(model13,model14) # on enlève finalement 'pool', meilleur AIC avec seulement s(time), model16 = -16979.16
model15 <- mgcv::gam(Y ~ s(time, by = pool), data = dfa,method = "REML")
AIC(model14,model15) # pas d'amélioration avec pool:time, model17 = -16704.81
model16 <- mgcv::gam(Y ~ s(time, by = treatment), data = dfa,method = "REML")
AIC(model14,model16) # pas d'amélioration avec treatment:time, model17 = -16966.32
AIC(model5,model10, model14, model15, model16) # modèle GAM 16 semble plus pertinent, pas d'interaction, pas de RE, seulement s(time), le plus simple sans effet fixe ni effet aléatoire

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
## additional GAMM ----
#

model5g <-  mgcv::gamm(Y ~ s(time)                 + height + treatment + pool                   , random = list(poisson = ~1, fs = ~1), data = dfa, method = "REML")
model7g <-  mgcv::gamm(Y ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = dfa, method = "REML")
model8g <-  mgcv::gamm(Y ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = dfa, method = "REML")
model9g <-  mgcv::gamm(Y ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = dfa, method = "REML")
model10g <- mgcv::gamm(Y ~ s(time, by = pool)      + height + treatment + pool, random = list(poisson = ~1), data = dfa, method = "REML")
model11g <- mgcv::gamm(Y ~ s(time, by = pool)      + height + treatment + pool, data = dfa, method = "REML")
model12g <- mgcv::gamm(Y ~ s(time, by = pool)               + treatment + pool, data = dfa, method = "REML")
model13g <- mgcv::gamm(Y ~ s(time, by = pool)                           + pool, data = segm, method = "REML")
model14g <- mgcv::gamm(Y ~ s(time)                                            , data = segm, method = "REML")
model15g <- mgcv::gamm(Y ~ s(time, by = pool)                                 , data = segm, method = "REML")
model16g <- mgcv::gamm(Y ~ s(time, by = treatment)                            , data = dfa,method = "REML")

summary(model14)
draw(model14)
draw(model14, residuals=TRUE)
concurvity(model14) # concurvity ok, un seul paramètre lissé
gam.check(model14)
abline(h=0, col="red")
# edf indiqué 6.27 < k = 9 pour s(time) donc non-linéarité et pas de underfitting mais attention p-value
# p-value positive indiquerait un sous ajustement aux données (underfitting) mais edf >> k pas besoin d'augmenter le nombre de para et de relancer
# on peut essayer une transformation sur Y pour homogénéiser la variance des résidus (hist semble ok quand même)
model14b <- mgcv::gam( abs(log(Y)) ~ s(time), data = dfa,method = "REML")
concurvity(model14b) # concurvity ok, un seul paramètre lissé
gam.check(model14b) # meilleur edf donc ajustement plus simple
AIC(model14, model14b) # delta AIC très grand, préférable model16
summary(model14b) 
# meilleur deviance explained = 16.7% et R-sq.(adj) = 0.164 mais peu diff de model16
# on préfère garder model16 en vu de l'AIC, difficulté à comparer avec la production de NA avec log()

model14c <- mgcv::gam(Y ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = df, method = "REML") # model final ajout de s(time), trajectoire commune + déviation par pool
model14d <- mgcv::gam(Y ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = df, method = "REML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique
AIC(model14, model14b, model14c, model14d, model14g)

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
table_model_final_acceleration <- AICmultitab(mod_list) # to save
SEL_model_final_acceleration <- MuMIn::model.sel(mod_list) # to save
## 

model_final_acceleration <- model14
#model_final_acceleration <- 
#  get(table_model_final_acceleration$model[
#    which.min(
#      ifelse(
#        table_model_final_acceleration$AIC == min(table_model_final_acceleration$AIC),
#        nchar(table_model_final_acceleration$formula),
#        -Inf
#      )
#    )
#  ])

## summary of the model ----
#
# model_final <- model14 GAM avec AIC = -16979.16 modèle le plus parcimonieux
#
# que retenir de summary (model_final) :
#
# R-sq.(adj) =  0.153 meilleur que model0, edf~6.269, p-value <2e-16 ***, Deviance explained = 15.5%
#
# pas d'effet direct ou indirect du traitement, de pool, ni de la taille 'height", pas d'effets alétoires importants ~quasi nuls
# temps diff entre traitements sans interaction significative treatment:time ou pool:time 
#
# time semble avoir une relation non-linéaire avec edf~6
# donc différence dans la dynamique temporelle 
# 
# Amont concernait le traitement avec flucutation environnementale pendant le développement et aval sans fluctuation du courant
# Less-Athas correspondait à la souche d'élevage atlantique et Cauterets à la souche sauvage méditérannéenne
#
# Au final modèle le plus simple avec relation non linéaire du temps
# PAS D'EFFET DU TRAITEMENT NI DU POOL SUR L'ACCELERATION
#

# visualiser les données accel avec un geom_smooth utilisant gam
ggplot(dfa, aes(x = time, y = Y, color = origin_treatment)) + # plus visuel pour le gam, mais parmi tous les segments, ne correspond pas au modèle !!
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()


#
# ANOVA DEUX FACTEURS POUR l'efficacité 'efficiency'
# efficiency ----
#
# dataset <- dfeff, origin, bio et dfe
# PAS DEPENDANT DU TEMPS -> PAS DE GAM

## dataset ----
colnames(dfeff)
str(dfeff)
dfe <- dfeff %>%
  dplyr::select(traitement, poisson, fs, time, d_cum_cm, d_tot_cm, efficiency) %>%
  mutate ( poisson = as.factor(poisson),
           fs = as.factor(fs),
           time = as.numeric(time),
           traitement = as.factor(traitement)) %>%
  rename ( treatment = traitement) %>%
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

dfe <- na.omit(dfe)

## visualiser la donnée ----
str(dfe)
ggplot(dfe, aes(x = fs, y = efficiency, color = origin_treatment)) + geom_point() 
ggplot(dfe, aes(x = weight, y = efficiency, color = origin_treatment)) + geom_point() 
ggplot(dfe, aes(x = height, y = efficiency, color = origin_treatment)) + geom_point() 
ggplot(dfe, aes(x = origin_treatment,y = efficiency,fill = origin_treatment)) +
  geom_boxplot(alpha = 0.7,outlier.shape = NA) +
  geom_jitter(width = 0.15,alpha = 0.25, size = 1) +
  theme_minimal(base_size = 14) +
  labs(x = "Origine × traitement",
       y = "Efficiency",
       title = "Distribution de l'efficacité selon les groupes") +
  theme(legend.position = "none",axis.text.x = element_text(angle = 20, hjust = 1))

ggplot(dfe,aes(x = treatment,y = efficiency,fill = treatment)) +
  geom_violin(trim = FALSE,alpha = 0.5) + geom_boxplot(width = 0.15,outlier.shape = NA,alpha = 0.8) +
  geom_jitter(width = 0.08, alpha = 0.2,size = 0.8) +
  facet_wrap(~pool) +
  theme_minimal(base_size = 14) +
  labs(title = "Distribution de l'efficacité", subtitle = "Comparaison des traitements selon l'origine",
    x = "Traitement",y = "Efficiency") +
  theme(legend.position = "none",strip.text = element_text(face = "bold"))

## CA pour ANOVA ----
car::leveneTest(efficiency ~ treatment * pool,data = dfe) # il y a au moins une variance qui diffère des autres en moyenne (Pr(>F) = 0.034)
car::leveneTest(efficiency ~ pool,data = dfe) # pool discrimine au moins deux groupes avec deux variances diff
car::leveneTest(efficiency ~ treatment ,data = dfe) # pas le traitement
car::leveneTest(log(efficiency) ~ pool,data = dfe) # transformation log ne résout pas le pb d'homoscédasticité
car::leveneTest(efficiency ~ bief, data = dfe)
car::leveneTest(efficiency ~ temperature_ellerby, data = dfe)

# on réalise une ANOVA en tenant compte du pb d'homoscédasticité quand même car Pr(>F)=0.03404 pas trop fort
## ANOVA ----
model_aov <- aov(efficiency ~ treatment * pool,data = dfe) # modèle aov classique
model_lm <- lm(efficiency ~ treatment * pool,data = dfe) # modèle linéaire
model_lmer <- lme4::lmer(efficiency ~ treatment * pool +(1 | poisson),data = dfe) # modèle mixte avec poisson en RE
model_bt <- lm(efficiency ~ treatment * pool + bief + temperature_ellerby, data = dfe) # modèle linéaire

summary(model_aov) # treatment:pool avec Pr(>F) = 0.01732 * effet signicatif du traitement dépendant du pool
summary(model_lm) # même valeur donc modèle linéaire est 'adapté' pour ANOVA, R-squared:  0.04205 très faible, p-value: 0.04403
summary(model_lmer) # poisson avec Variance ~0.0001184 donc RE(poisson) quasi null, négligeable, on utilise lm
anova(model_bt)
anova(model_lm)

# suite au résultat du test de Levene on utilise un GLS qui prend en compte la non-d'homoscédasticité avec 'pool' plus adapté qu'un LM classique
## GLS ----
model_gls <- gls(efficiency ~ treatment * pool ,data = dfe,weights = varIdent(form = ~1 | pool))
anova(model_gls) # l'interaction treatment:pool reste significative P-value=0.0183 donc effet signicatif du traitement dépendant du pool
summary(model_gls) # Variance diff selon pool, parameter estimates: Lees Athas ~1 vs Cauterets ~1.49, AIC = -349.0575
AIC(model_gls) # AIC = -349.0575

# ajout de wieght et height pour tester
# on teste la corrélation entre height et weight
## corelation with size and weight ----
cor.test(dfe$weight, dfe$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)
model_gls2 <- gls(efficiency ~ treatment * pool + height,data = dfe,weights = varIdent(form = ~1 | pool))
anova(model_gls2) # rien de significatif ormis treatment:pool qui reste significatif, donc indépendant de taille/poids ?
AIC(model_gls, model_gls2) # sans height meilleur AIC, treatment:pool reste significatif dans les deux cas, gls2 AIC = -335.9673

ggplot2::ggplot(dfe,aes(x = treatment,y = efficiency,color = pool,group = pool)) +
  
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se,geom = "errorbar",width = 0.1) +
  
  theme_minimal(base_size = 14) +
  labs(y = "Efficiency",x = "Traitement",color = "Origine",title = "Interaction traitement × origine" )

model_final_efficiency <- model_gls

## summary of the model (GLS) ----
#
# que retenir de l'analyse de la variance sur les facteurs pool x traitement
#
# hétérogénéité des variances entre pools détectée (Levene test, p < 0.05), 
# donc on utilise un GLS avec structure de variance hétérogène (varIdent(form = ~1 | pool))
#
# pas d'effet direct du traitement, du pool, ou de la taille !
# comme pour AOV et LM, on a une interaction significative entre treatment et origine (treatment:pool, p = 0.018)
# donc l’effet du traitement dépend de l’origine des individus
# effets direct treatment (p-value = 0.5212) et pool (p-value = 0.1549) non significatifs
#
# VALUES : Intercept 0.7226715, treatmentaval 0.0405809, poolLees Athas 0.0424628, treatmentaval:poolLees Athas -0.0698488
#
# prediction efficiency (model_gls) :
# Cauterets   Amont   0.7226715 
# Cauterets   Aval    0.7226715 + 0.0405809 ~0.764
# Less Athas  Amont   0.7226715 + 0.0424628 ~0.765
# Less Athats Aval    0.7226715 + 0.0405809 + 0.0424628 - 0.0698488 ~0.735
#
# EFFET SIGNIFICATIF DE L'INTERACTION TREATMENT:POOL, DONC EFFET DU TRAITEMENT SUR L'EFFICACITE DEPEND DE L'ORIGINE GENETIQUE
# EN MOYENNE LES INDIVS Lees-Athas:AVAL et Cauterets:AMONT SONT MOINS EFFICACES QUE LES DEUX AUTRES CATEGORIES
#



#
# MODELE LM NAIF, GAM et GAMM pour coef avec ratio snout/tail
# curvature ratio snout/tail ----
#
# dataset <- dfcoef, origin, bio, snail et snail2

FSRatioSnoutTailPlot("002_fs2", 4)
RatioSnoutTailPlot()

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

str(snail)

## visualisation de la donnée ----
plot(snail$Y ~ snail$time)
abline(lm(Y ~ time, data = snail), col="red", lwd=2)
hist(snail$Y) # ditrib ok
hist(log(snail$Y)) # décalage à droite, mais plus de la moitié en NA (valeurs négatives)
hist(abs(snail$Y))
hist(log(abs(snail$Y)))

# on transforme abs() pour éviter un signal opposé qui annulerait le signal entre courbes
snail$absY <- abs(snail$Y) # gérer le signal signé unilatéralement, fats-start gauche ou droite
# on peut transformer en log() pour une meilleure déviance dans les GAM et une distribution plus homogène des valeurs
# cependant GAM pas soumis à des CA très strictes donc on préfère garder valeur originale
# NDT: la transformation ne modifie pas ni la séléction de modèle ni les conlusions finales
# snail$logabsY <- log(snail$absY) # gérer la normalisation des données

plot(snail$Y ~ snail$treatment) # on observe à priori pas de grandes différences entre les deux traitement
plot(snail$Y ~ snail$pool) # on observe à priori pas de grandes différences entre les deux origines
ggplot(snail, aes(x = time, y = Y)) + 
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = origin_treatment),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon pool:treatment",
       x = "Temps",y = "Y",color = "Classe de poids") # diff notable du moins visuellement, lees athas amont sort du lot
# representation effet pool:treatement
snail2 <- snail %>%
  mutate(weight_class = ggplot2::cut_number(weight, n = 4,labels = c("léger","moyen-","moyen+","lourd")),
         height_class = ggplot2::cut_number(height, n = 4,labels = c("petit","moyen-","moyen+","grand")))
snail2 <- na.omit(snail2)
ggplot(snail2, aes(x = time, y = Y)) + 
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de poids",
       x = "Temps",y = "Y",color = "Classe de poids") 
# différence dans la valeur finale et dans la dynamique temporelle selon les classes de poids, pas même orientation selon poids !!
ggplot(snail2, aes(x = time, y = Y)) +
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de taille",
       x = "Temps",y = "Y",color = "Classe de taille") # idem, grosse diff entre les grands et petits, orientation et valeur finale
# on teste la corrélation entre height et weight
cor.test(snail$weight, snail$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)

#
## approche naïve par modèle linéaire ----
#

model0 <- lm( absY ~ time, data=snail)
summary(model0) # Adjusted R-squared:  0.005022, p-value: 0.0002782
AIC(model0) # AIC = 25666.96 avec absY, AIC = 7257.099 avec logabsY
model1 <- lm( absY ~ time + treatment + pool + height + poisson + fs, data=snail, na.action=na.omit)
drop1(model1, test="F") # AIC = 18451 avec absY, AIC = 401.99 avec logabsY
model2 <- lm( absY ~ time + fs, data=snail, na.action=na.omit)
drop1(model2, test="F") # AIC = 397.21, on préfère un modèle non linéaire puisque les variables n'ont pas de relation à priori linéaire -> GAM d'abord
model3 <- glm( absY ~ time + treatment + pool + height + poisson +  fs, data=snail) # GLM correspond à GAM sans lissage s() 

#
## GAM et GAMM ----
#

# on ajoute s(time) pour relation non linéaire du temps avec Y
model4 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + poisson +  fs, data=snail, method="REML") # s() pour lisser la variable non linéaire
# on change poisson et fs en effets aléatoires, les variables structurent les données mais n'ont pas d'effet fixes à priori
model5 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
# ajout de origin_treatment
model6 <- mgcv::gam( absY ~ s(time) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML")
# on teste les interactions temps:treatment
model7 <- mgcv::gam( absY ~ s(time, by = treatment) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:pool
model8 <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
modelbief <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
modeltemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + temperature_ellerby + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:height
model9 <- mgcv::gam( absY ~ s(time, by = height) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
AIC(model0, model1, model2, model3, model4, model5, model6, model7, model8, model9, modelbief, modeltemp) # AIC = 6579.407 pour model8 le plus petit
summary(model7) # AIC = 6579.407, R-sq.(adj) =  0.248   Deviance explained = 27.6%, 
plot(model7, pages = 1)
draw(model7, residuals = TRUE)

#
# exploration du meilleur modèle model7 GAM avec s(time, by = pool) et RE(poisson, fs)
## exploration of the best model ----
#

modelbieftemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + bief + temperature_ellerby + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML") # s() pour lisser la variable non linéaire
AIC(model8, modelbief, modeltemp, modelbieftemp) # on garde bief
model10 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML")
AIC(modelbief, model10) # AIC = 22689.05, on prend le plus simple, on retire origin_treatment
model11 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML")
AIC(modelbief, model11) # AIC = 6578.344, valeurs équivalentes, on prend le plus simple, on retire treatment
model12 <- mgcv::gam( absY ~ s(time, by = pool) + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML")
AIC(modelbief, model12) # AIC = 6578.411, valeurs équivalentes, on prend le plus simple, on retire pool
model13 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re") +  s(fs, bs = "re"), data=snail, method="REML")
AIC(modelbief, model13) # AIC = 6576.894, valeurs équivalentes, on prend le plus simple, on retire height
model14 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re"), data=snail, method="REML")
AIC(modelbief, model14) # AIC = 6587.468, AIC supérieur il est plus pertinent de garder RE(fs)
model15 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(fs, bs = "re"), data=snail, method="REML")
AIC(modelbief, model15) # AIC = 6561.590, meilleur AIC sans RE(poisson), on peut retirer
model16 <- mgcv::gam( absY ~ s(time) + bief + s(fs, bs = "re"), data=snail, method="REML")
AIC(model15, model16) # AIC = 6685.697, on garde donc s(time, by=pool) et RE(fs)
model17 <- mgcv::gam( absY ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method = "REML")
AIC(model16, model17) # AIC = 22663.02, on peut enlever bief

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
## additional GAMM ----
#

model6g <-  mgcv::gamm(absY ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model7g <-  mgcv::gamm(absY ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model8g <-  mgcv::gamm(absY ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
#model9g <-  mgcv::gamm(absY ~ s(time, by = height)                                + origin_treatment, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model10g <- mgcv::gamm(absY ~ s(time, by = pool)      + height + treatment + pool + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model11g <- mgcv::gamm(absY ~ s(time, by = pool)      + height             + pool + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model12g <- mgcv::gamm(absY ~ s(time, by = pool)      + height                    + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model13g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1, fs = ~1), data = snail, method = "REML")
model14g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1), data = snail, method = "REML")
model15g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(fs = ~1), data = snail, method = "REML")
model16g <- mgcv::gamm(absY ~ s(time) + bief , random = list(fs = ~1), data = snail, method = "REML")
model17g <- mgcv::gamm(absY ~ s(time, by = pool), random = list(fs = ~1), data = snail, method = "REML" )

summary(model15)
plot(model15, pages=1)
draw(model15, residuals = TRUE)
concurvity(model15) # pb avec concurvity s(time):poolCauterets = 0.81194507 pour 'worst' et s(fs) = 1 pour 'worst', le rest ok
gam.check(model15) # edf toujours ok entre 4 et 5 pour time:pool < k non-linéarité, pas besoin d'augmenter k
# par contre p-values significatives on teste une transfo log pour homogénéiser la variance des résidus
# on note que fs edf = 130 enorme, très forte variabilité de l'évenement fast-start
model15b <- mgcv::gam( log(absY) ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method="REML")
model15c <- mgcv::gam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method="REML")
concurvity(model15b) # pas d'amélioration avec log()
gam.check(model15b) # idem
AIC(model15, model15b, model15c) # log(abs(Y)) AIC = 6561.59 ; abs(Y) AIC = 24199.39 ; Y AIC = 27657.59
summary(model15b) # meilleur R-sq.(adj) =  0.388 et  Deviance explained = 42%

model15d <- mgcv::gam(absY ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = snail, method = "REML") # model final ajout de s(time), trajectoire commune + déviation par pool
model15e <- mgcv::gam(absY ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = snail, method = "REML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique
AIC(model15, model15d, model15e)

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
table_model_final_snout_tail_ratio <- AICmultitab(mod_list) # to save
SEL_model_final_snout_tail_ratio <- MuMIn::model.sel(mod_list) # to save
##

model_final_snout_tail_ratio <- model15d
#model_final_snout_tail_ratio <- 
#  get(table_model_final_snout_tail_ratio$model[
#    which.min(
#      ifelse(
#        table_model_final_snout_tail_ratio$AIC == min(table_model_final_snout_tail_ratio$AIC),
#        nchar(table_model_final_snout_tail_ratio$formula),
#        -Inf
#      )
#    )
#  ])

## summary of the model ----
#
# que retenir de summary(model_final) :
# model16 <- mgcv::gam( log(abs(Y)) ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method="REML")
#
# R-sq.(adj) =  0.248   Deviance explained = 27.6%   AIC = 6561.59
#
# Pas d'effet direct du traitement de pool ou de height sur la valeur du ratio
# pas d'effet de RE(poisson) pas de variabilité inter individu
# 
# Effet non linéaire du temps significatif, edf ~4.5
# Effet de pool dépendant du temps : time:Cauterets  p-value = <2e-16 ***  
#                                    time:Less-Athas  p-value = <2e-16 *** 
# Effet de l'effet alétoire RE(fs) p-value < 2e-16 *** grande diff de l'évenement fast-start 
#
# Difference dans la dynamique temporelle !
# donc possible detection dans la dynamique temporelle mais pas dans le résultat final (ratio de courbure)
# ici amont concernait le traitement avec flucutation environnementale 
# et Cauterets concernait la souche sauvage méditérannéenne
#  
# DIFF SIGNIFICATIVE DANS LA DYNAMIQUE TEMPORELLE DANS LE SENS DU FAST-START MAIS FORT EFFET ALETOIRE DU FAST-START (EVENEMENT)
#

RatioSnoutTailPlot() + # visualiser les données ratio avec un geom_smooth utilisant gam
  geom_smooth(aes(group = treatment), se = FALSE, size = 1.2)

ggplot(coef, aes(x = frame, y = ratio, color = treatment)) + # plus visuel pour le gam mais ne représente pas le modèle !
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()

ggplot(coef, aes(x = frame, y = log(abs(ratio)), color = treatment)) + # plus visuel pour le gam mais ne représente pas le modèle !
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()

#
# MODELE LM NAIF, GAM et GAMM pour coef2 avec ratio ant/post
# curvature ratio anterior/posterior ----
#
# dataset <- coef2, origin, bio, antpost et antpost2

FSRatioAntPostPlot("002_fs2", 4)

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

str(antpost)

## visualisation de la donnée ----
plot(antpost$Y ~ antpost$time)
abline(lm(Y ~ time, data = antpost), col="red", lwd=2)
hist(antpost$Y)
hist(log(antpost$Y)) # décalage à droite, mais plus de la moitié en NA (valeurs négatives)
hist(log(abs(antpost$Y)))

# on transforme abs() pour éviter un signal opposé qui annulerait le signal entre courbes
antpost$absY <- abs(antpost$Y) # gérer le signal signé unilatéralement, fats-start gauche ou droite
# on peut transformer en log() pour une meilleure déviance dans les GAM et une distribution plus homogène des valeurs
# cependant GAM pas soumis à des CA très strictes donc on préfère garder valeur originale
# NDT: la transformation ne modifie pas ni la séléction de modèle ni les conlusions finales
# antpost$logabsY <- log(antpost$absY) # gérer la normalisation des données

plot(antpost$Y ~ antpost$treatment) # on observe à priori pas de grandes différences entre les deux traitement
plot(antpost$Y ~ antpost$pool) # on observe à priori pas de grandes différences entre les deux origines
ggplot(antpost, aes(x = time, y = Y)) + 
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = origin_treatment),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon pool:treatment",
       x = "Temps",y = "Y",color = "Classe de poids") # diff notable du moins visuellement, lees athas amont sort du lot
# representation effet pool:treatement
antpost2 <- antpost %>%
  mutate(weight_class = ggplot2::cut_number(weight, n = 4,labels = c("léger","moyen-","moyen+","lourd")),
         height_class = ggplot2::cut_number(height, n = 4,labels = c("petit","moyen-","moyen+","grand")))
antpost2 <- na.omit(antpost2)
ggplot(antpost2, aes(x = time, y = Y)) + 
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = weight_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de poids",
       x = "Temps",y = "Y",color = "Classe de poids") 
# différence dans la valeur finale et dans la dynamique temporelle selon les classes de poids, pas même orientation selon poids !!
ggplot(antpost2, aes(x = time, y = Y)) +
  geom_line(aes(group = fs),alpha = 0.08,color = "grey50") +
  geom_smooth(aes(color = height_class),method = "gam",formula = y ~ s(x),se = TRUE,linewidth = 1.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Dynamique temporelle selon les classes de taille",
       x = "Temps",y = "Y",color = "Classe de taille") # idem, grosse diff entre les grands et petits, orientation et valeur finale
# on teste la corrélation entre height et weight
cor.test(antpost$weight, antpost$height) # cor ~0.96 et p-value < 2.2e-16 les deux sont très fortement corrélés (logique)

#
## approche naïve par modèle linéaire ----
#

model0 <- lm( absY ~ time, data=antpost)
summary(model0) # Adjusted R-squared: 0.004495, p-value: 0.0005037
AIC(model0) # AIC = 7468.493
model1 <- lm( absY ~ time + treatment + pool + height + poisson + fs, data=antpost, na.action=na.omit)
drop1(model1, test="F") # AIC = 7440.036
model2 <- lm( absY ~ time + fs, data=antpost, na.action=na.omit)
drop1(model2, test="F") # AIC = 7549.874, on préfère un modèle non linéaire puisque les variables n'ont pas de relation à priori linéaire -> GAM d'abord
model3 <- glm( absY ~ time + treatment + pool + height + poisson +  fs, data=antpost) # GLM correspond à GAM sans lissage s() 

#
## GAM et GAMM ----
#

# on ajoute s(time) pour relation non linéaire du temps avec Y
model4 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + poisson +  fs, data=antpost, method="REML") # s() pour lisser la variable non linéaire
# on change poisson et fs en effets aléatoires, les variables structurent les données mais n'ont pas d'effet fixes à priori
model5 <- mgcv::gam( absY ~ s(time) + treatment + pool + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
# ajout de origin_treatment
model6 <- mgcv::gam( absY ~ s(time) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML")
# on teste les interactions temps:treatment
model7 <- mgcv::gam( absY ~ s(time, by = treatment) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:pool
model8 <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
modelbief <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
modeltemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + pool + temperature_ellerby + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
# on teste les interactions temps:height
model9 <- mgcv::gam( absY ~ s(time, by = height) + treatment + pool + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
AIC(model0, model1, model2, model3, model4, model5, model6, model7, model8, model9, modelbief, modeltemp) # AIC = 6579.407 pour model8 le plus petit
summary(model7) # AIC = 6579.407, R-sq.(adj) =  0.248   Deviance explained = 27.6%, 
plot(model7, pages = 1)
draw(model7, residuals = TRUE)

#
# exploration du meilleur modèle model7 GAM avec s(time, by = pool) et RE(poisson, fs)
## exploration of the model ----
#

modelbieftemp <- mgcv::gam( absY ~ s(time, by = pool) + treatment + bief + temperature_ellerby + pool + bief + origin_treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML") # s() pour lisser la variable non linéaire
AIC(model8, modelbief, modeltemp, modelbieftemp) # on garde bief
model10 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + treatment + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML")
AIC(modelbief, model10) # AIC = 22689.05, on prend le plus simple, on retire origin_treatment
model11 <- mgcv::gam( absY ~ s(time, by = pool) + pool + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML")
AIC(modelbief, model11) # AIC = 6578.344, valeurs équivalentes, on prend le plus simple, on retire treatment
model12 <- mgcv::gam( absY ~ s(time, by = pool) + bief + height + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML")
AIC(modelbief, model12) # AIC = 6578.411, valeurs équivalentes, on prend le plus simple, on retire pool
model13 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re") +  s(fs, bs = "re"), data=antpost, method="REML")
AIC(modelbief, model13) # AIC = 6576.894, valeurs équivalentes, on prend le plus simple, on retire height
model14 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(poisson, bs = "re"), data=antpost, method="REML")
AIC(modelbief, model14) # AIC = 6587.468, AIC supérieur il est plus pertinent de garder RE(fs)
model15 <- mgcv::gam( absY ~ s(time, by = pool) + bief + s(fs, bs = "re"), data=antpost, method="REML")
AIC(modelbief, model15) # AIC = 6561.590, meilleur AIC sans RE(poisson), on peut retirer
model16 <- mgcv::gam( absY ~ s(time) + bief + s(fs, bs = "re"), data=antpost, method="REML")
AIC(model15, model16) # AIC = 6685.697, on garde donc s(time, by=pool) et RE(fs)
model17 <- mgcv::gam( absY ~ s(time, by = pool) + s(fs, bs = "re"), data=antpost, method = "REML")
AIC(model16, model17) # AIC = 22663.02, on peut enlever bief

#
# pour chaque GAM on explore un modèle mixte aussi -> GAMM
## additional GAMM ----
#

model6g <-  mgcv::gamm(absY ~ s(time)                 + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model7g <-  mgcv::gamm(absY ~ s(time, by = treatment) + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model8g <-  mgcv::gamm(absY ~ s(time, by = pool)      + height                    + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
#model9g <-  mgcv::gamm(absY ~ s(time, by = height)                                + origin_treatment, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model10g <- mgcv::gamm(absY ~ s(time, by = pool)      + height + treatment + pool + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model11g <- mgcv::gamm(absY ~ s(time, by = pool)      + height             + pool + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model12g <- mgcv::gamm(absY ~ s(time, by = pool)      + height                    + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model13g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1, fs = ~1), data = antpost, method = "REML")
model14g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(poisson = ~1), data = antpost, method = "REML")
model15g <- mgcv::gamm(absY ~ s(time, by = pool)                                  + bief, random = list(fs = ~1), data = antpost, method = "REML")
model16g <- mgcv::gamm(absY ~ s(time) + bief , random = list(fs = ~1), data = antpost, method = "REML")
model17g <- mgcv::gamm(absY ~ s(time, by = pool), random = list(fs = ~1), data = antpost, method = "REML" )

summary(model15)
plot(model15, pages=1)
draw(model15, residuals = TRUE)
concurvity(model15) # toujours le même problème avec concurvity s(time):poolCauterets = 0.81194507 pour 'worst' et s(fs) = 1 pour 'worst', le rest ok
gam.check(model15) # edf toujours ok entre 4 et 6 pour time:pool < k non-linéarité, pas besoin d'augmenter k
# par contre p-values significatives on teste une transfo log pour homogénéiser la variance des résidus
# on note que fs edf = 128 enorme, très forte variabilité de l'évenement fast-start
model15b <- mgcv::gam( log(absY) ~ s(time, by = pool) + s(fs, bs = "re"), data=antpost, method="REML") # modele test transformation de variable
model15c <- mgcv::gam( Y ~ s(time, by = pool) + s(fs, bs = "re"), data=antpost, method="REML") # modele test transformation de variable
concurvity(model15b) # pas d'amélioration
gam.check(model15b) # edf encore plus proche de k, danger d'underfitting avec le p-values très positives
AIC(model15,model15b, model15c)  # log(abs(Y)) AIC = 6622.729 ; abs(Y) AIC = 22663.024 ; Y AIC = 26026.221
summary(model15b) # meilleur R-sq.(adj) =  0.407 et Deviance explained = 44%

model15d <- mgcv::gam(absY ~ s(time, by = pool) + s(time) + s(fs, bs = "re"), data = antpost, method = "REML") # model final ajout de s(time), trajectoire commune + déviation par pool
model15e <- mgcv::gam(absY ~ s(time, pool, bs = "fs") + s(fs, bs = "re"), data = antpost, method = "REML") #  model final ajout de s(time, pool, bs="fs"), factor-smooth interaction hiérarchique
AIC(model15, model15d, model15e)

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
table_model_final_ant_post_ratio <- AICmultitab(mod_list) # to save
SEL_model_final_ant_post_ratio <- MuMIn::model.sel(mod_list) # to save
##

model_final_ant_post_ratio <- model17
#model_final_ant_post_ratio <- 
#  get(table_model_final_ant_post_ratio$model[
#    which.min(
#      ifelse(
#        table_model_final_ant_post_ratio$AIC == min(table_model_final_ant_post_ratio$AIC),
#        nchar(table_model_final_ant_post_ratio$formula),
#        -Inf
#      )
#    )
#  ])

## summary of the model ----
#
# que retenir de summary(model_final) :
# model16b <- mgcv::gam( log(Y) ~ s(time, by = pool) + s(fs, bs = "re"), data=snail, method="REML") 
# le même que pour le ratio snout/tail (attendu)
#
# R-sq.(adj) =  0.432   Deviance explained = 49.5%   AIC = 3240.637
#
# Pas d'effet direct du traitement de pool ou de height sur la valeur du ratio
# pas d'effet de RE(poisson) pas de variabilité inter individu
# 
# Effet non linéaire du temps significatif, edf ~4.5
# Effet de pool dépendant du temps : time:Cauterets  p-value = <2e-16 ***
#                                    time:Less-Athas  p-value = <2e-16 ***
# Effet de l'effet alétoire RE(fs) p-value < 2e-16 *** grande diff de l'évenement fast-start 
#
# Difference dans la dynamique temporelle !
# donc possible detection dans la dynamique temporelle mais pas dans le résultat final (ratio de courbure)
# ici amont concernait le traitement avec flucutation environnementale 
# et Cauterets concernait la souche sauvage méditérannéenne
#  
# DIFF SIGNIFICATIVE DANS LA DYNAMIQUE TEMPORELLE DANS LE SENS DU FAST-START MAIS FORT EFFET ALETOIRE DU FAST-START (EVENEMENT)
#

RatioSnoutTailPlot() + # visualiser les données ratio avec un geom_smooth utilisant gam
  geom_smooth(aes(group = treatment), se = FALSE, size = 1.2)

ggplot(coef2, aes(x = frame, y = ratio, color = treatment)) + # plus visuel pour le gam
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()

ggplot(coef2, aes(x = frame, y = abs(ratio), color = treatment)) + # plus visuel pour le gam
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  theme_minimal()

#
# SAVE and PLOT ----
#

# final models ----
model_final_segmentation
model_final_cumulative_distance
model_final_acceleration
model_final_efficiency
model_final_snout_tail_ratio
model_final_ant_post_ratio

# table for model selection ----
table_model_final_segmentation
SEL_model_final_segmentation

table_model_final_cumulative_distance
SEL_model_final_cumulative_distance

table_model_final_acceleration
SEL_model_final_acceleration

table_model_final_snout_tail_ratio
SEL_model_final_snout_tail_ratio

table_model_final_ant_post_ratio
SEL_model_final_ant_post_ratio

# plots ----
preGAMPLOTsegm()
gratia::appraise(model_final_segmentation)

preGAMPLOTdcum()
gratia::appraise(model_final_cumulative_distance)

preGAMPLOTaccel()
gratia::appraise(model_final_acceleration)

preGAMPLOTefficiency()
plot(model_final_efficiency)

preGAMPLOTsnail()
gratia::appraise(model_final_snout_tail_ratio)

preGAMPLOTantpost()
gratia::appraise(model_final_ant_post_ratio)

# export data frame for plot ----
write.table(treat, "output/originxtreatment.txt", row.names = FALSE)  
write.table(segm, "output/segm.txt", row.names = FALSE)  
write.table(segm2, "output/segm2.txt", row.names = FALSE)
write.table(df, "output/dfdcum.txt", row.names = FALSE)  
write.table(df2, "output/dfdcum2.txt", row.names = FALSE)
write.table(dfa, "output/dfa.txt", row.names = FALSE)
write.table(df2, "output/dfa2.txt", row.names = FALSE)
write.table(dfe, "output/dfefficiency.txt", row.names = FALSE)
write.table(snail, "output/snail.txt", row.names = FALSE)
write.table(snail2, "output/snail2.txt", row.names = FALSE)
write.table(antpost, "output/antpost.txt", row.names = FALSE)
write.table(antpost2, "output/antpost2.txt", row.names = FALSE)
# export table for model selection ----
write.table(table_model_final_segmentation, "models/table_model_final_segmentation.txt", row.names = FALSE )
write.table(table_model_final_cumulative_distance, "models/table_model_final_cumulative_distance.txt", row.names = FALSE )
write.table(table_model_final_acceleration, "models/table_model_final_acceleration.txt", row.names = FALSE )
write.table(table_model_final_snout_tail_ratio, "models/table_model_final_snout_tail_ratio.txt", row.names = FALSE )
write.table(table_model_final_ant_post_ratio, "models/table_model_final_ant_post_ratio.txt", row.names = FALSE )
write.table(as.data.frame(SEL_model_final_segmentation), "models/SEL_model_final_segmentation.txt", row.names = FALSE)
write.table(as.data.frame(SEL_model_final_cumulative_distance), "models/SEL_model_final_cumulative_distance.txt", row.names = FALSE)
write.table(as.data.frame(SEL_model_final_acceleration), "models/SEL_model_final_acceleration.txt", row.names = FALSE)
write.table(as.data.frame(SEL_model_final_snout_tail_ratio), "models/SEL_model_final_snout_tail_ratio.txt", row.names = FALSE)
write.table(as.data.frame(SEL_model_final_ant_post_ratio), "models/SEL_model_final_ant_post_ratio.txt", row.names = FALSE)

# export model to avoid long time calculation ----
saveRDS(model_final_segmentation, "models/model_final_segmentation.rds")
saveRDS(model_final_cumulative_distance, "models/model_final_cumulative_distance.rds")
saveRDS(model_final_acceleration, "models/model_final_acceleration.rds")
saveRDS(model_final_efficiency, "models/model_final_efficiency.rds")
saveRDS(model_final_snout_tail_ratio, "models/model_final_snout_tail_ratio.rds")
saveRDS(model_final_ant_post_ratio, "models/model_final_ant_post_ratio.rds")

## FIN SCRIPT 05_statistic_models.R

