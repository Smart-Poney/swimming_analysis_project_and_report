# Ce script a pour objectif de faire un modèle GAM et GAMM dans le cadre du projet tutoré UPPA 2025-2026 sur la caractérisation de la nage de Salmo trutta

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2, readr, lme4, gam, corrplot)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

load("OUTPUT/dataAMAV03.RData")
# créer un tableau avec unique id et la séquence donc S ou M associé 
data %>% distinct(id, sequence,traitement) %>% arrange(id) %>% view()

# enregister ce tableau au dessus et l'exporter 
#data %>% distinct(id,traitement) %>% arrange(id)
#data_vignon <- data %>% distinct(id,traitement) %>% arrange(id)
#write.csv(data_vignon, "OUTPUT/data_vignon.csv", row.names = FALSE)

# vérifier le nombre d'individus par traitement
data %>% filter(traitement == "AM") %>% filter(sequence == "S") %>% distinct(id) %>% nrow()
data %>% filter(traitement == "AM") %>% filter(sequence == "M") %>% distinct(id) %>% nrow()
data %>% filter(traitement == "AV") %>% filter(sequence == "M") %>% distinct(id) %>% nrow()
data %>% filter(traitement == "AV") %>% filter(sequence == "S") %>% distinct(id) %>% nrow()

data %>% filter(traitement == "AV") %>% distinct(id) %>% nrow()
# vérifier le nombre par séquence
data %>% filter(sequence == "S") %>% distinct(id) %>% nrow()
data %>% filter(sequence == "M") %>% distinct(id) %>% nrow()

#data issu du script 04_ORDINATION
#data

#### Retenir que quelques variables à l'issue du RF en script 04_ORDINATION ####
# par ordre d'importance selon le RF : 

# fréquence de battement
# taille en mm 
# poids 
# lumière
# température 

data_GAM <- data %>% dplyr::select(id, frequence_battement, taille_mm, poid, X_norm, Y_max, lumiere, temperature, traitement, bief, debut_fin_juin, Observateurs_ImageJ,Origine,saut_boite,zone, sequence, trace)

vars <- data_GAM %>% dplyr::select(frequence_battement, taille_mm, poid, temperature, lumiere)
cor_mat <- cor(vars, use = "complete.obs")
corrplot(cor_mat, method = "color")

data_GAM <- data_GAM %>% mutate(Observateurs_ImageJ = as.factor(Observateurs_ImageJ),
                        debut_fin_juin = as.factor(debut_fin_juin),
                        id = as.factor(id),
                        saut_boite = as.factor(saut_boite),
                        traitement = as.factor(traitement),
                        bief = as.factor(bief),
                        zone = as.factor(zone), 
                        poid = as.numeric(poid), 
                        taille_mm = as.numeric(taille_mm), 
                        temperature = as.numeric(temperature), 
                        lumiere = as.numeric(lumiere), 
                        frequence_battement = as.numeric(frequence_battement), 
                        sequence = as.factor(sequence), 
                        Origine = as.factor(Origine),
                        trace = as.factor(trace))

# /////////////////////////////////////////////////////////////////////////////#
          #### //////////////////// GAMMS  /////////////////// ####
# /////////////////////////////////////////////////////////////////////////////#

library(gam)
library(lme4)
library(mgcv)
library(itsadug)
library(ggplot2)
#install.packages("bbmle")
library(bbmle)

# modèle null sans covariable sans fonction lisse donc lm 
lm0 <- gam(Y_max ~ X_norm, method = "REML",data = data_GAM) # Deviance expl = 75.5%
summary(lm0)

# modèle null sans covariable avec fonction lisse donc modèle non linéaire
m0 <- gam(Y_max ~ s(X_norm), method = "REML", data = data_GAM) # Deviance expl = 83.3%
summary(m0)

# modèle avec covariable de traitement mais sans fonction lisse
m1 <- gam(Y_max ~ s(X_norm, by = traitement), method = "REML",data = data_GAM) # 83.6%

summary(m1) # R2 = 0.835

# plots 
plot_smooth(m10b, view = "X_norm", cond=list(traitement = "AM"))
plot_smooth(m10b, view = "X_norm", cond=list(traitement = "AV"),add = TRUE, col = "blue")
plot_diff(m10b,view = "X_norm",comp = list(traitement = c("AM", "AV")))

m2 <- gam(Y_max ~ s(X_norm, by = traitement) + s(id, bs = "re"), method = "REML",data = data_GAM) # 83.8% # meilleur en plus avec dev expliqué

summary(m2) # R2 = 0.837

# pente aléatoire par individu et ordonné à l'origine aléatoire
m3 <- gam(Y_max ~ s(X_norm, by = traitement) + s(id, bs = "re") + s(id,X_norm,bs = "re") , method = "REML",data = data_GAM) # 91.6%

summary(m3) # 0.912

# pente variable
m4 <- gam(Y_max ~ s(X_norm, by = traitement) + s(id,X_norm,bs = "re") , method = "REML",data = data_GAM) # 91.6%

summary(m4) # 0.912

# wig variable simple
m5 <- gam(Y_max ~ s(X_norm, by = traitement) + s(X_norm, by = id), method = "REML",data = data_GAM) 

# wig variable et OO variable
m6 <- gam(Y_max ~ s(X_norm, by = traitement) + s(X_norm, by = id) + s(id, bs = "re"), method = "REML",data = data_GAM) 

# wig variable et pente variable
m7 <- gam(Y_max ~ s(X_norm, by = traitement) + s(X_norm, by = id) + s(id,X_norm,bs = "re"), method = "REML",data = data_GAM)  

# wig variable,OO variable et pente variable (la totale)
m8 <- gam(Y_max ~ s(X_norm, by = traitement) + s(X_norm, by = id) + s(id,X_norm,bs = "re") + s(id, bs = "re"), method = "REML",data = data_GAM) 

AIC(m1,m2,m3,m4,m5,m6,m7,m8,mtout)
library(MuMIn)
AICc(m1,m2,m3,m4,m5,m6,m7,m8,mtout)
BIC(m1,m2,m3,m4,m5,m6,m7,m8,mtout)

# Ajout des covariables

mtout <- gam(Y_max ~ s(X_norm, by = traitement) + 
            s(X_norm, by = id) + 
            s(id,X_norm,bs = "re") + 
            s(id, bs = "re") + 
            traitement + 
            s(frequence_battement) + 
            s(poid) + 
            s(lumiere) + 
            s(temperature) + 
            bief + 
            debut_fin_juin + 
            Observateurs_ImageJ +  
            Origine +
            saut_boite + 
            zone + 
            sequence + 
            trace, 
            method = "REML",data = data_GAM)

summary(mtout) # R2 = 0.885
# apporte pas grand chose


# /////////////////////////////////////////////////////////////////////////////#
                      #### Base de GAMM défini ####
# /////////////////////////////////////////////////////////////////////////////#

m3base <- gam(Y_max ~ s(X_norm, by = traitement) + 
            s(id, bs = "re") + 
            s(id,X_norm,bs = "re") + 
            traitement,
            method = "REML",data = data_GAM) # 91.6% 

?gam()

# m3 modèle de base ici on rajoute toutes les coviarbles 

m3un <- gam(Y_max ~ s(X_norm, by = traitement) + 
           s(id, bs = "re") + 
           s(id,X_norm,bs = "re") + 
           traitement + 
           s(frequence_battement) + 
           s(poid) + 
           s(lumiere) + 
           s(temperature),
           method = "REML",data = data_GAM) # 88.6%

summary(m3un) # 0.912

# m3 avec fréquence de battement en effet fixe

m3deux <- gam(Y_max ~ s(X_norm, by = traitement) + 
                s(id, bs = "re") + 
                s(id,X_norm,bs = "re") +
                traitement +
                s(frequence_battement),
                method = "REML",data = data_GAM)


# m3 avec poid en effet fixe

m3trois <- gam(Y_max ~ s(X_norm, by = traitement) + 
                s(id, bs = "re") + 
                s(id,X_norm,bs = "re") + 
                traitement + 
                s(poid),
                method = "REML",data = data_GAM)

# m3 avec lumière en effet fixe

m3quatre <- gam(Y_max ~ s(X_norm, by = traitement) + 
                  s(id, bs = "re") + 
                  s(id,X_norm,bs = "re") + 
                  traitement + 
                  s(lumiere),
                  method = "REML",data = data_GAM)

# m3 avec temperature en effet fixe

m3cinq <- gam(Y_max ~ s(X_norm, by = traitement) + 
                s(id, bs = "re") + 
                s(id,X_norm,bs = "re") +
                traitement + 
                s(temperature),
                method = "REML",data = data_GAM)

# Comparaison des modèles avec AIC, AICc et BIC

AIC(m3,m3un,m3deux,m3trois,m3quatre,m3cinq)
AICc(m3,m3un,m3deux,m3trois,m3quatre,m3cinq)
BIC(m3,m3un,m3deux,m3trois,m3quatre,m3cinq)

# test de mélange avec poid, lumière, température et freq de battement en effet fixe


                                #### PAR 2 ####

# paramètres physio poid + freq battement 

mma <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(poid) + 
             s(frequence_battement),
             method = "REML",data = data_GAM)

# paramètres env lumière + température

mmb <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(lumiere) + 
             s(temperature),
             method = "REML",data = data_GAM)

# lumière et poid

mmc <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(lumiere) + 
             s(poid),
             method = "REML",data = data_GAM)

# lumière et fréquence de battemen 

mmd <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(lumiere) + 
             s(frequence_battement),
             method = "REML",data = data_GAM)

# témpérature et frequence de battement

mme <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(temperature) + 
             s(frequence_battement),
             method = "REML",data = data_GAM)

# température et poid

mmf <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(temperature) + 
             s(poid),
             method = "REML",data = data_GAM)

                              #### PAR 3 ####

# lumiere, poids et frequence de battement 

mmg <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(lumiere) +
             s(frequence_battement) + 
             s(poid),
             method = "REML",data = data_GAM)

# lumiere, température et frequence de battement


mmh <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(lumiere) +
             s(temperature) + 
             s(frequence_battement),
             method = "REML",data = data_GAM)

# température, poids et frequence de battement

mmi <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(poid) +
             s(temperature) + 
             s(frequence_battement),
             method = "REML",data = data_GAM)

# lumière, température et poid

mmj <- gam(Y_max ~ s(X_norm, by = traitement) + 
             s(id, bs = "re") + 
             s(id,X_norm,bs = "re") + 
             traitement + 
             s(poid) +
             s(temperature) + 
             s(lumiere),
             method = "REML",data = data_GAM)

AIC(m3,m3base,m3un,m3deux,m3trois,m3quatre,m3cinq, mma, mmb, mmc, mmd, mme, mmf, mmg, mmh, mmi, mmj)
AICc(m3base, m3,m3un,m3deux,m3trois,m3quatre,m3cinq, mma, mmb, mmc, mmd, mme, mmf, mmg, mmh, mmi, mmj)
BIC(m3base,m3,m3un,m3deux,m3trois,m3quatre,m3cinq, mma, mmb, mmc, mmd, mme, mmf, mmg, mmh, mmi, mmj)

summary(m3)
summary(m3base)
summary(m3trois)
summary(mmf)

plot_smooth(m3base, view = "X_norm", cond=list(traitement = "AM"))
plot_smooth(m3base, view = "X_norm", cond=list(traitement = "AV"),add = TRUE, col = "blue")

plot_smooth(m3trois, view = "X_norm", cond=list(traitement = "AM"))
plot_smooth(m3trois, view = "X_norm", cond=list(traitement = "AV"),add = TRUE, col = "blue")

plot_smooth(mmf, view = "X_norm", cond=list(traitement = "AM"))
plot_smooth(mmf, view = "X_norm", cond=list(traitement = "AV"),add = TRUE, col = "blue")

plot_diff(m3base,view = "X_norm",comp = list(traitement = c("AM", "AV")))
plot_diff(m3trois,view = "X_norm",comp = list(traitement = c("AM", "AV")))
plot_diff(mmf,view = "X_norm",comp = list(traitement = c("AM", "AV")))

gam.check(m3base)
plot(m3base, pages = 1)

?plot_smooth()
library(mgcv)
mgcv::draw(m3base)
str(m3base)
class(m3base)
plot.gam(m3base)
library(gratia)
draw(m3base)
str(data_GAM)
k.check(m3base)
par(mfrow = c(2, 2))
?gam.check(m3base)
?plot_diff
plot_smooth(m3base, view = "X_norm", cond = list(traitement = "AM"),
            col = "cornflowerblue", fill = "black", lwd = 2,
            ylab = "Amplitude maximale de nage (Y)",
            xlab = "Point sur le segment (1–15)",
            xaxt = "n", 
            ylim = c(0.012, 0.3))

summary(m3base)
plot_smooth(m3base, view = "X_norm", cond = list(traitement = "AV"), add = TRUE, col = "red", fill = "#d95f0250", lwd = 2) 
  
axis(1, at = seq(-1, 0, length.out = 15), labels = 1:15)

legend("topright",
       legend = c("Traitement AM", "Traitement AV"),
       col = c("cornflowerblue", "red"),
       lwd = 2,
       lty = 1,
       bty = "o",
       cex = 0.95,
       box.lwd = 2,
       box.col = "black")


plot_diff(m3base, view = "X_norm", 
          comp = list(traitement = c("AM", "AV")),
          col = "#7570b3", fill = "#7570b350", lwd = 2,
          ylab = "Différence de l'amplitude maximale de nage (Y) (AM − AV)",
          xlab = "Point sur le segment (1–15)",
          main = "",
          xaxt = "n")

axis(1, at = seq(-1, 0, length.out = 15), labels = 1:15)

#### notes ####

#Y (variable réponse) : amplitude
#X (effet fixe principal) : traitement (AM vs AV)
#Effets fixes secondaires : temps_categoriel, taille, vitesse
#Effets aléatoires : (1|poisson_id), (1|zone), (1|observateur)
# GAM ####

# effet aléatoire 
# effet fixe 

# les deux ensemble c'est un modèle mixte 

# model(Y vague s(x, by = groupe(amont/aval) + après)

####

# ON commence avec un GAM et ensuite on fait un GAMM ///////////////////////////

# intercept 
# willingnes 

# faire varier donc par exemple dire que l'ordonné a l'origine est la meme ou non

# construction de pleins de modèles et comparaison

# regarder l'AIC 

# VAUT MIEUX regarder le AIC pondéré car c'est le pourcentage que ca colle

# meilleur compromis et ensuite on regarde les p-values 
# bs="re" dans la fonction gam() mets de l'effet aléatoire sur l'ordo orig

# exemlple ID

# prends en compte l'autocorrélation

# regarder le willigness à l'échelle de l'individu, su l'on considère que non on mets aléatoire ou autre
# mais ça n'a aucun sens car on considère que les pentes sont différentes

# plot diff pour voir la différence des groupes ou c'est que les groupes sont différents LE FAIRE 

# polynome de degré 1 2 3 ? ca marche mais simple 

# forme de la relation : GAMM d'abord pas mixte et après mixte 

# combien de vaguellettes veut on avoir pour avec la forme générale

# sert a trouver une relation entre deux variables qui ne ressemble à rien à la base

# wiggliness GAMM guiness 


# VARIABLES 

# variable réponse : amplitude de nage (coordonnées x et y)
# variable fixe principale : amont et aval
# effets aléatoires potentiels : zones de nage (z1 z2 etc...), debut ou fin de vidéo (t1, t2,t3,t4), observateur (catégorielle (manon, nathan,...), taille (numérique), vitesse de nage (numérique), amont ou aval (catégorielle))

# tout ca a tester sur l'amplitude de nage

?lmer()

summary(data_GAM)
id_seq <- data_GAM %>% 
  distinct(id, sequence) %>% 
  count(id, sequence) %>% 
  tidyr::pivot_wider(names_from = sequence, values_from = n, values_fill = 0) %>% 
  mutate(type = case_when(
    M > 0 & S > 0 ~ "M_et_S",
    M > 0 & S == 0 ~ "M_seulement",
    M == 0 & S > 0 ~ "S_seulement",
    TRUE ~ "aucun"
  ))

count(id_seq, type)

