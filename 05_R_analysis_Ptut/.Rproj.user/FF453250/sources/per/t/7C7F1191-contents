# un maxilmum de covariables pour voir au début
# voir du random forest pour exploratore

# première exploratoire (ordination peut etre) pour trouver les variables qui peuvent expliquer les différences de courbes 

# deuxième étape : gamm sur les variables qui sortent les plus influent et donc diminuer le normbe de cov 

# ordination (ordisurf ou NMDS) pour essayer de spatialiser les différences de nages (codes couleurs : ex lignées, ID, ... cov autre) essayer avec amont et aval dans le même


# /////////////////////////////////////////////////////////////////////////////#
#### //////////////////// SÉQUENCES STEADY Stationnaire /////////////////// ####
# /////////////////////////////////////////////////////////////////////////////#

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2, readr, lme4, gam,dplyr)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

load("OUTPUT/dataAMAV02.RData")
load("OUTPUT/dataAMAV02_M.RData")

# fusionner les deux jeux de données pour avoir M et S dans le même jeu de données
data <- bind_rows(datamaxabs, datamaxabs_M)

# connaitre le nombre d'id dans la séquence M et S 
data %>% filter(sequence == "S") %>% distinct(id) %>% nrow()
data %>% filter(sequence == "M") %>% distinct(id) %>% nrow()
data %>% filter(traitement == "AM") %>% distinct(id)

# //// AJOUT COVARIABLES //// #### 

# DEJA DANS LE JEU DE DONNÉES 
# bief
# zone 
# traitement (amont aval)
# taille 
# poids 
# ID

# PAS ENCORE DANS LE JEU DE DONNÉES

# saut boite # dans infopoisson # FAIT
# LIGNÉE PAS ENCORE 
# début ou fin juin # dans infopoisson # FAIT
# observateur # dans infopoisson # FAIT
# température / lumière # dans INFO # FAIT
# fréquence de battement à partir de duree_sequence # dans infopoisson

# dabord je formate ce jeu de données et ensuite je fait la jointure d'un coup (left_joint)

infopoisson <- read_delim("INPUT/info_poisson08_01_2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE,  locale = locale(decimal_mark = ",")) %>% dplyr::select(id, date_auge,saut_boite, Observateurs_ImageJ,duree_sequence, heure_auge,poid) %>% mutate(data_heure = paste(date_auge, heure_auge)) %>%
  mutate(data_heure = as.POSIXct(data_heure, format = "%d/%m/%Y %H:%M")) %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

infopoisson_M <- read_delim("INPUT/infopoisson09_01_M2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE,  locale = locale(decimal_mark = ",")) %>% dplyr::select(Observateurs_ImageJ,duree_sequence,id,) %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0")) %>% mutate( duree_sequence = as.numeric(str_replace(duree_sequence, ",", ".")))

# mettre des 0 au lieu de NA pour saut boite 

infopoisson <- infopoisson %>% mutate(saut_boite = replace_na(saut_boite, 0))

# COVARIABLES lumière et température

COV_ENV <- read_excel("INPUT/INFOS/bief_11_12.xlsx")
COV_ENV <- COV_ENV %>% rename(data_heure = `Date et heure (CET/CEST)`, temperature = `Température , °C`, lumiere = `Lumière , lux`)

# catégoriser en début et fin de mois pour juin 

infopoisson <- infopoisson %>% mutate(date_auge = as.Date(date_auge, format = "%d/%m/%Y")) %>%
  mutate(debut_fin_juin = case_when(
    date_auge >= as.Date("2025-06-01") & date_auge <= as.Date("2025-06-15") ~ "debut",
    date_auge > as.Date("2025-06-15") & date_auge <= as.Date("2025-06-30") ~ "fin")) %>% 
    dplyr::select(-date_auge)

# COV_ENV pour lumière et temp
# dans COV_ENV il y a lumière et température que je veux associer à un individu dans infopoisson. Infopoisson contient data_heure avec l'heure et la date de la vidéo entière et COV_ENV contient des températures et lumière par heure donc j'aimerais asssocier chaque individu de infopoisson à une température et une lumière 

# Arrondir infopoisson à l'heure la plus proche
infopoisson
COV_ENV

infopoisson <- infopoisson %>%
  mutate(data_heure_round = floor_date(data_heure, "hour"))

infopoisson <- infopoisson %>% left_join(
    COV_ENV %>% dplyr::select(data_heure, temperature, lumiere),
    by = c("data_heure_round" = "data_heure")) %>% dplyr::select(-data_heure_round, -heure_auge, -data_heure)

# fréquence de battement 

infopoisson <- infopoisson %>% mutate(frequence_battement = duree_sequence / 3) %>% mutate(frequence_battement = round(frequence_battement,3))

# ASSOCIATION FINALE AVEC datamaxabs #### 

data <- data %>% left_join(infopoisson, by = "id") %>% dplyr::select(-poids_g)
is.na(data) %>% sum()
# regarder d'ou viennent les NA dans data
# il y a des NA dans frequence_battement et duree_sequence
data %>% filter(is.na(frequence_battement)) %>% distinct(id)
is.na(data$frequence_battement) %>% sum()
# dans infopoisson_M garder ques les id 006,140,184,203
data %>% filter(is.na(duree_sequence)) %>% distinct(id)
is.na(data$duree_sequence) %>% sum() 
# donc les NA viennent de 60 : duree_sequence et 60 : frequence_battement

NA_restants <- data %>% filter(if_any(everything(), is.na)) %>% dplyr::select(id) %>% distinct()
data %>% filter(if_any(everything(), is.na))
infopoisson_M <- infopoisson_M %>% mutate(duree_sequence_M = duree_sequence)

# garder que les id avec NA dans data
infopoisson_M_NA <- infopoisson_M %>% filter(id %in% NA_restants$id) %>% dplyr::select(id, duree_sequence)

# infopoisson
infopoisson_M <- infopoisson_M %>% dplyr::select(id, duree_sequence) %>%
  mutate(duree_sequence = as.numeric(duree_sequence))

data <- data %>% left_join(infopoisson_M,
    by = "id",
    suffix = c("", "_M")) %>%
  mutate(duree_sequence = coalesce(duree_sequence, duree_sequence_M)) %>%
  dplyr::select(-duree_sequence_M)

# refaire le calcul de la durée de vitesse

data <- data %>% mutate(frequence_battement = duree_sequence / 3) %>% mutate(frequence_battement = round(frequence_battement,3))

# CEST BON IL NY A PLUS DE NA ENFIN HA

genetique <- read_excel("INPUT/genetique.xlsx") %>% dplyr::select(id, Origine) %>% mutate(id = str_pad(as.character(id), width = 3, pad = "0"))

data <- data %>% left_join(genetique, by = "id")

# ENREGISTREMENT EN Rdata ####

save(data, file = "OUTPUT/dataAMAV03.RData")

                # TOUTES LES COVRIABLES SONT LA #### 
             # ON PEUT COMMNECER L'ANALYSE EXPLORATOIRE #

# datamaxabs
# randomforest multivariés ou simple
# mvpart
# regarder plutot l'importance relative plutot que l'arbre 
# NMDS (MASS) pour chaque point ind des autres (un ind représente par 15 points)
# regarder la fréquence de battement (3) pour tous et avec durée vidéo 
# test t pour regarder la différence de fréquence de battement par groupe (AM AV)
# faire un rpart 
#rpart_model <- rpart(Y_max ~ zone + traitement + sequence + taille_mm + poids_g + zone + #saut_boite + Observateurs_ImageJ + frequence_battement + temperature + lumiere, data = data)
#rpart.plot(rpart_model, type = 5)
#modelA <- randomForest(y_norm ~ zone + traitement + X_xnorm, data = dataAMAV, ntree = 500)
#plot(modelA)
#varImpPlot(modelA)
#partialPlot(modelA, x.var = "traitement", pred.data = dataAMAV)

covariables <- c("poid", "saut_boite", "Observateurs_ImageJ", 
                 "temperature", "lumiere", "debut_fin_juin", "traitement", 
                 "bief", "zone", "taille_mm", "frequence_battement", "Origine")

data <- data %>% mutate(Observateurs_ImageJ = as.factor(Observateurs_ImageJ),
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
    Origine = as.factor(Origine))

library(rpart)
library(rpart.plot)

arbre <- rpart(Y_max ~ poid + saut_boite + Observateurs_ImageJ + temperature + 
                 lumiere + debut_fin_juin + traitement + bief + zone + taille_mm + frequence_battement + sequence, data = data, method = "anova")

rpart.plot(arbre, type = 5)

# faire un random forest UNIVARIÉS
library(randomForest)
set.seed(123)

model_rf <- randomForest(Y_max ~ poid + saut_boite + Observateurs_ImageJ + 
                           temperature + lumiere + debut_fin_juin + traitement + bief + zone + taille_mm + frequence_battement + sequence + Origine,data = data, ntree = 10000, importance = TRUE)

data_rf_seq_S <- data %>% filter(sequence == "S")
data_rf_seq_M <- data %>% filter(sequence == "M")
data_rf_trait_AM <- data %>% filter(traitement == "AM")
data_rf_trait_AV <- data %>% filter(traitement == "AV")

# test de faire le random forest en coupant en deux
model_rf_S <- randomForest(Y_max ~ poid + saut_boite + Observateurs_ImageJ + 
                           temperature + lumiere + debut_fin_juin + traitement + bief + zone + taille_mm + frequence_battement + sequence + Origine,data = data_rf_seq_S, ntree = 10000, importance = TRUE)

varImpPlot(model_rf_S)

model_rf_M <- randomForest(Y_max ~ poid + saut_boite + Observateurs_ImageJ + 
                           temperature + lumiere + debut_fin_juin + traitement + bief + zone + taille_mm + frequence_battement + sequence + Origine,data = data_rf_seq_M, ntree = 10000, importance = TRUE)

varImpPlot(model_rf_M)

model_rf_AM <- randomForest(Y_max ~ poid + saut_boite + Observateurs_ImageJ + 
                           temperature + lumiere + debut_fin_juin + traitement + bief + zone + taille_mm + frequence_battement + sequence + Origine,data = data_rf_trait_AM, ntree = 10000, importance = TRUE)

varImpPlot(model_rf_AM)

model_rf_AV <- randomForest(Y_max ~ poid + saut_boite + Observateurs_ImageJ + 
                           temperature + lumiere + debut_fin_juin + traitement + bief + zone + taille_mm + frequence_battement + sequence + Origine,data = data_rf_trait_AV, ntree = 10000, importance = TRUE)

varImpPlot(model_rf_AV)

print(model_rf)
importance(model_rf)
model_rf$importance

varImpPlot(model_rf)
library(randomForestExplainer)

?partial()
library(purrr)
plot_min_depth_distribution(model_rf)
partial(model_rf, 
        pred.var = "traitement", 
        plot = TRUE, rug = TRUE)



rf2 <- model_rf

# Renommer les lignes de l'importance
rownames(rf2$importance) <- c("Poids (g)", "Saut boîte", "Obs ImageJ", "Température",
                              "Lumière", "Début/Fin Juin", "Traitement", "Bief",
                              "Zone", "Taille (mm)", "Fréquence de battement", "Séquence", "Origine")

# Maintenant varImpPlot utilise ces noms
varImpPlot(rf2,
           type = 1,
           main = "",
           col = "black",
           pch = 19,
           lwd = 2)

library(pdp)

partial_rf <- partial(
  object = model_rf,
  pred.var = "Origine",
  train = data,
  type = "regression")

partial_rf <- partial(
  object = model_rf,
  pred.var = "traitement",
  train = data,
  type = "regression")

plotPartial(partial_rf, rug = TRUE)


#### graphe model RF importance ####

# Cross-validation du random forest pour voir la stabilité
#install.packages("caret")
library(caret)

data_model <- data %>%
  dplyr::select(Y_max,poid,saut_boite,Observateurs_ImageJ,temperature,lumiere,
    debut_fin_juin,traitement,bief,zone,taille_mm,frequence_battement) %>%
  mutate(Observateurs_ImageJ = as.factor(Observateurs_ImageJ),
    debut_fin_juin = as.factor(debut_fin_juin),
    traitement = as.factor(traitement),
    bief = as.factor(bief),
    zone = as.factor(zone)) %>%na.omit()

set.seed(123)
repeat_cv <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  savePredictions = "final")

set.seed(123)
train_index <- createDataPartition(
  y = data_model$Y_max,
  p = 0.7,
  list = FALSE)

training_set <- data_model[train_index, ]
testing_set  <- data_model[-train_index, ]

set.seed(123)
rf_model <- train(
  Y_max ~ .,
  data = training_set,
  method = "rf",
  trControl = repeat_cv,
  metric = "RMSE",
  importance = TRUE,
  ntree = 500)
print(rf_model)

var_imp <- varImp(rf_model, scale = FALSE)$importance
var_imp <- data.frame(
  variable = rownames(var_imp),
  importance = var_imp$Overall)

var_imp %>% arrange(importance) %>%
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "Importance des variables – Random Forest",
    x = "Variables",
    y = "Importance") +
  theme_minimal()

pred_test <- predict(rf_model, newdata = testing_set)
RMSE_test <- RMSE(pred_test, testing_set$Y_max)
R2_test <- R2(pred_test, testing_set$Y_max)
RMSE_test
R2_test
