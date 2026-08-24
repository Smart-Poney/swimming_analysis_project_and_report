#
# 10/04/26 - 05.ordination_forest.R
#
# Test of random forest as preliminar exploratative analysis
#
# auteur: FG
# project: tracking_EthoVisionXT_analysis
# latest modification: 01/07/26
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))
graphics.off()

# library ----
library(randomForest)
library(randomForestExplainer)
library(purrr)
library(pdp)
library(MASS)
library(dplyr)
library(ggplot2)
library(forcats)
library(patchwork)

# paths ----
input_folder <- "data"
output_folder <- "output"
interpolated_input_folder <- "data_interpolated"
supp_input_folder <- "supplementary_data"

# load files
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


# rajout de la biométrie ----
bio <- readxl::read_excel("supplementary_data/biometrie.xlsx")
bio$id <- as.factor(bio$id)

# rajout de l'origine ----
origin <- readxl::read_excel("supplementary_data/origin.xlsx", na = "NA")
origin <- origin %>%
  dplyr::add_row(ID = 60, origin = NA) %>% # pb avec ID = 60 inexistant
  mutate(origin = as.factor(origin),
         ID = as.factor(ID)) %>%
  rename(pool = origin)
ef <- table(origin$pool)
barres <- barplot(ef, ylim=c(0,max(ef)*1.2), col = c("darkgreen", "blue", "darkred"))
text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")

# rajout du traitement ----
treatment <- readxl::read_excel("supplementary_data/traitement.xlsx", na = "NA")
treatment <- treatment %>%
  mutate(traitement = as.factor(traitement),
         ID = as.factor(ID)) %>%
  rename(treatment = traitement)

# comparaison origine x traitement ----
treat <- origin %>%
  left_join(treatment, by = "ID", relationship = "many-to-many")
sum(is.na(treat))
eff <- table(treat$treatment, treat$pool)
barress <- barplot(eff, ylim=c(0,max(eff)*2), col = c("darkgreen", "blue", "darkred"))
legend("topright", legend=c("aval", "amont"), col=c("blue", "darkgreen"), pch=15)
text( x=barres[,1], y=ef*1.1, labels = ef, pos = 3, cex = 1.3, col = "orange")

# graphe effectifs ----
gr <- data.frame(pool = c("Cauterets","Cauterets","Lees Athas","Lees Athas"),treatment = c("amont","aval","amont","aval"),n = c(65,43,48,53))
gr <- gr %>%
  group_by(pool) %>%
  mutate(prop = n / sum(n),label = paste0(round(prop*100), "%"))
ggplot(gr,aes(x = pool,y = prop,fill = treatment)) +
  geom_col(width = 0.7,colour = "black") +
  geom_text(aes(label = label),position = position_stack(vjust = 0.5),size = 4.5,colour = "white",fontface = "bold") +
  geom_text(data = gr %>%group_by(pool) %>%summarise(total = sum(n)),
    aes(x = pool,y = 1.05,label = paste0("n = ", total)),inherit.aes = FALSE,size = 4.5) +
  scale_fill_manual(values = c("amont" = "#1b9e77","aval" = "#7570b3")) +
  scale_y_continuous(labels = scales::percent,expand = expansion(mult = c(0,0.08))) +
  labs(
    title = "Sample composition by treatment and genetic origin",
    x = "Genetic origin",
    y = "Proportion of individuals",
    fill = "Treatment") +
  theme_classic(base_size = 13) +
  theme(legend.position = "top",plot.title = element_text(face = "bold"))


# reminder Random Forest ----
#
# On utilise un Random Forest algorithm comme analyse exploratoire des données
# Chaque variable réponse pourrait permettre de discriminer plus ou moins les groupes 'origin' ou 'treatment"
# Le but est d'avoir une première idée de l'importance du potentiel de discrimination par chaque variable
#
#
# we cannot use the rawdata contained in 'cdn' because there is too many lignes to be handled with a Random Forest algorithm
# because of this we'll use the variance of each variable to caractherize each individual
#
# We want to check the importance of those variables on the treatment or the origin :
#
# ___Types of groups to dscriminate :
# - genetic pool : Cauterets (wild mediterranean trouts) vs. Less-Athas (reared atlantic trouts)
# - treatment : amont (highly fluctuant flow regime) vs. aval (non-fluctuant flow regime)  <- amont stands for upper-stream and aval stands for lower-stream
#
# ___Biological measures :
# - Height/Weight highly correlated so we'll use only the height (more physical constraint inside the tank)
#
# ___R calculated earlier (cf 03_metrics_creation) :
# - Curvature variance, R calculated earlier (cf 03_metrics_creation)
# - Snout angle Variance
# - Tail angle Variance
# - IF POSSIBLE : Swimming modes detected with FFT
#
# ___EthoVision XT ouput :
# - Frequency/ cumulative time spent in external zone
# - Frequency/ cumulative time spent in transition zone
# - Frequency/ cumulative time spent in center zone
# - Mobility (Time spent moving)
# - Total distance achieved
# - Acceleration variance
# - Speed Variance
# - Jump y/n
# - Time jumped
#
#

# pre script to avoid reloading data each time
cdn5 <- cdn3
trk5 <- trk3
stats5 <- stats3
  
#
# TO SELECT CORRECT VARIABLES FOR THE MODEL
#
cnames <- c( 
  
  "ID",
  "treatment",
  "Distance.moved.Center.point.Total.cm",
  "In.zone.External.zone...Center.point.Latency.to.First.s",
  "In.zone.External.zone...Center.point.Cumulative.Duration..",
  "In.zone.2.Transition.zone...Center.point.Latency.to.First.s",
  "In.zone.2.Transition.zone...Center.point.Cumulative.Duration..",
  "In.zone.3.Center.zone...Center.point.Cumulative.Duration..",
  "Acceleration.Center.point.Variance..cm.s...",
  "Mobility.Body.fill.Variance...",
  "max_cum_curvature_angle",                                     
  "max_cum_head_center_angle",                                     
  "max_cum_center_tail_angle",                                     
  "var_curvature_angle",                                           
  "var_head_center_angle",                                         
  "var_center_tail_angle" 
  
  ) # selectionner les colonnes variables d'interêt

RFdata <- stats5[ ,cnames] #  creer un data frame propre pour random forest
RFdata <- stats5 %>%
dplyr::select(all_of(cnames))
  
sum(is.na(cdn5)) / (dim(cdn5)[1]*dim(cdn5)[2]) * 100 # 0.28 de NA dans le tableau cdn5
sum(is.na(cdn5[,13:18])) / (dim(cdn5[,13:18])[1]*dim(cdn5[,13:18])[2]) * 100 # 0.098 de NA dans les coordonées
cdn5 <- na.omit(cdn5)
  
RFdata <- RFdata %>% # left_join pour data frame avec toutes les variables
  rename( `Distance cumulated` = Distance.moved.Center.point.Total.cm,
          `Acceleration Variance` = Acceleration.Center.point.Variance..cm.s...,
          `Mobility Variance` = Mobility.Body.fill.Variance...,
          `Time spent in external zone` = In.zone.External.zone...Center.point.Cumulative.Duration..,
          `Time spent in transition zone` = In.zone.2.Transition.zone...Center.point.Cumulative.Duration..,
          `Time spent in centre zone` = In.zone.3.Center.zone...Center.point.Cumulative.Duration..,
          `Latency to externe zone` = In.zone.External.zone...Center.point.Latency.to.First.s,
          `Latency to transition zone` = In.zone.2.Transition.zone...Center.point.Latency.to.First.s,
          `Cumulated difference of Curvature angle` = max_cum_curvature_angle,
          `Cumulated difference of Head center angle` = max_cum_head_center_angle,
          `Cumulated difference of Center tail` = max_cum_center_tail_angle,
          `Variance of difference of Curvature angle` = var_curvature_angle,
          `Variance of difference of Head center angle` = var_head_center_angle,
          `Variance of difference of Center tail` = var_center_tail_angle) %>%
    
    left_join(origin, by = c("ID" = "ID")) %>% # ajouter l'origine
    mutate ( origin_treatment = as.factor(interaction(pool, treatment))) %>%
    
    left_join(bio, by = c("ID" = "id"), relationship = "many-to-one") %>% # ajouter la biométrie
    mutate ( weight = as.numeric(weight),
             height = as.numeric(height),) %>%
    
    mutate ( treatment = as.factor(treatment),
             ID = as.factor(ID)) %>%
    droplevels() # pour éviter les niveaux fantomes, problem avec left_join() qui garde les niveaux de facteurs des précédents vecteurs

write.table(RFdata, "output/statistics_behaviour.txt", row.names = FALSE)

#
# Random Forest ----
#
# df <- RFdata

RFdata <- RFdata[, names(RFdata) != "ID"] # enlever ID du dataframe pour analyse randomForest
head(RFdata)
str(RFdata)
RFdata <- RFdata |> dplyr::select(!c(height, weight)) # enlever weight et height pour tester sans, beaucoup de poids dans RF
# le but est de chercher des variables réponses fonctionnelles de nage

RFdatdata <- RFdata
colnames(RFdatdata) <- gsub(" ", "_", colnames(RFdatdata))
RFtreatCaut <- na.omit(RFdatdata) %>% filter(pool == "Cauterets") %>% dplyr::select(!c(pool, origin_treatment)) 
RFtreatLees <- na.omit(RFdatdata) %>% filter(pool == "Lees Athas") %>% dplyr::select(!c(pool, origin_treatment)) 
RFpoolAM <- na.omit(RFdatdata) %>% filter(treatment == "Amont") %>%dplyr::select(!c(treatment, origin_treatment)) 
RFpoolAV <- na.omit(RFdatdata) %>% filter(treatment == "Aval") %>% dplyr::select(!c(treatment, origin_treatment)) 

RFtreat <- na.omit(RFdata |> dplyr::select(!c(pool, origin_treatment)))
RFpool  <- na.omit(RFdata |> dplyr::select(!c(treatment, origin_treatment)))
colnames(RFtreat) <- gsub(" ", "_", colnames(RFtreat))
colnames(RFpool) <- gsub(" ", "_", colnames(RFpool))

model_treatment <- randomForest(treatment ~ ., data=RFtreat, importance = TRUE, ntree=5000)
# lancer un random forest avec treatment comme variable a discriminer
model_origin <- randomForest(pool ~ ., data=RFpool, importance = TRUE, ntree=5000)
# lancer un random forest avec l'origine comme variable a discriminer

#
# treatment RF explo ----
#

plot(model_treatment) # 0.5 -> 50% d'erreur qui ne décroit pas avec le nombre d'arbre, pas de bonne discrimination amont vs aval
varImpPlot(model_treatment) # MeanDecreaseGini > 8 pour chaque variable, pas de variable très explicative, valeurs homogènes
model_treatment$importance
randomForestExplainer::plot_min_depth_distribution(model_treatment) # Plus visuel, mais depth > 2.5, pas de variables bien discriminatives
partialPlot(model_treatment,pred.data = RFtreat,x.var = "Cumulated_difference_of_Head_center_angle",which.class = "Amont")
partialPlot(model_treatment,pred.data = RFtreat,x.var = "Cumulated_difference_of_Head_center_angle",which.class = "Aval")
p <- pdp::partial(model_treatment,pred.var = "Cumulated_difference_of_Head_center_angle",train = RFtreat)
plotPartial(p)
# snout_angle_variance et curvature_variance peut être mais pas flagrants par rapport aux autres
# snout_angle_variance interagit le moins avec les autres et explique le plus

model_treatment_prox <- randomForest(treatment ~ ., data=RFtreat, proximity = TRUE, ntree=5000) # pour réaliser matrice de proximité et NMDS
model_treatment_prox$proximity
# plot(isoMDS(1-model_treatment_prox$proximity)$points, col=RFtreat$treatment, asp=1) # pas de clustering, pas de discrimination amont vs aval
# asp = 1 gère les ratio x/y
# au final les variables semblent peu informatives du traitement avec un randomForest

dist_mat <- 1 - model_treatment_prox$proximity
diag(dist_mat) <- 0
dist_mat[dist_mat == 0] <- 1e-6 # remplace 0 par une valeur très faible pour iso MDS qui attends une matrice sans 0
dist_obj <- as.dist(dist_mat)
mds <- isoMDS(dist_obj)
plot(mds$points,
     col = as.factor(RFtreat$treatment),
     pch = 19,
     asp = 1)

RFdata_scaled <- as.data.frame(scale(RFtreat[,-1])) # mettre les variables même échelle
RFdata_scaled$treatment <- RFtreat$treatment # un peu mieux avec des variables même échelle mais rien de bien sortant, snout_angle_variance peut être

# ACP pour visualisation à NMDS
pca <- prcomp(RFtreat[,-1], scale=TRUE)
plot(pca$x[,1:2], col=RFtreat$treatment) # peu de cluster visible

modelglm_treatment <- glm(treatment ~ ., data=RFtreat, family="binomial") #stepAIC pour un modèle glm simple
summary(modelglm_treatment)
stepAIC(modelglm_treatment) # semble garder seulement head_direction_variance, et snout_angle_variance mais AIC~254 très fort

#
# genetic origin RF explo ----
#

plot(model_origin) # 0.5 -> 50% d'erreur qui ne décroit pas avec le nombre d'arbre, pas de bonne discrimination amont vs aval
varImpPlot(model_origin) # MeanDecreaseGini > 8 pour chaque variable, pas de variable très explicative, valeurs homogènes
model_origin$importance
randomForestExplainer::plot_min_depth_distribution(model_origin) # Plus visuel, mais depth > 2.5, pas de variables bien discriminatives
partialPlot(model_origin,pred.data = RFpool,x.var = "Acceleration_Variance",which.class = "Cauterets")
partialPlot(model_origin,pred.data = RFpool,x.var = "Acceleration_Variance",which.class = "Lees Athas")
p <- pdp::partial(model_origin,pred.var = "Acceleration_Variance",train = RFpool)
plotPartial(p)
# acceleration et mobility peut être mais pas flagrants par rapport aux autres
# acceleration variance interagit le moins avec les autres et explique le plus

model_pool_prox <- randomForest(pool ~ ., data=RFpool, proximity = TRUE, ntree=5000) # pour réaliser matrice de proximité et NMDS
model_pool_prox$proximity
#plot(isoMDS(1-model_pool_prox$proximity)$points, col=RFpool$pool, asp=1) # pas de clustering, pas de discrimination amon vs aval
# asp = 1 gère les ratio x/y
# au final les variables semblent peu informatives du traitement avec un randomForest
dist_mat <- 1 - model_pool_prox$proximity
diag(dist_mat) <- 0
dist_mat[dist_mat == 0] <- 1e-6 # remplace 0 par une valeur très faible pour iso MDS qui attends une matrice sans 0
dist_obj <- as.dist(dist_mat)
mds <- isoMDS(dist_obj)
plot(mds$points,
     col = as.factor(RFpool$pool),
     pch = 19,
     asp = 1)


RFdata_scaled <- as.data.frame(scale(RFpool[,-15])) # mettre les variables même échelle
RFdata_scaled$pool <- RFpool$pool # un peu mieux avec des variables même échelle mais rien de bien sortant, snout_angle_variance peut être

# ACP pour visualisation à NMDS
pca <- prcomp(RFpool[,-15], scale=TRUE)
plot(pca$x[,1:2], col=RFpool$pool) # peu de cluster visible

modelglm_pool <- glm(pool ~ ., data=RFpool, family="binomial") #stepAIC pour un modèle glm simple
summary(modelglm_pool)
stepAIC(modelglm_pool)


# séparer selon groupe et origine ----

model_treatmentCaut <- randomForest(treatment ~ ., data=RFtreatCaut, importance = TRUE, ntree=5000)
varImpPlot(model_treatmentCaut) # MeanDecreaseGini > 8 pour chaque variable, pas de variable très explicative, valeurs homogènes
model_treatmentCaut$importance
randomForestExplainer::plot_min_depth_distribution(model_treatmentCaut) # Plus visuel, mais depth > 2.5, pas de variables bien discriminatives

model_treatmentLees <- randomForest(treatment ~ ., data=RFtreatLees, importance = TRUE, ntree=5000)
varImpPlot(model_treatmentLees) # MeanDecreaseGini > 8 pour chaque variable, pas de variable très explicative, valeurs homogènes
model_treatmentLees$importance
randomForestExplainer::plot_min_depth_distribution(model_treatmentLees) # Plus visuel, mais depth > 2.5, pas de variables bien discriminatives

model_originAM <- randomForest(pool ~ ., data=RFpoolAM, importance = TRUE, ntree=5000)
varImpPlot(model_originAM) # MeanDecreaseGini > 8 pour chaque variable, pas de variable très explicative, valeurs homogènes
model_originAM$importance
randomForestExplainer::plot_min_depth_distribution(model_originAM) # Plus visuel, mais depth > 2.5, pas de variables bien discriminatives

model_originAV <- randomForest(pool ~ ., data=RFpoolAV, importance = TRUE, ntree=5000) 
varImpPlot(model_originAV) # MeanDecreaseGini > 8 pour chaque variable, pas de variable très explicative, valeurs homogènes
model_originAV$importance
randomForestExplainer::plot_min_depth_distribution(model_originAV) # Plus visuel, mais depth > 2.5, pas de variables bien discriminatives


# graphes pour random forest ----

rownames(model_treatment$importance) <- c("Distance cumulated","Latency to externe zone", "Time spent in external zone",
                              "Latency to transition zone", "Time spent in transition zone","Time spent in centre zone",
                              "Acceleration Variance","Mobility Variance","Cumulated difference of Curvature angle",
                              "Cumulated difference of Head center angle","Cumulated difference of Center tail",
                              "Variance of difference of Curvature angle","Variance of difference of Head center angle",
                              "Variance of difference of Center tail")
rownames(model_origin$importance) <- c("Distance cumulated","Latency to externe zone", "Time spent in external zone",
                                          "Latency to transition zone", "Time spent in transition zone","Time spent in centre zone",
                                          "Acceleration Variance","Mobility Variance","Cumulated difference of Curvature angle",
                                          "Cumulated difference of Head center angle","Cumulated difference of Center tail",
                                          "Variance of difference of Curvature angle","Variance of difference of Head center angle",
                                          "Variance of difference of Center tail")
varImpPlot(model_treatment, type=1,
           col = "black",pch = 19,lwd = 2, scale=TRUE,
           main = "Relative importance using MDA - Random Forest analysis of treatment")

varImpPlot(model_origin, type=1,
           col = "black",pch = 19,lwd = 2)
randomForestExplainer::plot_min_depth_distribution(model_treatment)
randomForestExplainer::plot_min_depth_distribution(model_origin)

# 
# on analyse l'effet du traitement principalement
#

model_treatment <- randomForest(treatment ~ ., data=RFtreat, importance = TRUE, ntree=5000)
varImpPlot(model_treatment, type=1,
           col = "black",pch = 19,lwd = 2, scale=TRUE,
           main = "Relative importance using MDA - Random Forest analysis of treatment")

pdf("figure/MDAtreat.pdf", width = 13)
varImpPlot(model_treatment, type=1,
           col = "black",pch = 19,lwd = 2, scale=TRUE,
           main = "Relative importance using MDA - Random Forest analysis of treatment")
dev.off()

table(RFtreat$treatment)
prop.table(table(RFtreat$treatment))
RFtreat %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    mean = mean(Latency_to_transition_zone),
    sd = sd(Latency_to_transition_zone),
    median = median(Latency_to_transition_zone),
    IQR = IQR(Latency_to_transition_zone),
    min = min(Latency_to_transition_zone),
    max = max(Latency_to_transition_zone)
  )
ggplot(RFtreat,
       aes(x=treatment,
           y=Latency_to_transition_zone,
           fill=treatment))+
  
  geom_violin(trim=FALSE,
              alpha=.4)+
  
  geom_boxplot(width=.15,
               outlier.shape=NA)+
  
  geom_jitter(width=.08,
              alpha=.6)+
  
  theme_classic()+
  labs(y="Latency to transition zone (s)",
       x="")

RFtreat %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    mean = mean(Cumulated_difference_of_Head_center_angle),
    sd = sd(Cumulated_difference_of_Head_center_angle),
    median = median(Cumulated_difference_of_Head_center_angle),
    IQR = IQR(Cumulated_difference_of_Head_center_angle),
    min = min(Cumulated_difference_of_Head_center_angle),
    max = max(Cumulated_difference_of_Head_center_angle)
  )

ggplot(RFtreat,
       aes(x=treatment,
           y=Cumulated_difference_of_Head_center_angle,
           fill=treatment))+
  
  geom_violin(trim=FALSE,
              alpha=.4)+
  
  geom_boxplot(width=.15,
               outlier.shape=NA)+
  
  geom_jitter(width=.08,
              alpha=.6)+
  
  theme_classic()+
  labs(y="Cumulated_difference_of_Head_center_angle(deg)", # une valeur extrème, on la retire
       x="")
RFtreat[RFtreat$Cumulated_difference_of_Head_center_angle > 2e+05,]
summary(RFtreat[98,])
RFtreatclean <- RFtreat[!(RFtreat$Cumulated_difference_of_Head_center_angle > 2e+05),]
ggplot(RFtreatclean,
       aes(x=treatment,
           y=Cumulated_difference_of_Head_center_angle,
           fill=treatment))+
  
  geom_violin(trim=FALSE,
              alpha=.4)+
  
  geom_boxplot(width=.15,
               outlier.shape=NA)+
  
  geom_jitter(width=.08,
              alpha=.6)+
  
  theme_classic()+
  labs(y="Cumulated_difference_of_Head_center_angle(deg)", # une valeur extrème, on la retire
       x="")

RFtreatclean %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    mean = mean(Cumulated_difference_of_Head_center_angle),
    sd = sd(Cumulated_difference_of_Head_center_angle),
    median = median(Cumulated_difference_of_Head_center_angle),
    IQR = IQR(Cumulated_difference_of_Head_center_angle),
    min = min(Cumulated_difference_of_Head_center_angle),
    max = max(Cumulated_difference_of_Head_center_angle)
  )

model_treatment <- randomForest(treatment ~ ., data=RFtreatclean, importance = TRUE, ntree=5000)
varImpPlot(model_treatment, type=1,
           col = "black",pch = 19,lwd = 2, scale=TRUE,
           main = "Relative importance using MDA - Random Forest analysis of treatment")

pdf("figure/MDAtreat.pdf", width = 13)
varImpPlot(model_treatment, type=1,
           col = "black",pch = 19,lwd = 2, scale=TRUE,
           main = "Relative importance using MDA - Random Forest analysis of treatment")
dev.off()

# test stat de 'Latency to transition zone ----
by(RFtreatclean$Latency_to_transition_zone,RFtreatclean$treatment,shapiro.test) # pas de distrib normale, pas de t.test on utilise wilcox
car::leveneTest(Latency_to_transition_zone~treatment,data=RFtreatclean) # homogénéité ok
wilcox.test(Latency_to_transition_zone~treatment,data=RFtreatclean) # p-value = 0.08946
rstatix::wilcox_effsize(RFtreatclean,Latency_to_transition_zone~treatment)
mod <- lm(Latency_to_transition_zone~treatment,data=RFtreatclean)
summary(mod)
anova(mod)

# test stat de 'Cumulated_difference_of_Head_center_angle ----
shapiro.test(RFtreatclean$Cumulated_difference_of_Head_center_angle)
by(RFtreatclean$Cumulated_difference_of_Head_center_angle,RFtreatclean$treatment,shapiro.test) # pas de distrib normale, pas de t.test on utilise wilcox
car::leveneTest(Cumulated_difference_of_Head_center_angle~treatment,data=RFtreatclean) # homogénéité ok
wilcox.test(Cumulated_difference_of_Head_center_angle~treatment,data=RFtreatclean) # p-value = 0.08946
rstatix::wilcox_effsize(RFtreatclean,Cumulated_difference_of_Head_center_angle~treatment)
mod <- lm(Cumulated_difference_of_Head_center_angle~treatment,data=RFtreatclean)
summary(mod)
anova(mod)


# figure pour genetic ----
RFpoolclean <- RFpool[!(RFpool$Cumulated_difference_of_Head_center_angle > 2e+05),]
model_origin <- randomForest(pool ~ ., data=RFpoolclean, importance = TRUE, ntree=5000)

rownames(model_treatment$importance) <- c("Distance cumulated","Latency to externe zone", "Time spent in external zone",
                                          "Latency to transition zone", "Time spent in transition zone","Time spent in centre zone",
                                          "Acceleration Variance","Mobility Variance","Cumulated difference of Curvature angle",
                                          "Cumulated difference of Head center angle","Cumulated difference of Center tail",
                                          "Variance of difference of Curvature angle","Variance of difference of Head center angle",
                                          "Variance of difference of Center tail")
rownames(model_origin$importance) <- c("Distance cumulated","Latency to externe zone", "Time spent in external zone",
                                       "Latency to transition zone", "Time spent in transition zone","Time spent in centre zone",
                                       "Acceleration Variance","Mobility Variance","Cumulated difference of Curvature angle",
                                       "Cumulated difference of Head center angle","Cumulated difference of Center tail",
                                       "Variance of difference of Curvature angle","Variance of difference of Head center angle",
                                       "Variance of difference of Center tail")

imp_treat <- data.frame(
  Variable = rownames(model_treatment$importance),
  MDA = importance(model_treatment)[,"MeanDecreaseAccuracy"],
  Model = "Treatment"
)

imp_origin <- data.frame(
  Variable = rownames(model_origin$importance),
  MDA = importance(model_origin)[,"MeanDecreaseAccuracy"],
  Model = "Origin"
)

imp <- bind_rows(imp_treat, imp_origin)

p_imp <- ggplot(imp,
                aes(x = MDA,
                    y = fct_reorder(Variable, MDA))) +
  
  geom_point(size = 3) +
  
  facet_wrap(~Model, scales = "free_x") +
  
  labs(
    x = "Mean decrease in accuracy",
    y = NULL,
    title = "Variable importance in Random Forest models"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

p_imp

pdf("figure/RFaccuracy.pdf", width = 13)
p_imp
dev.off()

imp <- model_treatment$importance[, "MeanDecreaseAccuracy"]

df <- data.frame(
  Variable = rownames(model_treatment$importance),
  MDA = imp
)

# graphique avec pourcentage pour support oral ----
imp <- model_treatment$importance[, "MeanDecreaseAccuracy"]
imp_shift <- imp - min(imp)
importance_pct <- 100 * imp_shift / sum(imp_shift)
df <- data.frame(
  Variable = rownames(model_treatment$importance),
  MDA = importance_pct
)
# Importance relative
#df$Importance <- 100 * df$MDA / sum(df$MDA)
# Tri comme varImpPlot
df <- df[order(df$MDA, decreasing = TRUE), ]
df$Variable <- factor(df$Variable,
                      levels = rev(df$Variable))
ggplot(df, aes(x = MDA, y = Variable)) +
  geom_col(fill = "steelblue") +
  labs(x = "relative importance (%)",
       y = "",
       title = "relative importance of variables") +
  theme_bw(base_size = 14)


## END OF SCRIPT 05.ordination_forest.R

