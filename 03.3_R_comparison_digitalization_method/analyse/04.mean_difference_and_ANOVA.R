#
# 03/03/26 - 04.mean_difference_and_ANOVA.R
#
# to compare variability between replicates of a single operator,
# and several operators, with one or two methods
# 
# autor: FG
# project: comparison_digitalization_method
# latest modification : 22/06/2026
#

rm(list=setdiff(ls(), "RUN"))

# library: ----
library(dplyr)
library(gplots)
library(multcompView) 

#
# comparison of mean difference 
# by operator/method ----
#

compar <- read.table("output/compar.txt", header=T, sep =",")
compar$poisson <- as.factor(compar$poisson)
compar$points <- as.numeric(compar$points)

# variabilité inter-opérateur
img <- compar[compar$methode=='imagej',] # un dataset par méthode
mat <- compar[compar$methode=='matlab',]

# ajouter une colonne points normalisés de 1 à 200
img <- img[order(img$fs, img$Image, img$operateur, img$points), ]
img$point_norm <- ave(img$points,
                      img$fs, img$Image, img$operateur,
                      FUN = function(x) seq_along(x))

mat <- mat[order(mat$fs, mat$Image, mat$operateur, mat$points), ]
mat$point_norm <- ave(mat$points,
                      mat$fs, mat$Image, mat$operateur,
                      FUN = function(x) seq_along(x))

mat <- mat %>% # permet de calculer un ecart 'sqrt((X - meanX)² + (Y - meanY)²)' par point (par point d'une frame, d'un fast-start, d'une methode), permet de regarder variabilité par operateur
  group_by(fs, Image, point_norm) %>%
  mutate(
    ecartX = abs(X - mean(X)),
    ecartY = abs(Y - mean(Y)),
    ecart_moy = sqrt((X - mean(X, na.rm = TRUE))^2 + (Y - mean(Y, na.rm = TRUE))^2)
  ) %>%
  ungroup()

img <- img %>%
  group_by(fs, Image, point_norm) %>%
  mutate(
    ecartX = abs(X - mean(X)),
    ecartY = abs(Y - mean(Y)),
    ecart_moy = sqrt((X - mean(X, na.rm = TRUE))^2 + (Y - mean(Y, na.rm = TRUE))^2)
  ) %>%
  ungroup()

# separer corerctement pour imagej 
fast <- unique(img$fs)
subimg <- list()
for (i in seq_along(fast)) {   # un dataset par fast start
  
  fs_name <- fast[i]
  df_fs <- img[img$fs == fs_name, ]
  
  subimg[[fs_name]] <- list()
  frames <- unique(df_fs$Image)
  for (j in seq_along(frames)) {   # un dataset par frame
    
    frame_name <- frames[j]
    df_frame <- df_fs[df_fs$Image == frame_name, ]
    
    subimg[[fs_name]][[frame_name]] <- list()
    points <- unique(df_frame$point_norm)
    for (k in seq_along(points)) {   # un dataset par point
      
      point_name <- points[k]
      df_point <- df_frame[df_frame$point_norm == point_name, ]
      subimg[[fs_name]][[frame_name]][[point_name]] <- df_point
    }
  }
}
rm(fast, frame_name, frames, fs_name, i, j, k, point_name, points)

# separer corerctement pour matlab
fast <- unique(mat$fs)
submat <- list()
for (i in seq_along(fast)) {   # un dataset par fast start
  
  fs_name <- fast[i]
  df_fs <- mat[mat$fs == fs_name, ]
  
  submat[[fs_name]] <- list()
  frames <- unique(df_fs$Image)
  for (j in seq_along(frames)) {   # un dataset par frame
    
    frame_name <- frames[j]
    df_frame <- df_fs[df_fs$Image == frame_name, ]
    
    submat[[fs_name]][[frame_name]] <- list()
    points <- unique(df_frame$point_norm)
    for (k in seq_along(points)) {   # un dataset par point
      
      point_name <- points[k]
      df_point <- df_frame[df_frame$point_norm == point_name, ]
      submat[[fs_name]][[frame_name]][[point_name]] <- df_point
    }
  }
}
rm(fast, frame_name, frames, fs_name, i, j, k, point_name, points)

# comparer la variabilité inter-opérateur pour les 3 fast-starts du poisson 206
opemat <- mat[mat$operateur == c("clara", "lili", "maxime", "rep2"),]
opemat <- opemat[opemat$poisson == '206',]
opeimg <- img[img$operateur == c("clara", "lili", "maxime", "rep2"),]
opeimg <- opeimg[opeimg$poisson == '206',]

opemat %>%
  group_by(operateur) %>%
  summarise(
    ecartX_moyen = mean(ecartX),
    ecartY_moyen = mean(ecartY),
    ecart_moyen = mean(ecart_moy, na.rm = TRUE),
    ecart_sd = sd(ecart_moy, na.rm = TRUE),
    .groups = "drop"
  )

opeimg %>%
  group_by(operateur) %>%
  summarise(
    ecartX_moyen = mean(ecartX),
    ecartY_moyen = mean(ecartY),
    ecart_moyen = mean(ecart_moy, na.rm = TRUE),
    ecart_sd = sd(ecart_moy, na.rm = TRUE),
    .groups = "drop"
  )

#
# mean difference 
# by method, method/replicate with unique operator ----
#

# ajouter une colonne points normalisés de 1 à 200
compar <- compar[compar$operateur == c("rep1", "rep2", "rep3"),]
compar <- compar[order(compar$methode, compar$fs, compar$Image, compar$operateur, compar$points), ]
compar$point_norm <- ave(compar$points, compar$fs, compar$Image, compar$operateur, compar$methode,
                         FUN = function(x) seq_along(x))

compar <- compar %>% # permet de calculer un ecart 'sqrt((X - meanX)² + (Y - meanY)²)' par point (par point d'une frame, d'un fast-start), permet de regarder variabilité par methode
  group_by(fs, Image, point_norm) %>%
  mutate(
    ecartX = abs(X - mean(X)),
    ecartY = abs(Y - mean(Y)),
    ecart_moy = sqrt((X - mean(X, na.rm = TRUE))^2 + (Y - mean(Y, na.rm = TRUE))^2)
  ) %>%
  ungroup()

compar %>%
  group_by(methode) %>%
  summarise(
    ecartX_moyen = mean(ecartX),
    ecartY_moyen = mean(ecartY),
    ecart_moyen = mean(ecart_moy, na.rm = TRUE),
    ecart_sd = sd(ecart_moy, na.rm = TRUE),
    .groups = "drop"
  )

compar %>%
  group_by(methode, operateur, poisson) %>%
  summarise(
    ecartX_moyen = mean(ecartX),
    ecartY_moyen = mean(ecartY),
    ecart_moyen = mean(ecart_moy, na.rm = TRUE),
    ecart_sd = sd(ecart_moy, na.rm = TRUE),
    .groups = "drop"
  )

#
# mean difference 
# by method/replicate with a unique operator and fish ----
#

compar <- compar[compar$poisson == '206' & compar$operateur != 'rep4',]
compar <- compar[order(compar$methode, compar$fs, compar$Image, compar$operateur, compar$points), ]
compar$point_norm <- ave(compar$points, compar$fs, compar$Image, compar$operateur, compar$methode,
                         FUN = function(x) seq_along(x))

compar <- compar %>% # permet de calculer un ecart 'sqrt((X - meanX)² + (Y - meanY)²)' par point (par point d'une frame, d'un fast-start), permet de regarder variabilité par methode
  group_by(fs, Image, point_norm) %>%
  mutate(
    ecartX = abs(X - mean(X)),
    ecartY = abs(Y - mean(Y)),
    ecart_moy = sqrt((X - mean(X, na.rm = TRUE))^2 + (Y - mean(Y, na.rm = TRUE))^2)
  ) %>%
  ungroup()

compar %>%
  group_by(methode) %>%
  summarise(
    ecartX_moyen = mean(ecartX),
    ecartY_moyen = mean(ecartY),
    ecart_moyen = mean(ecart_moy, na.rm = TRUE),
    ecart_sd = sd(ecart_moy, na.rm = TRUE),
    .groups = "drop"
  )

compar %>%
  group_by(methode, operateur, poisson) %>%
  summarise(
    ecartX_moyen = mean(ecartX),
    ecartY_moyen = mean(ecartY),
    ecart_moyen = mean(ecart_moy, na.rm = TRUE),
    ecart_sd = sd(ecart_moy, na.rm = TRUE),
    .groups = "drop"
  )

#
# dataset for ANOVA (1) ----
# ANOVA three factors with a single operator, the two method, and the fast-start event
#

rm(list=setdiff(ls(), "RUN"))
df <- read.table("output/compar.txt", header=T, sep =",")
df <- df[df$operateur!='clara',]
df <- df[df$operateur!='maxime',]
df <- df[df$operateur!='lili',]
df <- df[df$operateur!='rep4',]
df$frame <- as.numeric(gsub(".*-(\\d+)\\.jpg", "\\1", df$Image)) # extraire le numero de frame uniquement
df <- df[order(df$methode, df$operateur, df$fs, df$frame, df$points), ] 
df$points_norm <- ave(df$points, df$frame, df$fs, df$operateur, df$methode, # remettre les points de 1 à 200 pour chaque frame
                      FUN = function(x) seq_along(x))
df <- data.frame(df$operateur, df$poisson, df$fs, df$frame, df$points_norm, df$methode, df$X, df$Y)
colnames(df) <- c("ope","poisson","fs","frame","pts","met","X","Y")
df$frame<-as.factor(df$frame)
df$pts<-as.factor(df$pts)
df$ope<-as.factor(df$ope)
df$poisson<-as.factor(df$poisson)
df$fs<-as.factor(df$fs)
df$met<-as.factor(df$met)

df <- df %>% # permet de calculer un ecart 'sqrt((X - meanX)² + (Y - meanY)²)' par point (par point d'une frame, d'un fast-start, d'une methode)
  group_by(frame, pts) %>% # enelver ou rajouter df$methode si on veut la moyenne par methode ou non
  mutate(
    ecartX = abs(X - mean(X)),
    ecartY = abs(Y - mean(Y)),
    ecart_moy = sqrt((X - mean(X, na.rm = TRUE))^2 + (Y - mean(Y, na.rm = TRUE))^2),
    opemet = as.factor(interaction(ope, met))
  ) %>%
  ungroup()
summary(df)
verif <- df[df$ecart_moy >= 10,]

pdf("figure/ditsribution_ecart_moy_intra_met.pdf")
hist(df$ecart_moy, xlab='Ecart à la moyenne en pixels (distance euclidienne)',
     ylab = 'Fréquence', main = "Distribution de l'écart à la moyenne", col='orange')
dev.off()

moysd <- function(x) c(mean(x),sd(x))
apply(df[,9:11],2, moysd)
moy<-tapply(df$ecart_moy,list(df$ope,df$met),mean)
moymet<-tapply(df$ecart_moy,list(df$met),mean)
moy
moymet
ecartype<-tapply(df$ecart_moy,list(df$ope,df$met),sd)
ecartype

# barplot ----
#figure en barres des valeurs moyennes et sd
#The confidence intervals (ci.l = lower bound, ci.u = upper bound) to be plotted : 
dev.off()
pal=c("#8250C4","#73B761", "#B73A3A", "#12239E", "#E66C37", "#D8D7BF","#C83D95")
ci.u <- moy + ecartype 
ci.l <- moy - ecartype 
barplot2(moy, beside = T, plot.ci = T, 
         ci.l = moy, ci.u = ci.u, plot.grid = T, 
         col = c("#8250C4","#73B761", "#B73A3A"), 
         legend = rownames(moy))

#boxplot ----
box<-boxplot(ecart_moy ~ ope+met, data=df, notch=F, 
             col = c("#8250C4","#73B761", "#B73A3A"),
             main="", xlab="Population",
             legend = rownames(moy))

tab<-table(df$ope,df$met)
effectifs<-paste(names(tab),tab,sep="/n")
points(1:length(effectifs), tapply(df$ecart_moy,list(df$ope,df$met),mean), pch = 24, cex = 1, bg = "red")
text(1:length(effectifs), box$stats[5,], box$n, pos = 3, cex = 0.9, col = "black")

# preliminar tests (1) ----

#analyses preliminaires pour choisir le type de test de comparaison de moyennes: 
#test de normalite des distributions 
#test de shapiro : Ho = la distribution est normale
#tapply(df$ecart_moy,interaction(df$ope,df$met, drop=TRUE),shapiro.test) # pas de sens de tester la normalité d'autant de veleurs -> tester les résidus plutôt
#by(df, list(df$ope, df$met),function(x) with(x, shapiro.test(ecart_moy))) # pas de sens de tester la normalité d'autant de veleurs -> tester les résidus plutôt

#test d'homogeneite des variances
bartlett.test(df$ecart_moy, interaction(df$ope, df$met, drop=TRUE))
df$opemet<-with(df, paste(ope, met, sep=".")) # p-value < 2.2e-16 donc homogénéité des variances pas ok : proba d'observer ces données sous H0 très faible

# two-ways ANOVA (1) ----

# anavar<-anova(lm(ecart_moy ~ fs + ope * met, data=df, na.action=na.omit)) # quasi pas de diff entre les 2 méthodes, plus de variabilité dûe aux fs
# anavar<-anova(lm(ecart_moy ~ fs + met, data=df, na.action=na.omit)) # quasi pas de diff entre les 2 méthodes, plus de variabilité dûe aux fs
anavar<-anova(lm(ecart_moy ~ fs + met + opemet, data=df, na.action=na.omit))
anavar

ssq <- as.vector(anavar$`Sum Sq`)
names <- as.vector(rownames(anavar))
Stotal <- sum(ssq, na.rm=T)
var <- data.frame(names,ssq)
var$ssq_prop <- (var$ssq * 1 )/ unique(Stotal)
sum(var$ssq_prop)
options( digits = 8, scipen=100)
var$ssq_prop[var$names =="fs"] # inter-vidéo responsable de 52% de la variabilité
var$ssq_prop[var$names =="opemet"] # inter-opérateur responsable de 0.000004% de la variabilité -> négligeable face à inter-vidéo
var$ssq_prop[var$names =="met"] # inter-méthode responsable de 00002% de la variabilité -> négligeable face à inter-vidéo

kruskal.test(df$ecart_moy, df$ope)
kruskal.test(df$ecart_moy, df$met)
dev.off()
par(mfrow=c(2,2))
plot(aov(ecart_moy~ope+met+ope:met, data=df))

#tests apres anova : compararaison des moyennes des pop 2 a 2
aa<-pairwise.wilcox.test(df$ecart_moy, interaction(df$ope, df$met, drop=TRUE), p.adjust.method = "bonferroni")
aa
names(aa)
truc <- as.vector(aa$p.v) 
names(truc) <- paste(rep(rownames(aa$p.v), ncol(aa$p.v)),rep(colnames(aa$p.v), e=nrow(aa$p.v)), sep="-")
truc <- truc[!is.na(truc)] 
multcompLetters(truc, threshold=0.05)
moy

#
# dataset for ANOVA (2) ----
# ANOVA three factors with mutliple operators, the two method, and the fast-start event
#

df <- read.table("output/compar.txt", header=T, sep =",")
df <- df[df$operateur!='rep1',]
df <- df[df$operateur!='rep3',]
df <- df[df$operateur!='rep4',]
df <- df[df$operateur!='rep4',]
df <- df[df$poisson=='206',]
df$frame <- as.numeric(gsub(".*-(\\d+)\\.jpg", "\\1", df$Image)) # extraire le numero de frame uniquement
df <- df[order(df$methode, df$operateur, df$fs, df$frame, df$points), ] 
df$points_norm <- ave(df$points, df$frame, df$fs, df$operateur, df$methode, # remettre les points de 1 à 200 pour chaque frame
                      FUN = function(x) seq_along(x))
df <- data.frame(df$operateur, df$poisson, df$fs, df$frame, df$points_norm, df$methode, df$X, df$Y)
colnames(df) <- c("ope","poisson","fs","frame","pts","met","X","Y")
df$frame<-as.factor(df$frame)
df$pts<-as.factor(df$pts)
df$ope<-as.factor(df$ope)
df$poisson<-as.factor(df$poisson)
df$fs<-as.factor(df$fs)
df$met<-as.factor(df$met)

df <- df %>% # permet de calculer un ecart 'sqrt((X - meanX)² + (Y - meanY)²)' par point (par point d'une frame, d'un fast-start, d'une methode)
  group_by(frame, pts) %>% # enelver ou rajouter df$methode si on veut la moyenne par methode ou non
  mutate(
    ecartX = abs(X - mean(X)),
    ecartY = abs(Y - mean(Y)),
    ecart_moy = sqrt((X - mean(X, na.rm = TRUE))^2 + (Y - mean(Y, na.rm = TRUE))^2)
  ) %>%
  ungroup()

# histogram ----
hist(df$ecart_moy, xlab='Ecart à la moyenne en pixels (distance euclidienne)',
     ylab = 'Fréquence', main = "Distribution de l'écart à la moyenne", col='orange')

moy <- tapply(df$ecart_moy, list(df$met, df$fs, df$ope), mean)
sd  <- tapply(df$ecart_moy, list(df$met, df$fs, df$ope), sd)
moy
sd

# preliminar tests (2) ----
bartlett.test(ecart_moy ~ fs, data=df) 
bartlett.test(ecart_moy ~ ope, data=df)
bartlett.test(ecart_moy ~ met, data=df) # homogénéité des variances entre méthodes ok
# N > 5000 donc shapiro impossible, supposons effectif normal et testons les résidus

# two-ways ANOVA (2) ----
aov <- anova(lm(ecart_moy ~ fs * ope * met, data=df, na.action=na.omit))
aov # F-value(fs) >>>> F-value(ope/met) on peut penser que l'effet entre fs est suffisamment important pour négliger la variabilité inter opérateur et inter méthode ici
dev.off()
par(mfrow=c(2,2))
plot(aov(ecart_moy ~ fs * ope * met, data=df)) # normalité des résidus semble ~ok visuellement

ssq <- as.vector(aov$`Sum Sq`)
names <- as.vector(rownames(aov))
Stotal <- sum(ssq, na.rm=T)
var <- data.frame(names,ssq)
var$ssq_prop <- (var$ssq * 1 )/ unique(Stotal)
sum(var$ssq_prop)
options( digits = 8, scipen=100)
var$ssq_prop[var$names =="fs"] # inter-vidéo responsable de 52% de la variabilité
var$ssq_prop[var$names =="ope"] # inter-opérateur responsable de 0.000004% de la variabilité -> négligeable face à inter-vidéo
var$ssq_prop[var$names =="met"] # inter-méthode responsable de 00002% de la variabilité -> négligeable face à inter-vidéo

# au final on a bien deux méthodes répétable dont la précision n'est pas quantifiée ici

# ratio inter fs et inter met
(100/Stotal*70161232)/(100/Stotal*2447)

## END SCRIPT 04_mean_difference_and_ANOVA.R
