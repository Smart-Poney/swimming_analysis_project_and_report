
################################################################################
################################ ENVELOPPE #####################################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

pacman::p_load(tidyverse, readxl, ggplot2)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../..../...../SALMO/INPUT/...
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

data <- read.csv("INPUT/RESULTATS/imj_131_AV_S_z1.csv")

# /// FORMATAGE /// ####

data <- data[,-1]
colnames(data) <- c("X", "Y")

n_points <- 15

par(mfrow = c(1, 1))
    
# ------------------------------ #
# 1. Tracé simple de chaque bloc ####
# ------------------------------ #

plot(data, asp=1, type="n")
for (i in 1:(dim(data)[1]/n_points)){
  points(data[((i*n_points)-n_points+1):(i*n_points),1],
         data[((i*n_points)-n_points+1):(i*n_points),2], type="l")}

# ------------------------------ #
# 2. Décalage X selon le premier point du bloc ####
# ------------------------------ #

data2 <- data
for (i in 1:(dim(data)[1]/n_points)) {
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  data2[idx,1] <- data[idx,1] - data[idx[1],1]}

plot(data2, asp=1, type="n")
for (i in 1:(dim(data2)[1]/n_points)){
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  points(data2[idx,1], data2[idx,2], type="l")
}

# ------------------------------ #
# 3. Décalage X et Y selon le 12ème point du bloc ####
# ------------------------------ #

data2 <- data
for (i in 1:(dim(data)[1]/n_points)) {
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  ref_x <- data[idx[12], 1]
  ref_y <- data[idx[12], 2]
  data2[idx,1] <- data[idx,1] - ref_x
  data2[idx,2] <- data[idx,2] - ref_y
}
plot(data2, asp=1, type="n")
for (i in 1:(dim(data2)[1]/n_points)){
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  points(data2[idx,1], data2[idx,2], type="l", col="black")
}

#------------------------------ #
  # 3.bis Décalage X et Y selon le 12ème point du bloc / INVERSION et MARCHE ####
#------------------------------ #
  
data2 <- data
for (i in 1:(dim(data)[1]/n_points)) {
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  ref_x <- data[idx[12], 1]
  ref_y <- data[idx[12], 2]
  x <- data[idx,1] - ref_x
  y <- data[idx,2] - ref_y
  x <- x[1:12]  # points 1 à 12
  y <- y[1:12]
  data2[idx,1] <- NA
  data2[idx[1:12],1] <- x
  data2[idx,2] <- NA
  data2[idx[1:12],2] <- y}

plot(data2, asp=1, type="n")
for (i in 1:(dim(data2)[1]/n_points)){
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  keep <- which(!is.na(data2[idx,1]))
  points(data2[idx[keep],1], data2[idx[keep],2], type="l", col="black")}

# ------------------------------ #
# 4. Décalage selon moyenne locale (3 premiers points) ####
# ------------------------------ #

data2 <- data
for (i in 1:(dim(data)[1]/n_points)){
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  data2[idx,1] <- data[idx,1] - mean(data[idx[1:12],1])
  data2[idx,2] <- data[idx,2] - mean(data[idx[1:12],2])
}

plot(data2, asp=1, type="n")
for (i in 1:(dim(data2)[1]/n_points)){
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  points(data2[idx,1], data2[idx,2], type="l")
}

# ------------------------------ #
# 5. Standardisation angle du front ####
# ------------------------------ #

plot(data, asp=1, type="n", xlim=c(0,1), ylim=c(-0.5,0.5))
dd1 <- data[1:4,]
c1 <- coef(lm(Y ~ X, dd1))[2]
data2 <- data

for (i in 1:(dim(data)[1]/n_points)){
  idx <- ((i*n_points)-n_points+1):(i*n_points)
  
  dd <- data[idx[1:4],]  # 4 premiers points du bloc pour l'angle
  c2 <- coef(lm(Y ~ X, dd))[2]
  
  angle <- atan((c1 - c2)/(1 + c1*c2))  # rotation angle
  
  x <- data[idx,1]
  y <- data[idx,2]
  
  if(!is.na(angle)){
    xp <- x * cos(angle) - y * sin(angle)
    yp <- x * sin(angle) + y * cos(angle)
  } else {
    xp <- x
    yp <- y
  }
  
  # normalisation pour taille uniforme
  stand <- max(xp) - min(xp)
  xp <- (xp - min(xp)) / stand
  yp <- (yp - mean(yp[1:5])) / stand  # centré sur moyenne des 5 premiers points
  
  data2[idx,1] <- xp
  data2[idx,2] <- yp
  
  if(angle > 0){
    points(xp, yp, type="l", col="red")
  } else {
    points(xp, yp, type="l", col="blue")
  }
}

# ------------------------------ #
# 6. Max locaux par décile ####
# ------------------------------ #
mm <- matrix(0, ncol=2, nrow=10)
xx <- cut((1:100)/100, 10)
for(i in 1:nlevels(xx)){
  mm[i,1] <- mean(c((i-1)/10, i/10))
  mm[i,2] <- max(data2$Y[which(data2$X >= ((i-1)/10) & data2$X <= i/10)])
}
points(mm, type="l", lwd=3)

