# Matrice de distance selon Frechet et calcul de la NMDS 

load("OUTPUT/dataAMAV03.RData")

# ATTENTION : CHARGER LE BON JEU DE DONNEES 
# AJOUTER LA Variable Statique/Mobile pour visualisation dans le graphe NMDS 

# Charger les librairies 
library(dplyr)
library(SimilarityMeasures)  #fonction FrechetDist ne fonctionne pas 
library(longitudinalData)   #fonction FrechetDist ne fonctionne pas 
library(vegan)

View(data)

#Concaténation de id_traitement
data$id_seq <- paste0(data$id, "_", data$sequence)
data$Origine <- as.factor(data$Origine)
# Matrice de distance
traj_df <- data %>%
  arrange(id_seq, trace) %>%   # très important : ordre des points
  group_by(id_seq) %>%
  summarise(
    traj = list(as.matrix(cbind(X_norm, Y_max))),
    traitement = first(traitement),
    taille_mm = first(taille_mm),
    bief = first(bief),
    zone = first(zone),
    saut_boite = first(saut_boite),
    frequence_battement = first(frequence_battement),
    sequence=first(sequence),
    Origine=first(Origine),
    poid=first(poid)
  )

head(traj_df)

traj_df$traj[[1]]  #vérification de la matrice pour le premier individu 


# Distance de Fréchet sans aucun package 

frechet_discrete <- function(P, Q) {
  n <- nrow(P)
  m <- nrow(Q)
  
  ca <- matrix(-1, n, m)
  
  dist_euc <- function(a, b) {
    sqrt(sum((a - b)^2))
  }
  
  recurse <- function(i, j) {
    if (ca[i, j] > -1) {
      return(ca[i, j])
    } else if (i == 1 && j == 1) {
      ca[i, j] <<- dist_euc(P[1, ], Q[1, ])
    } else if (i > 1 && j == 1) {
      ca[i, j] <<- max(recurse(i - 1, 1),
                       dist_euc(P[i, ], Q[1, ]))
    } else if (i == 1 && j > 1) {
      ca[i, j] <<- max(recurse(1, j - 1),
                       dist_euc(P[1, ], Q[j, ]))
    } else if (i > 1 && j > 1) {
      ca[i, j] <<- max(
        min(
          recurse(i - 1, j),
          recurse(i - 1, j - 1),
          recurse(i, j - 1)
        ),
        dist_euc(P[i, ], Q[j, ])
      )
    } else {
      ca[i, j] <<- Inf
    }
    return(ca[i, j])
  }
  
  recurse(n, m)
}

# Calcul de la matrice de distance 
n <- nrow(traj_df)
D <- matrix(0, n, n)

for (i in 1:n) {
  for (j in 1:n) {
    D[i, j] <- frechet_discrete(
      traj_df$traj[[i]],
      traj_df$traj[[j]]
    )
  }
}

dist_frechet <- as.dist(D)

# Vérification
summary(dist_frechet)

#View(dist_frechet)
# Heatmap de la matrice de distance 
D_mat <- as.matrix(dist_frechet)

heatmap(
  D_mat,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  col = colorRampPalette(c("white", "orange", "red"))(100),
  margins = c(6, 6)
)

D_mat  # Valeurs de la matrice

# Structure globale des distances
hist(
  dist_frechet,
  breaks = 30,
  col = "grey80",
  border = "white",
  main = "Distribution des distances de Fréchet",
  xlab = "Distance de Fréchet"
)

# NMDS

nmds <- metaMDS(
  dist_frechet,
  k = 2,
  trymax = 100,
  autotransform = FALSE
)

nmds$stress

# Plot de la NMDS selon le traitement 

ordiplot(nmds, type = "n")
points(
  nmds,
  col = as.factor(traj_df$traitement),
  pch = 19
)

# Ajout des étiquettes individus
text(
  nmds,
  labels = traj_df$id_seq,
  pos = 3,          # position au-dessus du point
  cex = 0.7,        # taille du texte
  col = "black"
)

legend(
  "topright",
  legend = levels(as.factor(traj_df$traitement)),
  col = seq_along(levels(as.factor(traj_df$traitement))),
  pch = 19,
  bty = "n"
)


# Permanova
# adonis2(
# dist_frechet ~ traitement,
#data = traj_df,
#permutations = 999
#)


# ===============================
# NMDS différenciation AM / AV (méthode vegan OFFICIELLE)
# ===============================

ordiplot(nmds, type = "n")

palette <- c("#FF6666", "#66CCFF")

# Polygones convexes (D'ABORD)
ordihull(
  nmds,
  groups = traj_df$traitement,
  draw = "polygon",
  border = c("#FF6666", "#66CCFF"),
  col = NA,
  lwd = 2
)

# Points (INCHANGÉS)
points(
  nmds,
  col = palette[as.factor(traj_df$traitement)],
  pch = 19
)

# Labels (INCHANGÉS)
text(
  nmds,
  labels = traj_df$id_seq,
  pos = 3,
  cex = 0.5,
  col = "black"
)

# Légende
legend(
  "topright",
  legend = levels(as.factor(traj_df$traitement)),
  col = palette,
  pch = 19,
  bty = "n"
)


# ===============================
# NMDS différenciation Mobile / Stationnaire (méthode vegan OFFICIELLE)
# =============================== #66FF33

ordiplot(nmds, type = "n")
palette_mouv <- c("#9933CC", "#66FFCC")

# Polygones convexes (D'ABORD)
ordihull(
  nmds,
  groups = traj_df$sequence,
  draw = "polygon",
  border = c("#9933CC", "#66FFCC"),
  col = NA,
  lwd = 2
)

# Points (INCHANGÉS)
points(
  nmds,
  col = palette_mouv[as.factor(traj_df$sequence)],
  pch = 19
)

# Labels (INCHANGÉS)
text(
  nmds,
  labels = traj_df$id_seq,
  pos = 3,
  cex = 0.9,
  col = "black"
)

# Légende
legend(
  "topright",
  legend = levels(as.factor(traj_df$sequence)),
  col = palette_mouv[as.factor(traj_df$sequence)],
  pch = 19,
  pt.cex = 1.5, 
  cex = 1,
  bty = "n"
)


# ===============================
# NMDS différenciation Origine (méthode vegan OFFICIELLE)
# ===============================

ordiplot(nmds, type = "n")
palette_orig <- c("grey", "black")

# Polygones convexes (D'ABORD)
ordihull(
  nmds,
  groups = traj_df$Origine,
  draw = "polygon",
  border = c("grey", "black"),
  col = NA,
  lwd = 2
)

# Points (INCHANGÉS)
points(
  nmds,
  col = palette_orig[as.factor(traj_df$Origine)],
  pch = 19
)

# Labels (INCHANGÉS)
text(
  nmds,
  labels = traj_df$id_seq,
  pos = 3,
  cex = 0.5,
  col = "black"
)

# Légende
orig_levels <- levels(as.factor(traj_df$Origine))

legend(
  "topright",
  legend = orig_levels,
  col = palette_orig,
  pch = 19,
  bty = "n"
)


# ===============================
# NMDS différenciation Poids (méthode vegan OFFICIELLE)
# ===============================

ordiplot(nmds, type = "n")


# Palette continue du beige au rouge
pal_poid <- colorRampPalette(c("#F5E6C8", "#FF0000"))

#D7301F

n_col <- 100
cols <- pal_poid(n_col)

# Association poids avec la couleur
poid_col <- cols[
  cut(
    traj_df$poid,
    breaks = n_col,
    include.lowest = TRUE
  )
]

# Points
points(
  nmds,
  col = poid_col,
  pch = 19
)

# Labels
text(
  nmds,
  labels = traj_df$id_seq,
  pos = 3,
  cex = 0.5,
  col = "black"
)


# Légende continue
legend_vals <- pretty(range(traj_df$poid), n = 5)

legend(
  "bottomleft",
  legend = legend_vals,
  fill = pal_poid(length(legend_vals)),
  title = "Poids (g)",
  cex = 0.6,
  title.cex = 0.7,
  inset = 0.02,
  bty = "n"
)


