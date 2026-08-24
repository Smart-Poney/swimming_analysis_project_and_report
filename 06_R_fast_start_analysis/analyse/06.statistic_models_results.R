#
# 13/05/2026 - 06.statistic_models_results.R
# 
# RESULTS OF GA(M)M, graphes, and tables
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))
devtools::install_deps(upgrade =  'never')
devtools::load_all()
correct_load() # function PLOT.R

# library: ----
library(dplyr)
library(ggplot2)
library(itsadug)
library(writexl)

# final models ----
model_final_segmentation <- readRDS("models/model_final_segmentation.rds")
model_final_cumulative_distance <- readRDS("models/model_final_cumulative_distance.rds")
model_final_acceleration <- readRDS("models/model_final_acceleration.rds")
model_final_efficiency <- readRDS("models/model_final_efficiency.rds")
model_final_snout_tail_ratio <- readRDS("models/model_final_snout_tail_ratio.rds")
model_final_ant_post_ratio <- readRDS("models/model_final_ant_post_ratio.rds")

# table for model selection ----
table_model_final_segmentation <- read.table("models/table_model_final_segmentation.txt", header=TRUE)
table_model_final_cumulative_distance <- read.table("models/table_model_final_cumulative_distance.txt", header=TRUE)
table_model_final_acceleration <- read.table("models/table_model_final_acceleration.txt", header=TRUE)
table_model_final_snout_tail_ratio <- read.table("models/table_model_final_snout_tail_ratio.txt", header=TRUE)
table_model_final_ant_post_ratio <- read.table("models/table_model_final_ant_post_ratio.txt", header=TRUE)
SEL_model_final_segmentation <- read.table("models/SEL_model_final_ant_post_ratio.txt", header=TRUE)
SEL_model_final_cumulative_distance <- read.table("models/SEL_model_final_ant_post_ratio.txt", header=TRUE)
SEL_model_final_acceleration <- read.table("models/SEL_model_final_ant_post_ratio.txt", header=TRUE)
SEL_model_final_snout_tail_ratio <- read.table("models/SEL_model_final_ant_post_ratio.txt", header=TRUE)
SEL_model_final_ant_post_ratio <- read.table("models/SEL_model_final_ant_post_ratio.txt", header=TRUE)

#
# segments ----
#

preGAMPLOTsegm() # we applied sqrt (root squared) on the cumsum(diff(angle_by_segment))
table_model_final_segmentation
SEL_model_final_segmentation
gratia::appraise(model_final_segmentation)
gratia::draw(model_final_segmentation, residuals=TRUE, pages=1)
summary(model_final_segmentation)
mgcv::gam.check(model_final_segmentation)

table_segmentation <- table_model_final_segmentation %>%
  dplyr::select(model_type, formula, df, AIC, deltaAIC, weight) %>%
  filter(model_type == "GAM" | model_type == "GAMM") %>%
  arrange(AIC)
write_xlsx(table_segmentation, "output/table_segmentation.xlsx")

## repres graphique ----
segm2 <- segm %>%
  mutate( treatment = as.factor(treatment),
          poisson = as.factor(poisson),
          fs = as.factor(fs),
          time = as.numeric(time),
          segment = as.factor(segment),
          pool = as.factor(pool),
          poisson_fs = as.factor(poisson_fs))
newdat <- expand.grid(
  time = seq(min(segm2$time), max(segm2$time), length = 200),
  segment = levels(segm2$segment),
  pool = levels(segm2$pool),
  height = mean(segm2$height, na.rm = TRUE),
  fs = levels(segm2$fs)[1]
)
pred <- predict(
  model_final_segmentation,
  newdata = newdat,
  se.fit = TRUE,
  exclude = "s(fs)"
)
newdat$fit <- pred$fit
newdat$se <- pred$se.fit
newdat$lwr <- (newdat$fit - 1.96 * newdat$se)
newdat$upr <- (newdat$fit + 1.96 * newdat$se)
pdf("figure/onesegmGAM.pdf", width = 10)
ggplot(newdat[newdat$segment == "segment_12_pts_191-200",], aes(time, fit, colour = pool, group = pool)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = pool),alpha = 0.2,colour = NA) +
  labs(
    x = "Time (frame)",
    y = "Cumulated difference of angle (deg)",
    colour = "Origin",
    fill = "Origin",
    title = "Temporal dynamics of the cumulated difference of angle") +
  theme_classic() +
  facet_wrap(~segment) +
  scale_colour_manual(values = c("steelblue4", "darkorange3")) +
  scale_fill_manual(values = c("steelblue4", "darkorange3"))
dev.off()

pdf("figure/segmGAM.pdf", width = 10)
ggplot(newdat, aes(time, fit, colour = pool, group = pool)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = pool),alpha = 0.2,colour = NA) +
  labs(
    x = "Time (frame)",
    y = "Cumulated difference of angle (deg)",
    colour = "Origin",
    fill = "Origin",
    title = "Temporal dynamics of the cumulated difference of angle per segment") +
  theme_classic() +
  facet_wrap(~segment) +
  scale_colour_manual(values = c("steelblue4", "darkorange3")) +
  scale_fill_manual(values = c("steelblue4", "darkorange3"))
dev.off() 

pdf("figure/segmGAMDIFF.pdf", width = 10)
diff_pool <- gratia::difference_smooths(model_final_segmentation,smooth = "s(time)",pair = c("poolCauterets","poolLees Athas"))
itsadug::plot_diff(model_final_segmentation, view = "time", comp=list(pool=c('Cauterets', 'Lees Athas'))) # pour vérifier similaire à gratia, et contrasts déja ok avec gratia
diff_pool$signif <- diff_pool$.lower_ci > 0 | diff_pool$.upper_ci < 0
sig_windows <- diff_pool |> dplyr::filter(signif) # ajouter les intervalles intéressants come dans itsadug::plot_diff
ggplot(diff_pool,aes(x = time,y = .diff)) +
  geom_ribbon(aes(ymin = .lower_ci,ymax = .upper_ci),alpha = 0.25,fill = "darkblue") +
  geom_line(linewidth = 1.2,colour = "steelblue4") +
  geom_rect(data=data.frame(xmin=c(1.000000,5.030303,7.787879,10.121212),xmax=c(3.969697,6.303030,8.848485,19.878788), # changer les valeurs selon itsadug::plotdiff
                            ymin=-Inf,ymax=Inf),inherit.aes=FALSE,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=.12) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_classic(base_size = 14) +
  labs(x = "Time",y = "Difference between the cumulated angle difference\nalong pool (Cauterets − Lees Athas)",
       title = "Estimated difference between temporal trajectories")
dev.off()

#
# distance cumulated ----
#

preGAMPLOTdcum() # we applied sqrt (root squared) on the cumsum(diff(distance_achieved))
table_model_final_cumulative_distance
SEL_model_final_cumulative_distance
gratia::appraise(model_final_cumulative_distance)
draw(model_final_cumulative_distance, residuals=TRUE, pages=1)
summary(model_final_cumulative_distance)
gam.check(model_final_cumulative_distance)

table_distance <- table_model_final_cumulative_distance %>%
  dplyr::select(model_type, formula, df, AIC, deltaAIC, weight) %>%
  filter(model_type == "GAM" | model_type == "GAMM") %>%
  arrange(AIC)
write_xlsx(table_distance, "output/table_distance.xlsx")

## repres graphique ----
dcum2 <- dfcum %>%
  mutate( treatment = as.factor(treatment),
          poisson = as.factor(poisson),
          fs = as.factor(fs),
          time = as.numeric(time),
          pool = as.factor(pool),
          poisson_fs = as.factor(poisson_fs))
newdat <- expand.grid(
  time = seq(min(dcum2$time), max(dcum2$time), length = 200),
  pool = levels(dcum2$pool),
  fs = levels(dcum2$fs)[1]
)
pred <- predict(
  model_final_cumulative_distance,
  newdata = newdat,
  se.fit = TRUE,
  exclude = "s(fs)"
)
newdat$fit <- pred$fit
newdat$se <- pred$se.fit
newdat$lwr <- (newdat$fit - 1.96 * newdat$se)^2 # passer de sqrt() à vraie valeur biologique
newdat$upr <- (newdat$fit + 1.96 * newdat$se)^2
pdf("figure/dcumGAM.pdf", width = 10)
ggplot(newdat, aes(time, (fit)^2, colour = pool, group = pool)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = pool),alpha = 0.2,colour = NA) +
  labs(
    x = "Time (s)",
    y = "Cumulated distance (cm)",
    colour = "Origin",
    fill = "Origin",
    title = "Temporal dynamics of the cumulated distance") +
  theme_classic() +
  scale_colour_manual(values = c("steelblue4", "darkorange3")) +
  scale_fill_manual(values = c("steelblue4", "darkorange3"))
dev.off() 

pdf("figure/dcumGAMDIFF.pdf", width = 10)
diff_pool <- gratia::difference_smooths(model_final_cumulative_distance,smooth = "s(time)",pair = c("poolCauterets","poolLees Athas"))
itsadug::plot_diff(model_final_cumulative_distance, view = "time", comp=list(pool=c('Cauterets', 'Lees Athas'))) # pour vérifier similaire à gratia, et contrasts déja ok avec gratia
diff_pool$signif <- diff_pool$.lower_ci > 0 | diff_pool$.upper_ci < 0
sig_windows <- diff_pool |> dplyr::filter(signif) # ajouter les intervalles intéressants come dans itsadug::plot_diff
ggplot(diff_pool,aes(x = time,y = .diff)) +
  geom_ribbon(aes(ymin = .lower_ci,ymax = .upper_ci),alpha = 0.25,fill = "darkblue") +
  geom_line(linewidth = 1.2,colour = "steelblue4") +
  geom_rect(data=data.frame(xmin=c(0.015342,0.068161),xmax=c(0.036470,0.099852), # changer les valeurs selon itsadug::plotdiff
    ymin=-Inf,ymax=Inf),inherit.aes=FALSE,
    aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=.12) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_classic(base_size = 14) +
  labs(x = "Time",y = "Difference between cumulated distances\nalong pool (Cauterets − Lees Athas)",
       title = "Estimated difference between temporal trajectories")
dev.off()

#
# acceleration ----
#

preGAMPLOTaccel() # we didn't applied any transformation on the acceleration
table_model_final_acceleration
SEL_model_final_acceleration
gratia::appraise(model_final_acceleration)
draw(model_final_acceleration, residuals=TRUE, pages=1)
summary(model_final_acceleration)
gam.check(model_final_acceleration)

table_acceleration <- table_model_final_acceleration %>%
  dplyr::select(model_type, formula, df, AIC, deltaAIC, weight) %>%
  filter(model_type == "GAM" | model_type == "GAMM") %>%
  arrange(AIC)
write_xlsx(table_acceleration, "output/table_acceleration.xlsx")

## repres graphique ----
accel2 <- dfa %>%
  mutate( treatment = as.factor(treatment),
          poisson = as.factor(poisson),
          fs = as.factor(fs),
          time = as.numeric(time),
          pool = as.factor(pool),
          poisson_fs = as.factor(poisson_fs))
newdat <- expand.grid(
  time = seq(min(accel2$time), max(accel2$time), length = 200),
  pool = levels(accel2$pool))
pred <- predict(
  model_final_acceleration,
  newdata = newdat,
  se.fit = TRUE)
newdat$fit <- pred$fit
newdat$se <- pred$se.fit
newdat$lwr <- newdat$fit - 1.96 * newdat$se
newdat$upr <- newdat$fit + 1.96 * newdat$se
pdf("figure/accelGAM.pdf", width = 10)
ggplot(newdat, aes(time/1000, fit)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),alpha = 0.2,colour = NA) +
  labs(
    x = "Time (s)",
    y = "acceleration (cm/s²)",
    title = "Temporal dynamics of acceleration") +
  theme_classic() +
  scale_colour_manual(values = "#000000")
dev.off() 

#
# efficiency ----
#

preGAMPLOTefficiency()
summary(model_final_efficiency)
plot(model_final_efficiency, residuals=TRUE)
summary(model_final_efficiency)

#
# ratio snout-tail ----
#

preGAMPLOTsnail() # we didn't applied any transformation on the ratio of angle (needs to be negative and is already normally distributed)
table_model_final_snout_tail_ratio # comme le modèle retenu contient log(Y), on a une très grande différence d'AIC donc le weight est trop faible pour tous les autres modèles
SEL_model_final_snout_tail_ratio
gratia::appraise(model_final_snout_tail_ratio)
draw(model_final_snout_tail_ratio, residuals=TRUE, pages=1)
summary(model_final_snout_tail_ratio)
gam.check(model_final_snout_tail_ratio)

table_snail <- table_model_final_snout_tail_ratio %>%
  dplyr::select(model_type, formula, df, AIC, deltaAIC, weight) %>%
  filter(model_type == "GAM" | model_type == "GAMM") %>%
  arrange(AIC)
write_xlsx(table_snail, "output/table_snail.xlsx")

## repres graphique ----
snail3 <- snail %>%
  mutate( treatment = as.factor(treatment),
          poisson = as.factor(poisson),
          fs = as.factor(fs),
          time = as.numeric(time),
          pool = as.factor(pool),
          poisson_fs = as.factor(poisson_fs))
newdat <- expand.grid(
  time = seq(min(snail3$time), max(snail3$time), length = 200),
  pool = levels(snail3$pool),
  fs = levels(snail3$fs)[1])
pred <- predict(
  model_final_snout_tail_ratio,
  newdata = newdat,
  se.fit = TRUE,
  exclude = "s(fs)"
)
newdat$fit <- pred$fit
newdat$se <- pred$se.fit
newdat$lwr <- pred$fit - 1.96 * pred$se
newdat$upr <- pred$fit + 1.96 * pred$se
pdf("figure/snailGAM.pdf", width = 10)
ggplot(newdat, aes(time, fit, colour = pool, group = pool)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = pool),alpha = 0.2,colour = NA) +
  labs(
    x = "Time (frame)",
    y = "log(|Snout-Tail ratio (deg)|)",
    colour = "Origin",
    fill = "Origin",
    title = "Temporal dynamics of snout–tail angle ratio") +
  theme_classic() +
  scale_colour_manual(values = c("steelblue4", "darkorange3")) +
  scale_fill_manual(values = c("steelblue4", "darkorange3"))
dev.off() 

pdf("figure/snailGAMDIFF.pdf", width = 10)
diff_pool <- gratia::difference_smooths(model_final_snout_tail_ratio,smooth = "s(time)",pair = c("poolCauterets","poolLees Athas"))
itsadug::plot_diff(model_final_snout_tail_ratio, view = "time", comp=list(pool=c('Cauterets', 'Lees Athas'))) # pour vérifier similaire à gratia, et contrasts déja ok avec gratia
diff_pool$signif <- diff_pool$.lower_ci > 0 | diff_pool$.upper_ci < 0
sig_windows <- diff_pool |> dplyr::filter(signif) # ajouter les intervalles intéressants come dans itsadug::plot_diff
ggplot(diff_pool,aes(x = time,y = .diff)) +
  geom_ribbon(aes(ymin = .lower_ci,ymax = .upper_ci),alpha = 0.25,fill = "darkblue") +
  geom_line(linewidth = 1.2,colour = "steelblue4") +
  geom_rect(data=data.frame(xmin=c(1.636364,4.181818,7.363636,10.757576,15.636364),xmax=c(3.545455,6.515152,9.484848,11.181818,15.636364), # changer les valeurs selon itsadug::plotdiff
                            ymin=-Inf,ymax=Inf),inherit.aes=FALSE,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=.12) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_classic(base_size = 14) +
  labs(x = "Time",y = "Difference between snout/tail ratio\nalong pool (Cauterets − Lees Athas)",
       title = "Estimated difference between temporal trajectories")
dev.off()


#
# ratio anterior-posterior ----
#

preGAMPLOTantpost() # we didn't applied any transformation on the ratio of angle (needs to be negative and is already normally distributed)
table_model_final_ant_post_ratio # comme le modèle retenu contient log(Y), on a une très grande différence d'AIC donc le weight est trop faible pour tous les autres modèles
SEL_model_final_ant_post_ratio
gratia::appraise(model_final_ant_post_ratio)
gratia::draw(model_final_ant_post_ratio, residuals=TRUE, pages=1)
summary(model_final_ant_post_ratio)
mgcv::gam.check(model_final_ant_post_ratio)

table_antpost <- table_model_final_ant_post_ratio %>%
  dplyr::select(model_type, formula, df, AIC, deltaAIC, weight) %>%
  filter(model_type == "GAM" | model_type == "GAMM") %>%
  arrange(AIC)
write_xlsx(table_antpost, "output/table_antpost.xlsx")

## repres graphique ----
antpost3 <- antpost %>%
  mutate( treatment = as.factor(treatment),
          poisson = as.factor(poisson),
          fs = as.factor(fs),
          time = as.numeric(time),
          pool = as.factor(pool),
          poisson_fs = as.factor(poisson_fs))
newdat <- expand.grid(
  time = seq(min(antpost3$time), max(antpost3$time), length = 200),
  pool = levels(antpost3$pool),
  fs = levels(antpost3$fs)[1])
pred <- predict(
  model_final_ant_post_ratio,
  newdata = newdat,
  se.fit = TRUE,
  exclude = "s(fs)"
)
newdat$fit <- pred$fit
newdat$lwr <- pred$fit - 1.96 * pred$se.fit
newdat$upr <- pred$fit + 1.96 * pred$se.fit # on ajouter exp() pour retrouver les données biologiques transformées avec log(abs())
newdat$se <- pred$se.fit
pdf("figure/antpostGAM.pdf", width = 10)
ggplot(newdat, aes(time, fit, colour = pool, group = pool)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = pool),alpha = 0.2,colour = NA) +
  labs(
    x = "Time (frame)",
    y = "log(|Anterior-Posterior ratio (deg)|)",
    colour = "Origin",
    fill = "Origin",
    title = "Temporal dynamics of anterior–posterior angle ratio") +
  theme_classic() +
  scale_colour_manual(values = c("steelblue4", "darkorange3")) +
  scale_fill_manual(values = c("steelblue4", "darkorange3"))
dev.off()  

pdf("figure/antpostGAMDIFF.pdf", width = 10)
diff_pool <- gratia::difference_smooths(model_final_ant_post_ratio,smooth = "s(time)",pair = c("poolCauterets","poolLees Athas"))
itsadug::plot_diff(model_final_ant_post_ratio, view = "time", comp=list(pool=c('Cauterets', 'Lees Athas'))) # pour vérifier similaire à gratia, et contrasts déja ok avec gratia
diff_pool$signif <- diff_pool$.lower_ci > 0 | diff_pool$.upper_ci < 0
sig_windows <- diff_pool |> dplyr::filter(signif) # ajouter les intervalles intéressants come dans itsadug::plot_diff
ggplot(diff_pool,aes(x = time,y = .diff)) +
  geom_ribbon(aes(ymin = .lower_ci,ymax = .upper_ci),alpha = 0.25,fill = "darkblue") +
  geom_line(linewidth = 1.2,colour = "steelblue4") +
  geom_rect(data=data.frame(xmin=c(1.636364,4.181818,7.151515,15.000000),xmax=c(3.333333,6.303030,9.272727,16.272727), # changer les valeurs selon itsadug::plotdiff
                            ymin=-Inf,ymax=Inf),inherit.aes=FALSE,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=.12) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_classic(base_size = 14) +
  labs(x = "Time",y = "Difference between anterior/posterior ratio\nalong pool (Cauterets − Lees Athas)",
    title = "Estimated difference between temporal trajectories")
dev.off()

# comparaison ratio ----
ratio <- as.data.frame(cbind(antpost3$Y, snail3$Y))
colnames(ratio) <- c("antpost", "snail")
cor.test(ratio$antpost, ratio$snail)
hist(ratio$antpost,breaks = 30,freq = TRUE,col = rgb(0,0,1,0.4),
     xlim = range(c(ratio$antpost, ratio$snail)),main = "Distribution des angles",xlab = "Angle")
hist(ratio$snail,breaks = 30,freq = TRUE,col = rgb(1,0,0,0.4),add = TRUE)
curve(dnorm(x,mean = mean(ratio$antpost, na.rm = TRUE), sd=sd(ratio$antpost, na.rm = TRUE)),
  col = "blue",lwd = 3,add = TRUE)
curve(dnorm(x,mean = mean(ratio$snail, na.rm = TRUE), sd=sd(ratio$snail, na.rm = TRUE)),
  col = "red",lwd = 3,add = TRUE)
legend("topright",legend = c("antpost", "snail"),fill = c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)),border = NA)
t.test(ratio$antpost, ratio$snail)
aov <- aov(ratio$antpost~ratio$snail)
anova(aov)


# Les variables locomotrices et morphométriques présentent des dynamiques temporelles fortement non linéaires,
# mieux capturées par des modèles additifs généralisés (GAM) que par des modèles linéaires classiques (LM, GLM). 
# Les différences entre pools se manifestent principalement par des trajectoires temporelles distinctes
# plutôt que par des différences de niveau moyen. Les effets des traitements expérimentaux sont globalement
# faibles ou dépendants du contexte environnemental. Enfin, une forte variabilité interindividuelle est observée 
# dans la majorité des modèles avec l'évenement de fast-start qui reste un facteur très important.

# summary model table ----
model_summary_table <- function(tab) {
  
  clean_formula <- function(f) {
    
    # réponse
    response <- trimws(strsplit(f, "~")[[1]][1])
    
    # RHS
    rhs <- trimws(strsplit(f, "~")[[1]][2])
    
    # termes
    terms <- unlist(strsplit(rhs, "\\+"))
    terms <- trimws(terms)
    
    # smooths
    temporal <- terms[grepl("time", terms)]
    
    # random
    random <- c()
    
    if(any(grepl("s\\(poisson.*,bs *= *\"re\"", terms))) {
      random <- c(random, "poisson")
    }
    
    if(any(grepl("s\\(fs.*,bs *= *\"re\"", terms))) {
      random <- c(random, "fs")
    }
    
    # fixed
    fixed <- terms[
      !(terms %in% temporal) &
        !grepl("bs *= *\"re\"", terms)
    ]
    
    fixed <- fixed[fixed != "1"]
    
    list(
      response = response,
      temporal = paste(temporal, collapse = " + "),
      fixed = paste(fixed, collapse = " + "),
      random = paste(random, collapse = " + ")
    )
  }
  
  parsed <- lapply(tab$formula, clean_formula)
  
  out <- data.frame(
    model = tab$model,
    type = tab$model_type,
    response = sapply(parsed, `[[`, "response"),
    temporal = sapply(parsed, `[[`, "temporal"),
    fixed = sapply(parsed, `[[`, "fixed"),
    random = sapply(parsed, `[[`, "random"),
    AIC = round(tab$AIC,2),
    deltaAIC = round(tab$deltaAIC,2),
    stringsAsFactors = FALSE
  )
  
  # ordre logique
  out$type <- factor(out$type,
                     levels = c("LM","GLM","GAM","GAMM"))
  
  out <- out[order(out$type, out$deltaAIC),]
  
  return(out)
}
table1 <- model_summary_table(table_model_final_segmentation)

## FIN SCRIPT 06.statistic_models_results.R
