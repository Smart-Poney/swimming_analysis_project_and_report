#
# 04/06/2026 - 08.GAM_table.R
# 
# Figure and visual representation for model selection ML/REML
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))
devtools::install_deps(upgrade =  'never')

# library: ----
library(dplyr)
library(ggplot2)
library(itsadug)
library(tidyverse)
library(stringr)
library(forcats)
library(patchwork)
library(stringr)
library(purrr)
library(AICcmodavg)
library(RColorBrewer)
theme_set(theme_bw())

# table for model selection ----
table_model_final_segmentation <- read.table("models/table_model_final_segmentation.txt", header=TRUE)
table_model_final_cumulative_distance <- read.table("models/table_model_final_cumulative_distance.txt", header=TRUE)
table_model_final_acceleration <- read.table("models/table_model_final_acceleration.txt", header=TRUE)
table_model_final_snout_tail_ratio <- read.table("models/table_model_final_snout_tail_ratio.txt", header=TRUE)
table_model_final_ant_post_ratio <- read.table("models/table_model_final_ant_post_ratio.txt", header=TRUE)
ML_table_model_final_segmentation <- read.table("models/ML_table_model_final_segmentation.txt", header=TRUE)
ML_table_model_final_cumulative_distance <- read.table("models/ML_table_model_final_cumulative_distance.txt", header=TRUE)
ML_table_model_final_acceleration <- read.table("models/ML_table_model_final_acceleration.txt", header=TRUE)
ML_table_model_final_snout_tail_ratio <- read.table("models/ML_table_model_final_snout_tail_ratio.txt", header=TRUE)
ML_table_model_final_ant_post_ratio <- read.table("models/ML_table_model_final_ant_post_ratio.txt", header=TRUE)

# visualisation ----
table_model_final_segmentation
table_model_final_cumulative_distance
table_model_final_acceleration
table_model_final_snout_tail_ratio
table_model_final_ant_post_ratio
ML_table_model_final_segmentation
ML_table_model_final_cumulative_distance
ML_table_model_final_acceleration
ML_table_model_final_snout_tail_ratio
ML_table_model_final_ant_post_ratio

# checker les formules retenues ----
write.csv(
  c(table_model_final_segmentation[1:15, c("model","formula")],
    table_model_final_cumulative_distance[1:15, c("model","formula")],
    table_model_final_acceleration[1:15, c("model","formula")],
    table_model_final_snout_tail_ratio[1:15, c("model","formula")],
    table_model_final_ant_post_ratio[1:15, c("model","formula")],
    table_model_final_segmentation[1:15, c("model","formula")],
    ML_table_model_final_cumulative_distance[1:15, c("model","formula")],
    ML_table_model_final_acceleration[1:15, c("model","formula")],
    ML_table_model_final_snout_tail_ratio[1:15, c("model","formula")],
    ML_table_model_final_ant_post_ratio[1:15, c("model","formula")]),
  "models/check_models.csv"
)
check_models <- read.csv("models/check_models.csv", header=TRUE)

# function definition
# nommer les modeles auto ----
extract_model_signature <- function(formula_char){
  
  rhs <- strsplit(formula_char, "~")[[1]][2]
  
  terms <- trimws(
    unlist(
      strsplit(rhs, "\\+")
    )
  )
  
  effects <- c()
  
  for(term in terms){
    
    term <- trimws(term)
    
    # Effets temporels
    
    if(grepl("s\\(time\\s*,\\s*by\\s*=\\s*pool", term)){
      
      effects <- c(effects, "temps_pool")
      
    } else if(grepl("s\\(time\\s*,\\s*by\\s*=\\s*treatment", term)){
      
      effects <- c(effects, "temps_treatment")
      
    } else if(grepl("s\\(time\\s*,\\s*by\\s*=\\s*height", term)){
      
      effects <- c(effects, "temps_height")
      
    } else if(grepl("s\\(time.*bs\\s*=\\s*\"fs\"", term)){
      
      effects <- c(effects, "temps_fs")
      
    } else if(grepl("s\\(time\\)", term)){
      
      effects <- c(effects, "temps")
      
    }
    
    # Variables explicatives
    
    if(grepl("\\btreatment\\b", term))
      effects <- c(effects, "treatment")
    
    if(grepl("\\bpool\\b", term))
      effects <- c(effects, "pool")
    
    if(grepl("\\borigin_treatment\\b", term))
      effects <- c(effects, "interaction")
    
    if(grepl("\\bheight\\b", term))
      effects <- c(effects, "taille")
    
    if(grepl("\\bbief\\b", term))
      effects <- c(effects, "bief")
    
    if(grepl("temperature", term))
      effects <- c(effects, "temperature")
    
    #if(grepl("\\bsegment\\b", term)) # ne pas différencier les modèles car présence de segment mais à rajouter dans la figure
    #  effects <- c(effects, "segment")
    
    # Effets aléatoires
    
    if(grepl("poisson", term))
      effects <- c(effects, "poisson")
    
    if(grepl("\\bfs\\b", term))
      effects <- c(effects, "fs")
  }
  
  effects <- unique(effects)
  
  # on ne garde pas les RE standards
  effects <- setdiff(
    effects,
    c("poisson","fs")
  )
  
  effects <- sort(effects)
  
  paste(
    c("model", effects),
    collapse = "_"
  )
}

prepare_metric <- function(df, metric_name){
  
  df %>%
    
    mutate(
      
      metric = metric_name,
      
      model_signature =
        sapply(
          formula,
          extract_model_signature
        )
      
    ) %>%
    
    dplyr::select(
      
      model,
      model_signature,
      method,
      weight,
      deltaAIC,
      formula,
      metric
      
    )
}

# assemblage REML ----
reml_data <- bind_rows(
  prepare_metric(table_model_final_segmentation,"Segmentation"),
  prepare_metric(table_model_final_acceleration,"Acceleration"),
  prepare_metric(table_model_final_cumulative_distance,"Distance"),
  prepare_metric(table_model_final_ant_post_ratio,"Ant/Post"),
  prepare_metric(table_model_final_snout_tail_ratio,"Snout/Tail")) %>%
  
  mutate(selection_method = "REML")

# assemblage ML ----
ml_data <- bind_rows(
  
  prepare_metric(ML_table_model_final_segmentation,"Segmentation"),
  prepare_metric(ML_table_model_final_acceleration,"Acceleration"),
  prepare_metric(ML_table_model_final_cumulative_distance,"Distance"),
  prepare_metric(ML_table_model_final_ant_post_ratio,"Ant/Post"),
  prepare_metric(ML_table_model_final_snout_tail_ratio,"Snout/Tail")) %>%
  
  mutate(selection_method = "ML")

# modeles uniques ----
all_signatures <- bind_rows(
  
  reml_data %>%
    dplyr::select(model_signature),
  
  ml_data %>%
    dplyr::select(model_signature)
  
) %>%
  
  distinct() %>%
  mutate(
    complexity = stringr::str_count(model_signature, "_")
  ) %>%
  arrange(complexity)

model_order <- all_signatures$model_signature

# garder seulement les 15 premiers models de chaque table ----
reml_data_top <- reml_data %>%
  group_by(metric) %>%
  slice_min(deltaAIC, n = 15) %>%
  ungroup()

ml_data_top <- ml_data %>%
  group_by(metric) %>%
  slice_min(deltaAIC, n = 15) %>%
  ungroup()

# pour normaliser la couleur par colonne ----
reml_data_top <- reml_data_top %>%
  group_by(metric) %>%
  mutate(
    weight_scaled = weight / max(weight, na.rm = TRUE)
  ) %>%
  ungroup()
ml_data_top <- ml_data_top %>%
  group_by(metric) %>%
  mutate(
    weight_scaled = weight / max(weight, na.rm = TRUE)
  ) %>%
  ungroup()

# ne garder qu'une valeur de weightsAIC par formule de modèle ----
reml_data_plot <- reml_data_top %>%
  group_by(metric, model_signature) %>%
  mutate(weight = max(weight),
    .groups = "drop"
  )
ml_data_plot <- ml_data %>%
  group_by(metric, model_signature) %>%
  mutate(weight = max(weight),
         .groups = "drop"
  )

# ordonner les datasets selon complexité modèle ----
reml_data_plot$model_signature <-
  factor(
    reml_data_plot$model_signature,
    levels = model_order
  )

ml_data_plot$model_signature <-
  factor(
    ml_data_plot$model_signature,
    levels = model_order
  )

# pour normaliser la couleur par colonne 
combined_data <- bind_rows(reml_data_plot, ml_data_plot)
combined_data <- combined_data %>%
  group_by(selection_method, metric) %>%
  mutate(
    weight_scaled = weight / max(weight, na.rm = TRUE)
  ) %>%
  ungroup()

# vérifier que chaque métrique a au moins une valeur <- ok
combined_data %>%
  group_by(selection_method, metric) %>%
  summarise(
    n_models = n(),
    max_weight = max(weight),
    .groups = "drop"
  )
reml_data_plot %>%
  count(metric, model_signature) %>%
  filter(n > 1)

# ordonner ----

reml_data_plot$metric[reml_data_plot$metric == 'Distance'] <- "A_Distance"
reml_data_plot$metric[reml_data_plot$metric == 'Acceleration'] <- "B_Acceleration"
reml_data_plot$metric[reml_data_plot$metric == 'Segmentation'] <- "C_Segmentation"
reml_data_plot$metric[reml_data_plot$metric == 'Snout/Tail'] <- "D_Snout/Tail"
reml_data_plot$metric[reml_data_plot$metric == 'Ant/Post'] <- "E_Ant/Post"

#
# figure 1 HeatMap ----
#

fig_reml <- ggplot(
  reml_data_plot,
  aes(
    x = metric,
    y = model_signature,
    fill = weight_scaled
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.3f", weight)
    ),
    size = 3
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "white",
      "#FEE8C8",
      "#FDBB84",
      "#E34A33"
    ),
    limits = c(0,1)
  ) +
  
  labs(
    x = "",
    y = "",
    title = "Model support (REML)"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

fig_reml

#
# figure 2 HeatMap ML vs REML ----
#

fig_compare <- ggplot(
  combined_data,
  aes(
    metric,
    model_signature,
    fill = weight_scaled
  )
) +
  
  geom_tile(
    color = "white"
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.3f", weight)
    ),
    size = 2.7
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "white",
      "#FEE8C8",
      "#FDBB84",
      "#E34A33"
    ),
    limits = c(0,1)
  ) +
  
  facet_wrap(
    ~selection_method,
    ncol = 2
  ) +
  
  labs(
    x = "",
    y = "",
    fill = "AIC weight"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

fig_compare

#
# extraire les effets auto ----
#

extract_effects <- function(formula){
  tibble(
    
    treatment = str_detect(formula, "treatment"),
    pool = str_detect(formula, "pool"),
    interaction = str_detect(formula, "origin_treatment"),
    bief = str_detect(formula, "bief"),
    temperature = str_detect(formula, "temperature"),
    poisson = str_detect(formula, "poisson"),
    fs = str_detect(formula, "fs"),
    segment = str_detect(formula, "segment")
  )
}

# matrice des effets
effect_table <- reml_data %>%
  group_by(model,model_signature) %>%
  dplyr::slice(1) %>%
  ungroup()

effects <- bind_cols(
  effect_table %>%
    dplyr::select( model,model_signature),
  bind_rows(lapply(effect_table$formula, extract_effects))) 

# mettre au format long
effects_long <- effects %>%
  pivot_longer(
    cols = treatment:segment,
    names_to = "effect",
    values_to = "present"
  ) 

#
# figure 3 model composition ----
#

fig_effects <- ggplot(
  effects_long,
  aes(
    effect,
    model_signature,
    fill = present
  )
) +
  
  geom_tile(
    color = "grey70"
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "white",
      "#FEE8C8",
      "#FDBB84",
      "#E34A33"
    ),
    limits = c(0,1)
  ) +
  
  labs(
    x = "",
    y = "",
    fill = "Included"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

fig_effects

#
# SAVE ----
#

# AIC weights ----
table_weights <- reml_data %>%
  dplyr::select(model_signature, metric,weight) %>%
  pivot_wider(names_from = metric,values_from = weight)

#write.csv(table_weights,"AIC_weight_summary.csv",row.names = FALSE)

# export des figures ----
ggsave(
  "output/Figure_REML_heatmap.png",
  fig_reml,
  width = 8,
  height = 6,
  dpi = 600
)

ggsave(
  "output/Figure_REML_vs_ML_heatmap.png",
  fig_compare,
  width = 12,
  height = 6,
  dpi = 600
)

ggsave(
  "output/Figure_model_structure.png",
  fig_effects,
  width = 8,
  height = 5,
  dpi = 600
)
