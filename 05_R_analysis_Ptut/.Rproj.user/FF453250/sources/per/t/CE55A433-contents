#######################################################################################
#                                                                                     #
#               Script pour réorganiser aléatoirement les vidéos                      #
#                                                                                     #
#######################################################################################

library(readxl)
data_cov_steady <- read_excel("INPUT/INFOS/data_cov_steady.xlsx", 
                              na = "NA")
# View(data_cov_steady)

# Separation des videos prioritaire ou non, puis tirage aléatoire
non_prioritaires <- c(1:5, 81, 116, 174, 183:192)
df_nonprio <- data_cov_steady[non_prioritaires, ]
df_prio <- data_cov_steady[-non_prioritaires, ]

set.seed(123)  
df_prio_random <- df_prio[sample(nrow(df_prio)), ]
set.seed(123) 
df_nonprio_ramdom <- df_nonprio[sample(nrow(df_nonprio)), ]

# Dataframe reorganisé avec ordre aleatoire
data_cov_steady_random <- rbind(df_prio_random, df_nonprio_ramdom)

# Vérifier
head(data_cov_steady_random)
tail(data_cov_steady_random)
View(data_cov_steady_random)

# Exportation en excel
install.packages("openxlsx")
library(openxlsx)
# write.xlsx(data_cov_steady_random, file = "data_cov_steady_random.xlsx") # exporte par défaut dans le même working directory que le script
