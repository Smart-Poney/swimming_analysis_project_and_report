############################################
# 25/02/26 - create.R
#
# description and library
#
# autor: FG    
# project : comparison_digitalization_method
# latest modification: 22/06/2026
############################################

# create description 
usethis::use_description(check_name = FALSE)
usethis::use_mit_license()
usethis::use_package("usethis")
usethis::use_package("dplyr")
usethis::use_package("readxl")
usethis::use_package("gplots")
usethis::use_package("multcompView")

# devtools::document()
# creation d'un fichier functions.R dans R/ --
utils::file.edit("make.R")
usethis::use_r("RUN")
utils::file.edit("analyse/01_formate_data.R")
utils::file.edit("analyse/02_origin_fix.R")
utils::file.edit("analyse/03_animation.R")
utils::file.edit("analyse/04_meandifference_and_ANOVA.R")