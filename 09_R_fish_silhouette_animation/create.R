# create description 
usethis::use_description(check_name = FALSE)
usethis::use_mit_license()
usethis::use_package("usethis")
usethis::use_package("dplyr")
usethis::use_package("readxl")
usethis::use_package("ggplot2")
usethis::use_package("svglite")
usethis::use_package("gifski")
usethis::use_package("zoo")
usethis::use_package("viridis")
usethis::use_package("tydiverse")

# creation d'un fichier functions.R dans R/ --
usethis::use_r("import_data")
utils::file.edit("analyse/01_fast_start_animation_whole_fish.R")
utils::file.edit("analyse/02_animation_group.R")
utils::file.edit("make.R")
