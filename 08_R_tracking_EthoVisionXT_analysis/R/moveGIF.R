#' moveGIF.R
#'
#' to move the GIF generated into the GIF folder
#'
#' @param NA 
#'
#' @returns nothing
#' @export 
#'
#' @examples
#' moveGIF()


moveGIF <- function(path = ".") {
  
  gif_dir <- file.path(path, "GIF")
  
  # lister les fichiers .gif (insensible à la casse)
  gif_files <- list.files(path = path,
                          pattern = "\\.gif$",
                          ignore.case = TRUE,
                          full.names = TRUE)
  
  # vérifier s'il y en a
  if (length(gif_files) == 0) {
    message("Aucun fichier GIF trouvé.")
    return(invisible(NULL))
  }
  
  # déplacer les fichiers
  file.rename(from = gif_files,
              to = file.path(gif_dir, basename(gif_files)))
  
  message(length(gif_files), " fichier(s) GIF déplacé(s) dans : ", gif_dir)
}
