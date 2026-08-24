#' ask.R
#'
#' @param NA 
#'
#' @returns an action
#' @export
#'
#' @examples
#' if (ask_yes_no("Launch GIF creation script (04) ?")) { 
#'    RUN("analyse/04.GIF_for_angle_visualization.R")
#' }


ask_yes_no <- function(question = "Do it ?") {
  repeat {
    answer <- tolower(trimws(readline(paste0(question, " (y/n) : "))))
    
    if (answer %in% c("y", "yes")) {
      return(TRUE)
    } else if (answer %in% c("n", "no")) {
      return(FALSE)
    } else {
      cat("Invalid input, y or n.\n")
    }
  }
}

ask_time_limit <- function(cloud) {
  repeat {
    
    max_time <- max(cloud)
    input <- readline(paste0("time limit for GIF ? (expect long time process for long time analysis) ; (max = ", max_time, ") : "))
    value <- as.numeric(input)
    
    if (!is.na(value) && value > 0 && value <= max_time) {
      return(value)
    } else {
      cat("Invalid. Must be a number between 0 and ", max_time, "\n")
    }
  }
}

ask_individual <- function(cloud) {
  repeat {
    
    max_indiv <- max(na.omit(as.numeric(cloud))) #les NA proviennent du 63_bis chr -> num impossible
    input <- readline(paste0("individual number ? (one at a time) ; (max = ", max_indiv, ") : "))
    value <- as.numeric(input)
    
    if (!is.na(value) && value >= 1 && value <= max_indiv) {
      return(value)
    } else {
      cat("Invalid. Must be a number between 1 and ", max_indiv, "\n")
    }
  }
}

