#' RUN.R
#'
#' Execute an R script with progress tracking
#'
#' Parse and evaluate an R script expression by expression while displaying
#' a progress bar in the console. Messages are suppressed during execution.
#' At the end of the script, a completion message is printed.
#'
#' @param cloud Character string. Path to the R script to execute. 
#'
#' @returns No explicit return value. The function is used for its side effects:
#' execution of the script, progress display, and console messages.
#'
#' @examples
#' RUN("analyse/01_formate_data.R")
#'
RUN <- function(cloud) {
  
  script_name <- basename(cloud)
  cat(sprintf("Running %s\n", script_name))
  
  # Lire et parser
  print("Script execution (%)")
  exprs <- parse(cloud)
  n <- length(exprs)
  
  # Progress bar
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  for (i in seq_along(exprs)) {
    
    # Exécuter expression
    suppressMessages(
      eval(exprs[[i]], envir = .GlobalEnv)
    )
    
    # Update progress
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  cat("\n")
  cat("==================================\n")
  cat(sprintf("END SCRIPT %s\n", script_name))
  cat("==================================\n\n")
}
