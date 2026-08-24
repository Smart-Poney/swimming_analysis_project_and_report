#
# 18/03/2026 - 01.import_data.R 
# 
# Import raw data from MATLAB and create a single file 'coordinates'
#
# autor : FG
# project : fast_start_analysis
# latest modification : 23/06/2026
#

# Use Ctrl + Maj + o to navigate
rm(list=setdiff(ls(), "RUN"))

# folder path ----
input_folder <- "data"
dir.create("output", showWarnings = T, recursive= T)
output_folder <- "output"

# custom function to import MATLAB data ----
import_table <- function(table) {
  
  do.call(rbind, lapply(table, function(filepath) {
    
    df <- read_excel(filepath, col_names = TRUE)
    df <- as.data.frame(df)
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    n_frames <- n_cols / 2
    
    pts <- rep(1:n_rows, times = n_frames)
    frame <- rep(1:n_frames, each = n_rows)
    
    X <- numeric(n_rows * n_frames)
    Y <- numeric(n_rows * n_frames)
    k <- 1
    for (f in 1:n_frames) {
      col_x <- (f - 1) * 2 + 1
      col_y <- col_x + 1
      
      X[k:(k + n_rows - 1)] <- df[[col_x]]
      Y[k:(k + n_rows - 1)] <- df[[col_y]]
      
      k <- k + n_rows
    }
    
    df <- data.frame(
      pts = pts,
      X = X,
      Y = Y,
      frame = frame
    )
    
    tmp <- do.call(rbind, strsplit(gsub("\\.xls$", "", basename(filepath)), "_"))
    df$poisson <- tmp[,1]
    df$fs <- paste0(tmp[,1], "_",tmp[,2]) 
    rm(tmp)
    df <- df[ , c("poisson", "fs", "frame", "pts", "X", "Y")]
    
    return(df)
    
  }))
  
}

# list files ----
name <- file.path(output_folder, "coordinates.txt")
if (file.exists(name)) {
  coordinates <- read.table(name, header=TRUE)
} else {
  files <- base::list.files(input_folder, full.names = TRUE)
  coordinates <- import_table(files)
  write.table(coordinates, name, row.names = FALSE)
}
sum(is.na(coordinates))

## FIN SCRIPT 01_import_data.R 

