#' goodloop
#'
#' @param cloud/FUN 
#'
#' @returns
#' @export
#'
#' @examples
#' 

goodloop <- function(cloud, FUN) {
  
  print("arg1 expect a length object ; arg2 expect function(f)")
  
  tot <- length(cloud)
  pb <- txtProgressBar(min = 0, max = tot, style = 3)
  
  for (i in seq_along(cloud)) {
    
    setTxtProgressBar(pb, i)
    FUN(cloud[i],i)
  }
  
  close(pb)
}
