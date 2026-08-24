#' coef_calculus for tracking
#' Calculate body orientation angle
#'
#' Compute the orientation angle (in degrees) of the body midline for each
#' fast-start event and frame. The angle is calculated from the first and last
#' points of the midline using their X and Y coordinates.
#'
#' @param cloud A data frame containing at least the columns \code{fs},
#'   \code{frame}, \code{pts}, \code{X}, and \code{Y}.
#'
#' @returns The input data frame with an additional column:
#'   \itemize{
#'     \item \code{angle}: body orientation angle in degrees
#'   }
#'
#' @examples
#' coef_calculus(cloud)
#'
#' @export 

get_angle <- function(df) {
  dx <- df$X2 - df$X1
  dy <- df$Y2 - df$Y1
  atan2(dy, dx)
}

coef_calculus <- function(cloud) {
  cloud %>% 
    mutate(
      angle = (get_angle(pick(everything())) * 180 / pi )
    )
}

angle_diff_abs <- function(a, b) {
  d <- a - b
  d <- (d + 180) %% 360 - 180
  abs(d)
}

angle_diff <- function(a, b) {
  d <- a - b
  d <- (d + 180) %% 360 - 180
}

coef_lm <- function(cloud) {
  cloud <- cloud %>% 
    group_by(fs, frame) %>% 
    mutate(
      coef = as.numeric(coef(lm(Y~X, data=cur_data()))["X"])
    ) %>%
    ungroup()

  return(cloud)
  
}  
