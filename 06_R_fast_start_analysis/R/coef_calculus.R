#' coef_calculus
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
  dx <- df$X[which.max(df$pts)] - df$X[which.min(df$pts)] # utiliser which.max pas seulement max(df$X) sinon points peuvent être inversés et mauvaix calcul angle
  dy <- df$Y[which.max(df$pts)] - df$Y[which.min(df$pts)]
  atan2(dy, dx)
}

coef_calculus <- function(cloud) {
  cloud %>% 
    group_by(fs, frame) %>%  # différent de la fonction dans project 'R analyse fast start' car on groupe par fs
    mutate(
      angle = (get_angle(pick(everything())) * 180 / pi ) # pour obtenir un angle en degré (deg * 180 / pi)
    ) %>%
    ungroup()
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


