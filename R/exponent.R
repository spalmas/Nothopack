#' Exponent function
#'
#' a^pow exponential function. Useful to avoid site index estimate errors
#'
#' @param a parameter
#' @param pow parameter
#'
#' @return The remaining parameter
#'
#' @seealso see \code{\link{remaining_calc_B}}
#'
#' @examples
#' library(Nothopack)
#' exponent(2, 2)
exponent <- function(a, pow) (abs(a)^pow)*sign(a)
