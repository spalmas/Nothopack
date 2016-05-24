#' Remaining Variable Calculator A
#'
#' When two of Basal Area (AB), Number of trees per hectare (NHA) and quadratic Diamete (DC)
#' are given, it returns the value of the remaining parameter.
#'
#' @param BA Basal area in square meters.
#' @param NHA Number of trees per hectare.
#' @param DC Quadratic Diameter
#' @return The remaining parameter
#' @examples
#' library(Nothopack)
#' remaining_calc_A(AB = 25, NHA = 1400)

remaining_calc_A <- function(AB = NA, NHA  = NA, DC  = NA){
  print(AB)
    if (sum(is.na(c(AB, DC, NHA))) >2 ){
    stop('There must be at least two values provided')
  } else if ( sum(is.na(c(AB, DC, NHA))) == 0 ){
    warning('Why would you use this calc! You have the three variables')
  } else if (is.na(AB)) {      #Estimacion de AB con NHA y DC
    return((pi/4)*NHA*(DC/100)^2)
  } else if (is.na(NHA)) {    #Estimacion de NHA0 con AB0 y DC0
    return((4/pi)*AB*(100/DC)^2)
  } else if (is.na(DC)) {    #Estimacion de DC0 con NHA0 y AB0
    return(100*((4/pi)*(AB/NHA))^0.5)
  }
}
