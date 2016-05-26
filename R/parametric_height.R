#' Height Models in function of Whole Stand parameters
#'
#' Calculates individual tree heights in function of whole stand parameter
#' like Dominant Height (HD), Quadratic Diameter (DC) and also DBH
#' HT = b0 + b1 HD + b2 DC0.95 + b3 exp(-0.08 DAP) + b4 HD3 exp(0.08 DAP) + b5 DC3 exp(-0.08 DAP)
#' The coefficients come from the Nothofagus Simulator
#' from Gezan.
#'
#' @param HD Dominant height in meters.
#' @param DC Quadratic Diameter in centimeters.
#' @param DBH Diameter at Breast Height in centimeters.
#' @return The individual Height in meters
#' @examples
#' height_param(HD = 15, DC = 12, DBH = 14, dom_sp = 1)
#' library(Nothopack)

height_param <- function(dom_sp, zone, HD = NA, DC = NA, DBH = NA, hparam.coef = hparam_coef,...){
  coef.list <- subset(hparam.coef, zone == zone & dom_sp_code == dom_sp, select = c(b0, b1, b2, b3, b4, b5) )

  return(coef.list$b0 +  coef.list$b1 * HD + coef.list$b2 * (DC^0.95)
         + coef.list$b3 * exp(-0.08 * DBH) + coef.list$b4 * (HD^3) *exp(-0.08*DBH)
         + coef.list$b5 *(DC^3)*exp(-0.08 * DBH))
}
