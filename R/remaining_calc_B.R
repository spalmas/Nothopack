#' Remaining Variable Calculator B
#'
#' When two of Age (E), Dominant Height (HD) and Site Index (IS)
#' are given, it returns the value of the remaining parameter.
#' The coefficients for the Site index come from the Nothofagus Simulator
#' from Gezan.
#'
#' @param dom_sp Dominant species
#' @param zone Growth Zone
#' @param E Age in Years
#' @param HD dominant height
#' @param IS dite index
#' @param ... additional arguments to be passed to the low level bisection function
#'
#' @return The remaining parameter
#'
#' @seealso \code{\link{hd_coef}}. For BA, DC and NHA remainine calc see \code{\link{remaining_calc_A}}
#'
#' @examples
#' remaining_calc_B(dom_sp = 1, zone = 2, HD = 20, IS = 10)
#'
remaining_calc_B <- function(dom_sp, zone, E = NA, HD = NA, IS = NA, hd.coef = hd_coef,...){
  coef.list <- subset(hd.coef, zone == zone & dom_sp_code == dom_sp, select = c(a, b0, b1) )

  if (sum(is.na(c(E, HD, IS))) > 2 ){
    stop('There must be at least two values provided')
  } else if ( sum(is.na(c(E, HD, IS))) == 0 ){
    warning('Why would you use this calc??? You have the three variables')
  } else if (is.na(E)) {       #If Initial age is missing
    return (0)  #HOW?
  } else if (is.na(HD)) {              #If Dominant height is missing

    c <- coef.list$b0 + coef.list$b1 * IS
    return( 0.3 + coef.list$a * (1 - Nothopack:::exponent( 1 - (IS/coef.list$a)^c, (E+0.5)/18) )^(1/c))

  } else if (is.na(IS)) {          #If Site Index is missing

    #definition of IS function for bisection method (should it be somewhere else?)
    IS.eq <- function(x){
      c <- coef.list$b0 + coef.list$b1 * x
      0.3 + coef.list$a * (1 - Nothopack:::exponent( 1 - (x/coef.list$a)^c, (E+0.5)/18) )^(1/c) - HD
    }
    #bisection method
    tryCatch(pracma::bisect(IS.eq,
                            a=0, b=30,
                            maxiter = 100)$root,
             error = function(e) {NA})
  }
}

