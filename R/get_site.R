#' Calculates remaining stand level variable from the set: ED, HD, SI
#'
#' \code{get_site} When two of stand level variebles: dominat age (ED, year), dominant height (HD, m) and site index (SI, m)
#' are given, it returns the value of the remaining stand level parameter.
#' Note: The coefficients for the site index curves come from Gezan et al. (2006).
#'
#' @param dom_sp Dominant species (1: Rauli, 2: Roble, 3: Coigue)
#' @param zone Growth zone (1, 2, 3, 4)
#' @param ED Dominant age (year)
#' @param HD Dominant height (m)
#' @param SI Site index at reference dominant age of 18 (m)
#' @param ... additional arguments to be passed to the low level bisection function
#'
#' @references 
#' Gezan et al. (2006). Simulador Nothofagus. Internal Report XXXXXXXX
#'
#' @return The missing stand level parameter
#'
#' @seealso \code{\link{hd_coef}}. For BA, QD and N see \code{\link{get_stand}}
#'
#' @examples
#' remaining_calc_B(dom_sp=1, zone=2, HD=20, SI=10)
#' remaining_calc_B(dom_sp=1, zone=2, ED=25, SI=10)
#' remaining_calc_B(dom_sp=1, zone=2, ED=25, HD=14)

get_site <- function(dom_sp, zone, ED=NA, HD=NA, SI=NA, hd.coef=hd_coef,...){
  coef.list <- subset(hd_coef, hd_coef_zone == zone & hd_coef_sp_code == dom_sp,
                      select = c(hd_coef_a, hd_coef_b0, hd_coef_b1) )

  if (sum(is.na(c(ED, HD, SI))) > 2 ){
    stop('There must be at least two stand parameters provided')
    
  } else if ( sum(is.na(c(ED, HD, SI))) == 0 ){
    warning('Why would you use this calculation? You have allready have the three variables')
    
  } else if (is.na(ED)) {               # If Initial age is missing
    parm<-0       #HOW?
    
  } else if (is.na(HD)) {              # If Dominant height is missing
    c <- coef.list$hd_coef_b0 + coef.list$hd_coef_b1 * SI
    parm <- 0.3 + coef.list$hd_coef_a * (1-Nothopack:::exponent(1-(SI/coef.list$hd_coef_a)^c, (ED+0.5)/18) )^(1/c)
    return(parm)

  } else if (is.na(SI)) {              # If Site Index is missing

    #definition of IS function for bisection method (should it be somewhere else?)
    SI.eq <- function(x){
      c <- coef.list$hd_coef_b0 + coef.list$hd_coef_b1 * x
      0.3 + coef.list$hd_coef_a * (1-Nothopack:::exponent(1-(x/coef.list$hd_coef_a)^c, (ED+0.5)/18))^(1/c) - HD
    }
    #bisection method
    parm <- tryCatch(pracma::bisect(SI.eq,
                            a=0, b=30,
                            maxiter = 100)$root,
             error = function(e) {NA})
  }
  return(parm)
}

# Note: 
# - Need to update the reference for the parameters
# - Still needs to get the ED from the model.. need to see how...
