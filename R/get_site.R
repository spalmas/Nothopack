#' Calculates remaining stand level variable from the set: ED, HD, SI
#'
#' \code{get_site} When two of stand level variebles: dominat age (ED, year), dominant height (HD, m) and site index (SI, m)
#' are given, it returns the value of the remaining stand level parameter.
#' Note: The coefficients for the site index curves come from Gezan and Ortega (2001).
#'
#' @param dom_sp Dominant species (1: Rauli, 2: Roble, 3: Coigue)
#' @param zone Growth zone (1, 2, 3, 4)
#' @param ED Dominant age (year)
#' @param HD Dominant height (m)
#' @param SI Site index at reference dominant age of 18 (m)
#' @param ... additional arguments to be passed to the low level bisection function
#'
#' @references 
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para 
#' Renovales de Roble, Rauly y Coigue. Reporte Interno. Projecto FONDEF D97I1065, Chile
#' 
#' Gezan, S.A. and Moreno, P. (2000). CURVAS DE SITIO – ALTURA DOMINANTE PARA RENOVALES
#'  DE ROBLE, RAULÍ Y COIGUE. Reporte Interno. Projecto FONDEF D97I1065, Chile
#'
#' @return The missing stand level parameter
#'
#' @seealso \code{\link{hd_coef}}. For BA, QD and N see \code{\link{get_stand}}
#'
#' @examples
# ED<-get_site(dom_sp=1, zone=2, HD=14, SI=10)
# round(ED,0)
# HD<-get_site(dom_sp=1, zone=2, ED=25, SI=10)
# HD
# SI<-get_site(dom_sp=1, zone=2, ED=25, HD=14)
# SI
library (pracma)

get_site <- function(dom_sp, zone, ED=NA, HD=NA, SI=NA){
  coef.list <- subset(hd_coef, hd_coef_zone == zone & hd_coef_sp_code == dom_sp,
                      select = c(hd_coef_a, hd_coef_b0, hd_coef_b1) )

  if (sum(is.na(c(ED, HD, SI))) > 2 ){
    stop('There must be at least two stand parameters provided')
    
  } else if ( sum(is.na(c(ED, HD, SI))) == 0 ){
    warning('Why would you use this calculation? You have allready have the three variables')
    
  } else if (is.na(ED)) {               # If Initial age is missing
    
    # Definition of ED function for bisection method (should it be somewhere else?)
    ED.eq <- function(x){
      c <- coef.list$hd_coef_b0 + coef.list$hd_coef_b1*SI
      - HD + coef.list$hd_coef_a * (1-(1-(SI/coef.list$hd_coef_a)^c)^((x-2)/18) )^(1/c) 
    }
    # Bisection method
    parm <- tryCatch(pracma::bisect(ED.eq,
                                    a=2, b=100,
                                    maxiter=100)$root)

  } else if (is.na(HD)) {              # If Dominant height is missing
    c <- coef.list$hd_coef_b0 + coef.list$hd_coef_b1*SI
    parm <- coef.list$hd_coef_a * (1-(1-(SI/coef.list$hd_coef_a)^c)^((ED-2)/18))^(1/c)
    return(parm)

  } else if (is.na(SI)) {              # If Site Index is missing

    # Definition of SI function for bisection method (should it be somewhere else?)
    SI.eq <- function(x){
      c <- coef.list$hd_coef_b0 + coef.list$hd_coef_b1 * x
      - HD + coef.list$hd_coef_a * (1-(1-(x/coef.list$hd_coef_a)^c)^((ED-2)/18))^(1/c) 
    }
    # Bisection method
    parm <- tryCatch(pracma::bisect(SI.eq,
                            a=0, b=40,
                            maxiter=100)$root)
  }
  return(parm)
}

# Note: Model 
# HD = a [1 – {1 – (IS / a) c } ((E - 2) / 18)] 1/c
# c = b0 + b1 IS
