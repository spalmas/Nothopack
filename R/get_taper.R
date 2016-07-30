#' Calculates elements from taper equation models
#'
#' \code{diam_distr} Reads input of tree information (Specie, DBH, HT), stand (dom_sp, zone), 
#' and specification of parameters (di, hi) and provides de other elements based on a selection of
#' different fitted taper equation models. 
#
#' @param dom_sp Dominant specie (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed) 
#' @param zone Growth zone of the corresponding stand
#' @param DBH Diameter at breast height (cm)
#' @param HT Total tree height (m)
#' @param di Stem diameter (cm) at given stem height hi (m)
#' @param hi Stem height (m) at given stem diameter (cm)
#' @param Tmodel Number of fitted taper model to use (1:M4, 2:M5, 3:M6)
#' 
#' @references
#' Gezan, S.A. and Moreno, P. (2000b). ????????????. 
#' Reporte Interno. Projecto FONDEF D97I1065. Chile
#'  
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @return The missing component (di or hi) from the requested taper model 
#'
#' @author
#' S.A. Gezan, P. Moreno and S. Palmas
#'
#' @examples
#' # Example 1: Unknown diameter inside bark stem diameter 
#' get_taper(dom_sp=1, zone=1, DBH=12.1, HT=14.2, hi=4.3, Tmodel=1)
#' 
#' # Example 2: Unknown stem height 
#' get_taper(dom_sp=1, zone=1, DBH=12.1, HT=14.2, di=12.1, Tmodel=1)
#' 
#' dom_sp=1
#' zone=1
#' DBH=12.1
#' HT=14.2
#' hi=12.1
#' Tmodel=1

get_taper <- function(dom_sp=NA, zone=NA, DBH=NA, HT=NA, di=NA, hi=NA, Tmodel=1){

  # Rauli parameters, sp=1 (just to get started)
  dom_sp <- 1
  zone <- 1
  Tmodel <- 1
  b0 <- -0.19392205
  b1 <- 0.99342476
  b2 <- 0.58748585
  b3 <- -0.056864102
  b4 <- 0.20239942
  
  # Get the complete tree profile (in increments of 1 cm)
  h <- seq(0.01,HT,by=0.01)
  z <- h/HT
  x <- (1-z^0.5) / (1-0.2^0.5)
  y <- b0 + b1*log(DBH) + b2*log(x)*(z^2) + b3*log(x)*log(z) + b4*log(x)*(DBH/HT)
  d <- exp(y)
  d[length(d)] <- 0 
  #plot(h,d)
  
  # Getting missing parameter 
  if (is.na(di)) {              # If di is missing
    posit <- which(abs(h - hi) == min(abs(h - hi)))
    di <- d[posit]
  } else if (is.na(hi)) {       # If hi is missing
    posit <- which(abs(d - di) == min(abs(d - di)))
    hi <- h[posit]
  }  
  return(list(di=di,hi=hi))
}

# Note: - Need to check that di and hi are not illogical from the DBH and HT of the tree