#' Calculates elements from taper equation models
#'
#' \code{diam_distr} Reads input of tree information (Specie, DBH, HT), stand (dom_sp, zone),
#' and specification of parameters (di, hi) and provides the other element based on a selection of
#' different fitted taper equation models.
#
#' @param dom_sp Dominant specie (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param ZONE Growth zone of the corresponding stand
#' @param DBH Diameter at breast height (cm)
#' @param HT Total tree height (m)
#' @param di Stem diameter (cm) at given stem height hi (m)
#' @param hi Stem height (m) at given stem diameter (cm)
#' @param d Vector of stem diameters (cm)
#' @param h Vector of stem heights (m) (starting in 0.01 until HT by 0.01)
#'
#' @references
#' Gezan, S.A. and Moreno, P. (2000). INFORME PROCEDIMIENTOS Y RESULTADOS MODELOS DE AHUSAMIENTO.
#' Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @return The missing component (di or hi) from the requested taper model. Or returns the complete
#' vectors of d and h
#'
#' @author
#' S.A. Gezan, P. Moreno and S. Palmas
#'
#' @examples
#' # Example 1: Unknown diameter inside bark stem diameter
#' get_taper(SPECIES=1, ZONE=2, DBH=12.1, HT=14.2, hi=4.3)$di
#'
#' # Example 2: Unknown stem height
#' tree<-get_taper(SPECIES=1, ZONE=2, DBH=12.1, HT=14.2, di=9.8)
#' tree$hi
#' plot(tree$h,tree$d)

get_taper <- function(SPECIES = NA, ZONE='Todas', DBH=NA, HT=NA, di=NA, hi=NA){
  #Get parameters for that tree
  taper_params_tree <- taper_params %>% filter(SPECIES. == SPECIES & ZONE. == ZONE)
  #This SPECIES. is with a dot. While the rest is without . is this confusing or can it cause errors?

  #taper_params_tree <- taper_params %>% filter(SPECIES == 2 & ZONE == 3)


  #Assigning to variables. It can be done with taper_params_tree$b0 always, but it is too long
  b0<-taper_params_tree$b0
  b1<-taper_params_tree$b1
  b2<-taper_params_tree$b2
  b3<-taper_params_tree$b3
  b4<-taper_params_tree$b4
  b5<-taper_params_tree$b5
  b6<-taper_params_tree$b6
  b7<-taper_params_tree$b7

  # Get the complete tree profile (in increments of 1 cm)
  h <- seq(0.01,HT,by=0.01)

  if(taper_params_tree$M == 4){
    x <- (HT-h)/(HT-1.3)
    y <- b0*x^(1.5) + b1*(x^(1.5) - x^3)*DBH + b2*(x^(1.5) - x^3)*HT + b3*(x^(1.5) - x^32)*DBH*HT + b4*(x^(1.5) - x^32)*HT^0.5 + b5*(x^(1.5) - x^40)*HT^2
    d <- DBH * sqrt(y)
  } else if (taper_params_tree$M == 5){
    z <- h/HT
    x <- (1-z^0.5) / (1-0.2^0.5)
    y <- b0 + b1*log(DBH) + b2*DBH + b3*log(x)*(z^2) + b4*log(x)*log(z) + b5*log(x)*(z^0.5) + b6*log(x)*exp(z) + b7*log(x)*(DBH/HT)
    d <- exp(y)
  } else if (taper_params_tree$M == 6){
    z <- h/HT
    x <- (1-z^0.5) / (1-0.2^0.5)
    y <- b0 + b1*log(DBH) + b2*log(x)*(z^2) + b3*log(x)*log(z) + b4*log(x)*(DBH/HT)
    d <- exp(y)
  }

  #Assigning last diameter to 0
  d[length(d)] <- 0

  # Getting missing parameter
  if (is.na(di)) {              # If di is missing
    posit <- which(abs(h - hi) == min(abs(h - hi)))
    di <- d[posit]
  } else if (is.na(hi)) {       # If hi is missing
    posit <- which(abs(d - di) == min(abs(d - di)))
    hi <- h[posit]
  }
  return(list(di=di,hi=hi,d=d,h=h))
}

# Note: - Need to check that di and hi are not illogical from the DBH and HT of the tree
