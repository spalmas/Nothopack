#' Projects number of trees (N) from stand level parameters to the next year
#'
#' \code{NHAmodule} Evaluates stand level input variables to project number of trees per hectare to the following year.
#' This is a mortality model, as it does not incorporate ingrowth.
#'
#' @param N0 Number of trees (trees/ha) of the stand at current time.
#' @param QD0 Quadratic Diameter (m2/ha) of the stand at current time.
#' @param model Number of fitted model for N estimation (1:Original Reineke, 2:New Reineke)
#'
#' @return Number of trees (N1, m2/ha) for the next year (i.e. ED+1)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @examples
#' (N1<-NHAmodule(N0=2730,QD0=12.43,model=1))
#' (N1<-NHAmodule(N0=2730,QD0=12.43,model=2))
#' (N1<-NHAmodule(N0=2730,QD0=12.43,model=3))
#' (N1<-NHAmodule(N0=2730,QD0=12.43,model=3))

NHAmodule <- function(NHA0=NA, QD0=NA, NHA_model=1, EDOM = 1, return.DQmax = FALSE){
  #NHA0=3500, EDOM = 4, return.QDmax = TRUE
  # Model: log(n_trees_ha2) = log(n_trees_ha1)*(1 - theta*Delta.ANHO*(dq1/dq_max_original))

  if (NHA_model == 1){

    slopes <- c(11.6167, 11.3770, 11.7630, 11.6167) #From GOA2007 Rauli, Roble, Coigue, Mixto
    slope <- slopes[EDOM]
    theta <- 0.003596 # Using original Reineke function and esimated theta
    DQmax <- exp((log(NHA0) - slope) / -1.4112)

  } else if (NHA_model == 2) {    #Modelo no muy revisado and only with one species

    theta <- 0.0056560  # Using new Reineke function and estimated theta
    DQmax <- exp((log(NHA0) - 13.500416)/-1.990455)

  }

  NHA1 <- exp(log(NHA0)*(1-theta*(QD0/DQmax)))
  #N1<-exp(log(N0)*(1-theta*(QD0/QDmax)))*1.02  # A very rought fix!

  if(return.DQmax){
    return(DQmax)
  } else {
    return(NHA1)
  }
}

# Note
# - Need to update help for different models.
