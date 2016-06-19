#' Predicts and/or projects stand Basal Area (BA) from stand level parameters
#'
#' \code{BAmodule} Evaluate stand level input variables to predict and/or project basal area.
#' Projections are based in year increments.
#'
#' @param ED0 Dominant age (years) of the stand at current time.
#' @param HD0 Dominant height (m) of the stand at current time.
#' @param N0 Number of trees (trees/ha) of the stand at current time.
#' @param BA0 Basal area (m2/ha) of the stand at curernt time (required for projections).
#' @param model Number of fitted model to use for estimation (1:non-linear fit, 2:linear fit).
#' @param projection if TRUE projection from BA0 is executed for a 1 year increment.
#'
#' @return Basal area (BA0, m2/ha) for the current age (for prediction)
#' or at ED0+1 (for projection)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#' 
#' @examples
#' # Example 1: Predicts Basal Area
#' BAest<-BAmodule(ED0=20,HD0=17.20,N0=2730,model=1,projection=FALSE)
#' BAest$BA0
#'
#' # Example 2: Projects Basal Area
#' (BAest<-BAmodule(ED0=20,HD0=17.20,N0=2730,BA0=33.11,model=1,projection=TRUE)$BA1)
#' (BAest<-BAmodule(ED0=20,HD0=17.20,N0=2730,BA0=33.11,model=2,projection=TRUE)$BA1)

BAmodule <- function(ED0=NA, HD0=NA, N0=NA, BA0=NA, model=1, projection=FALSE){

  # Model 1 (non-linear): BA = exp(b0)*ED^b1*HD^b2*NHA^b3
  bm1<-c(-3.36953,0.45753,0.75578,0.41176) # b0,b1,b2,b3
  # Model 2 (linear): log(AB) = b0 + b1*log.ED + b2*log.HD + b3*log.NHA
  bm2<-c(-3.90894,0.43824,0.87593,0.43949) # b0,b1,b2,b3
  if (model==1){
    bm<-bm1
  }
  if (model==2){
    bm<-bm2
  }

  if (projection==FALSE){
    # Prediction
    BA0<-exp(bm[1])*(ED0^bm[2])*(HD0^bm[3])*(N0^bm[4])
    BA1<-NA
  }
  if (projection==TRUE){
    # Projection
    BA1<-BA0*(1+bm[2]/ED0)  # Needs to be adjusted for other derivatives
  }
  return(list(BA0=BA0,BA1=BA1))
}

# Note: BA1 model needs to incorporate other derivatives in the future