#' Predicts and/or projects stand Basal Area (BA) from stand level parameters
#'
#' \code{BAmodule} Evaluate stand level input variables to predict and/or project basal area.
#' Projections are based in year increments.
#'
#' @param EDOM0 Dominant age (years) of the stand at current time.
#' @param HDOM Dominant height (m) of the stand at current time.
#' @param N0 Number of trees (trees/ha) of the stand at current time.
#' @param BA0 Basal area (m2/ha) of the stand at curernt time (required for projections).
#' @param model available fitted model to use for estimation.
#' @param projection if TRUE projection from BA0 is executed for a 1 year increment.
#'
#' @return Basal area (BA0, m2/ha) for the current age (for prediction)
#' or at EDOM0+1 (for projection)
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' # Example 1: Predicts Basal Area
#' BAest<-BAmodule(EDOM0=20,HDOM0=17.20,N0=2730,BA0=NA,model=2,projection=FALSE)
#' BAest$BA0
#'
#' # Example 2: Projects Basal Area
#' (BAest<-BAmodule(EDOM0=20,HDOM0=17.20,N0=2730,BA0=33.11,model=1,projection=TRUE)$BA1)

BAmodule <- function(EDOM0=NA,HDOM0=NA,N0=NA,BA0=NA,model=1,projection=FALSE){

  # Model 1 (non-linear): BA = exp(b0)*EDOM^b1*HDOM^b2*NHA^b3
  bm1<-c(-3.36953,0.45753,0.75578,0.41176) # b0,b1,b2,b3
  # Model 2 (linear): log(AB) = b0 + b1*log.EDOM + b2*log.HDOM + b3*log.NHA
  bm2<-c(-3.90894,0.43824,0.87593,0.43949) # b0,b1,b2,b3
  if (model==1){
    bm<-bm1
  }
  if (model==2){
    bm<-bm2
  }

  if (projection==FALSE){
    # Prediction
    BA0<-exp(bm[1])*(EDOM0^bm[2])*(HDOM0^bm[3])*(N0^bm[4])
    BA1<-NA
  }
  if (projection==TRUE){
    # Projection
    BA1<-BA0*(1+bm[2]/EDOM0)
  }
  return(list(BA0=BA0,BA1=BA1))
}

