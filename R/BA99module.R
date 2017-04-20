#' Predicts and/or projects stand Basal Area for Other Species (BAN) from stand level parameters
#'
#' \code{BA99module} Evaluate stand level input variables to predict and/or project basal area for Other Species
#' Projections are based in 1 year increments.
#'
#' @param EDOM0 and EDOM1 Dominant age (years) of the stand at time 0 and at time 1
#' @param PNHAN0, PNHAN1 Proportion of Number of trees of Nothofagus (trees/ha) of the stand at time 0 and at time 1
#' @param PBAN0 Proportion of Basal area (m2/ha) of Nothofagus of the stand at time 0 and at time 1
#' @param projection if TRUE projection from BA0 is executed for a 1 year increment.
#'
#' @return Basal area for Other Species (BA99, m2/ha) for the current age (for prediction)
#' or at AD0+1 (for projection)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#' @examples
#' # Example 1: Predicts Basal Area
#' BA99est<-BA99module(EDOM0=19, PNHAN0=.8, PBAN0 = 0.91, projection=FALSE)
#' BA99est$BA990
#'
#' # Example 2: Projects Basal Area
#' BA99est<-BA99module(BA990 = 3.2, EDOM0=19, PNHAN0=0.81, PNHAN1=0.81, PBAN0 = 0.91 , PBAN1 = 0.91, projection=TRUE)
#' BA99est$BA991

BA99module <- function(BA990=NA, AD0=NA, PNHAN0=NA, PNHAN1=NA, PBAN0 = NA, PBAN1=NA, projection=FALSE){

  # Model 1 (linear): BA99 = exp(b0)*EDOM^b1*PNHAN^b2*PBAN^b3 - 10
  bm<-c(2.04068, 0.07997, -0.13080, -2.22934) # b0,b1,b2,b3

  #### Prediction  #CHECK EQUATION
  if (!projection){
    BA990<-exp(bm[1])*(AD0^bm[2])*(PNHAN0^bm[3])*(PBAN0^bm[4]) - 10
    BA991<-NA
  }

  #### Projection
  if (projection){
    BA991<-BA990*exp(bm[2]*log((AD0+1)/AD0)+bm[3]*log(PNHAN1/PNHAN0)+bm[4]*log(PBAN1/PBAN0))
  }

  return(list(BA990=BA990,BA991=BA991))
}
