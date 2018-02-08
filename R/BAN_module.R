#' Predicts and/or projects stand Basal Area for Nothofagus (BAN) from stand level parameters
#'
#' \code{BANmodule} Evaluate stand level input variables to predict and/or project basal area for Nothofagus
#' Projections are based in 1 year increments.
#'
#' @param EDOM0 and EDOM1 Dominant age (years) of the stand at time 0 and at time 1
#' @param SI Site Index
#' @param NHA0, NHA1 Number of trees (trees/ha) of the stand at time 0 and at time 1
#' @param PBAN0 Proportion of Basal area (m2/ha) of Nothofagus of the stand at time 0 and at time 1
#' @param projection if TRUE projection from BA0 is executed for a 1 year increment.
#'
#' @return Basal area for Nothogafus (BAN, m2/ha) for the current age (for prediction)
#' or at AD0+1 (for projection)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @examples
#' # Example 1: Predicts Basal Area
#' BAest<-BANmodule(AD0=19, SI=14, NHA0=2140, PBAN0 = 0.91, projection=FALSE)
#' BAest$BAN0
#'
#' # Example 2: Projects Basal Area
#' BAest<-BANmodule(BAN0 = 36.5, AD0=19,  SI=14, NHA0=2730, NHA1=2650, PBAN0 = 0.91 , PBAN1 = 0.91, projection=TRUE)
#' BAest$BAN1

BANmodule <- function(BAN0=NA, AD0=NA, SI=NA, NHA0=NA, NHA1=NA, PBAN0 = NA, PBAN1=NA, projection=FALSE){
 # lm2 <- lm(log(AB_NOTH) ~ log.EDOM + log.IS + log.NHA + log.PBA_NOTH, data = PRODAL)

  # Model 1 (linear): BAN = exp(b0) * EDOM^b1 * IS^b2 * NHA^b3 * PBAN^b4
  bm<-c(-6.85382, 1.28466, 0.70700, 0.55894, 1.51427) # b0,b1,b2,b3,b4

  #### Prediction
  if (!projection){
    BAN0<-exp(bm[1])*(AD0^bm[2])*(SI^bm[3])*(NHA0^bm[4]*(PBAN0^bm[5]))
    BAN1<-NA
  }

  #### Projection
  if (projection){
    BAN1<-BAN0*exp(bm[2]*log((AD0+1)/AD0) + bm[3]*log(SI/SI) + bm[4]*log(NHA1/NHA0) + bm[5]*log(PBAN1/PBAN0))
  }

  return(list(BAN0=BAN0,BAN1=BAN1))
}

# Note: BA1 model needs to incorporate other derivatives in the future
