#' Distributes stand level variable (N, BA, VOL) from total stand to each specie/cohort
#'
#' \code{get_props} Distributes simulated stand level variables basal area (BA, m2), number of trees (N, trees/ha)
#' and total volume (VOL, m3/ha) to each of the species/cohort based on the initial provided proportion.
#'
#' @param BA Basal area (m2) (all species/cohorts)
#' @param N Number of trees (trees/ha) (all species/cohorts)
#' @param VOL Total volume without bark (m3/ha) (all species/cohorts)
#' @param PBA0 vector of basal areas (m2/ha) for each of the species/cohorts 
#' (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param PN0 vector of basal areas (m2/ha) for each of the species/cohorts 
#' (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param PVOL0 vector of basal areas (m2/ha) for each of the species/cohorts 
#' (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' 
#' @return Three vectors with the updated and distributed stand level parameters BA, N and VOL
#' for each of the species/cohorts.
#'
#' @examples
#' #Example
#' get_props(BA=54.76,N=1259,VOL=642.83,
#'           PBA0=c(0.00,0.15,0.76,0.09),PN0=c(0.00,0.15,0.68,0.17),PVOL0=c(0.00,0.18,0.80,0.02)) 

get_props <- function(BA=NA, N=NA, VOL=NA, PBA0=rep(NA,4), PN0=rep(NA,4), PVOL0=rep(NA,4)) {

  if (sum(PBA0) != 1 ){  stop('Your proportions of basal area do not sum to one.') }
  if (sum(PN0) != 1 ){   stop('Your proportions of number of trees do not sum to one.') }
  if (sum(PVOL0) != 1 ){ stop('Your proportions of volume not sum to one.') }
  
  PBAF <- PBA0   # This should be a regression model that predicts
  PNF <- PN0   # This should be a regression model that predicts
  PVOLF <- PVOL0   # This should be a regression model that predicts
  
  BAF <- BA*PBAF
  NF <- N*PNF
  VOLF <- VOL*PVOLF
  
  distr.table <- data.frame(cbind(c(1,2,3,4),BAF,NF,VOLF))
  colnames(distr.table)<-c('Specie','BA','N','VOL')
  rownames(distr.table)<-c('Rauli','Roble','Coigue','Others')
  return(distr.table)
}


# Note - Warning that porportions do not sum to 1