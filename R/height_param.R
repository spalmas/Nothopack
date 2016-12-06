#' Estimates total tree height in function of stand level parameters
#'
#' \code{parametric_height} Estimatess individual tree heights in function of whole-stand level parameters:
#' dominant height (HD, m), quadratic diameter (QD), and individual tree diameter at breast height (DBH, cm).
#' Basic model is:
#' HT = b0 + b1*HD + b2*QD^0.95 + b3*exp(-0.08*DBH) + b4*(HD^3)*exp(0.08*DBH) + b5*(QD^3)*exp(-0.08*DBH)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @param dom_sp Dominant species (1: Rauli, 2: Roble, 3: Coigue)
#' @param zone Growth zone (1, 2, 3, 4)
#' @param HD Dominant height (m).
#' @param QD Quadratic diameter (cm) of the stand.
#' @param DBH Diameter at breast height (cm) of tree.
#'
#' @return Individual total tree height (m)
#'
#' @examples
#' (HT<-height_param(dom_sp=2, zone=2, HD=15, QD=12, DBH=24))


height_param <- function(dom_sp, zone, HD=NA, QD=NA, DBH=NA){
  coef.list <- subset(hparam_coef, hparam_zone == zone & hparam_dom_sp_code == dom_sp,
                      select = c(hparam_b0, hparam_b1, hparam_b2, hparam_b3, hparam_b4, hparam_b5))

  hest <-(coef.list$hparam_b0 + coef.list$hparam_b1*HD + coef.list$hparam_b2*(QD^0.95)
  + coef.list$hparam_b2*exp(-0.08*DBH) + coef.list$hparam_b4*(HD^3)*exp(-0.08*DBH)
  + coef.list$hparam_b5*(QD^3)*exp(-0.08*DBH))

  return(hest)
}
