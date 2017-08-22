#' Predicts stand-level volume based on stand-level parameters
#'
#' \code{Vmodule} Reads stand-level input variables (BA, HD and PropNN) to predict stand-level total
#' volume without bark.
#'
#' @param BA Basal Area (m2/ha) of the current stand
#' @param HD Dominant Height (m) of the current stand
#' @param PropNN Proportion of trees in the stand that are Nothofagus. This parameter is optional
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @return Total volume without bark (m3/ha) for the current conditions across all species
#'
#' @examples
#' # Example 1: Predicts volume with PNHAN
#' Vmodule(BA=45, HD=14.2, PNHAN=0.83)
#'
#' # Example 2: Predicts volume without PNHAN
#' Vmodule(BA=45, HD=14.2)

Vmodule <- function(BA=NA, HD=NA, PNHAN=NA){
  if (!is.na(PNHAN)){
    VOL <- 0.34690724*(BA^0.99378516)*(HD^0.93053601)*((100*PNHAN)^0.04637122)
  } else {
    VOL <- 0.43320630*(BA^0.97944091)*(HD^0.93962528)
  }
  return(VOL)
}

# Note - Need a trap in case BA or HD are missing.
#      - Should we extend this model to have Vmodelo=1 and Vmodelo=2???
