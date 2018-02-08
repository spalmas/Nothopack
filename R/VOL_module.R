#' Predicts stand-level volume based on stand-level parameters
#'
#' \code{Vmodule} Reads stand-level input variables (BA, HD and PNHAN, PBAN) to predict stand-level total
#' volume without bark. It is recommended to use the model with BA, HD and PNHAN as it is more stable.
#'
#' @param BA Basal Area (m2/ha) of the current stand
#' @param HD Dominant Height (m) of the current stand
#' @param PNHAN Proportion of trees in the stand that are Nothofagus. This parameter is optional
#' @param PBAN Proportion of basal area in the stand that are Nothofagus. This parameter is optional
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#' (updated version with recalculated stand volumes)
#'
#' @return Total volume with (or without?) bark (m3/ha) for the current conditions across all species
#'
#' @examples
#' # Example 1: Predicts volume with PNHAN and PBAN
#' Vmodule(BA=32, HD=15.9, PNHAN=0.81, PBAN=0.90)
#'
#' # Example 2: Predicts volume with PNHAN
#' Vmodule(BA=32, HD=15.9, PNHAN=0.81)
#'
#' # Example 3: Predicts volume 
#' Vmodule(BA=32, HD=15.9)

Vmodule <- function(BA=NA, HD=NA, PNHAN=NA, PBAN=NA){
  
  # Updated version with recalculated stand volumes and Baskerville correction
  if (!is.na(PNHAN)) {
    if (!is.na(PBAN)) {
      VOL <- exp(-0.934+0.03576/2)*(BA^0.9100)*(HD^1.0949)*(PNHAN^-0.270)*(PBAN^0.2241)
    } else {      
      VOL <- exp(-0.820+0.03673/2)*(BA^0.9076)*(HD^1.0602)*(PNHAN^0.1197)
    }
  } else {
    VOL <- exp(-0.735+0.03891/2)*(BA^0.8662)*(HD^1.0661)
  }
  
  return(VOL)
}

# Note - Need a trap in case BA or HD are missing.
#      - Should we extend this model to have Vmodelo=1 and Vmodelo=2???
