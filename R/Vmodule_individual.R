#' Predicts volume for individual trees
#'
#' \code{Vmodule_individual} Evaluate tree level input and site information to predict the total volume.
#' It is based in the Estimacion de los Volumenes por Clase y Producto found in
#' in INFORME SIMULADOR.pdf.
#'
#' @param BA Basal Area
#' @param HD Dominant Height
#' @param PNHAN Proportion of nothofagus trees number
#'
#' @return Total volume without bark (m3/ha) for the current BA, HD and PNHAN
#'
#' @examples
#' # Example 1: Predicts volume with PNHAN
#' Vmodule(BA = 50, HD = 14, PNHAN = .7)
#'
#' # Example 2: Predicts volume without PNHAN
#' Vmodule(BA = 45, HD = 18)

Vmodule <- function(BA = NA, HD = NA, PNHAN = NA){
  if (!is.na(PNHAN)){
    vol <- 0.34690724*(BA^0.99378516 * HD^0.93053601 * PNHAN^0.04637122)
  } else {
    vol <- 0.43320630*(BA^0.97944091 * HD^0.93962528)
  }
  return(vol)
}
