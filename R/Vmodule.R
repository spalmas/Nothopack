#' Predicts volume from stand level parameters and from list
#'
#' \code{Vmodule} Evaluate stand level input variables to predict and/or project basal area.
#' Projections are based in year increments. The parameter PNHAN is optional and the volume can be estimated withouy ir.
#' It is based in INFORME SIMULADOR.pdf
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
