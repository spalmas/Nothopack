#' Predicts individual tree volume based for different products based on taper equations.
#'
#' \code{Vmodule_individual} Reads input of tree information (Specie, DBH, HT), stand (dom_sp, zone), 
#' and of specification of products (dmin, dmax, steml), and it provides the inside bark volume (cm3)
#' for the given tree based on different fitted taper equation models. 
#'
#' @param dom_sp Dominant specie (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed) 
#' @param zone Growth zone of the corresponding stand
#' @param DBH diameter at breast height (cm)
#' @param HT total tree height (m)
#' @param dmin minimum stem diameter to consider (cm)
#' @param dmax maximum stem diameter to consider (cm)
#' @param steml length of stem (m) (?????????????????)
#' @param Tmodel Number of fitted taper model to use (1:M4, 2:M5, 3:M6)
#' 
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#' 
#' Gezan, S.A. and Moreno, P. (2000b). ????????????. 
#' Reporte Interno. Projecto FONDEF D97I1065. Chile
#'  
#' @return Tree volume without bark (m3) based on the specifications provided
#'
#' @examples
#' # Example 1: Predicts total tree volume 
#' Vmodule_individual(dom_sp=1, zone=1, DBH=12.1, HT=14.2, Tmodel=1)
#'
#' # Example 2: Predicts tree volume from stump heigt (0.3 m) to a tree height of 3 m 
#' Vmodule_individual(dom_sp=1, zone=1, DBH=12.1, HT=14.2, Tmodel=1)
#'

Vmodule_individual <- function(dom_sp=NA, zone=NA, DBH=NA, HT=NA, dmin=NA, dmax=NA, steml=0){
  if (!is.na(PNHAN)){
    vol <- 0.34690724*(BA^0.99378516 * HD^0.93053601 * PNHAN^0.04637122)
  } else {
    vol <- 0.43320630*(BA^0.97944091 * HD^0.93962528)
  }
  return(vol)
}

# Note - Decide if we do length of stem, as we might have other equations...
# - remember it is di without bark, hence at hi=1.3, di<<>DBH.