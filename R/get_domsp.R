#' Determines the dominant specie of a stand
#'
#' \code{get_domsp} Determines the dominant Nothofagus specie of a given stand
#' based on stand level parameters.
#'
#' @param BA Vector of basal area (m2/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065, Chile
#'
#' @return DOM.SP The dominant specie (1: Rauli, 2: Roble, 3: Coigue, 4:Mixed)
#'
#' @examples
#' # Example
#' BA<-c(36.5,12.8,1.6,2.4)
#' (DOM.SP<-get_domsp(BA))

get_domsp <- function(BA=NA){
  
  # Proportion of SPECIES by basal area
  BA0 <- sum(BA)
  PBA1 <- BA[1]/BA0   # Rauli
  PBA2 <- BA[2]/BA0   # Roble
  PBA3 <- BA[3]/BA0   # Coigue
  PBA99 <- BA[4]/BA0   # Others  
  PBAN <- sum(BA[1:3])/BA0   # Proportion BA for all Nothofagus

  # Obtaining dominant SPECIES.
  if (PBAN < 0.6 ){    # If BA Nothodagus represent less than 60% of the stand
    DOM.SP <- 99  # Others besides Nothofagus
  } else {
    if (PBA1 >= 0.7){
      DOM.SP <- 1  # Rauli
    } else if (PBA2 >= 0.7){
      DOM.SP <- 2  # Roble
    } else if (PBA3 >= 0.7){
      DOM.SP <- 3  # Coigue
    } else {
      DOM.SP <- 4  # Mixed Nothofagus
    }
  }
  
  return(DOM.SP)
}


