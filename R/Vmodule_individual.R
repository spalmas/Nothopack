#' Predicts individual tree volume inside bark based on taper equations.
#'
#' \code{Vmodule_individual} Reads input of tree information (specie, DBH, HT), stand (dom_sp, zone), 
#' and of specification of restrictions (dmin, blength), and it provides the inside bark volume (m3)
#' for the given tree based on different fitted taper equation models. 
#'
#' @param dom_sp Dominant specie (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed) 
#' @param zone Growth zone of the corresponding stand
#' @param DBH diameter at breast height (cm)
#' @param HT total tree height (m)
#' @param dmin minimum stem diameter inside bark (cm) 
#' @param blength bole length (m) 
#' @param stump length of stump to discount (default 0.3 m) 
#' @param Tmodel Number of fitted taper model to use (1:M4, 2:M5, 3:M6)
#' 
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#' 
#' Gezan, S.A. and Moreno, P. (2000b). ????????????. 
#' Reporte Interno. Projecto FONDEF D97I1065. Chile
#'  
#' @return Tree volume without bark (m3) based on the specifications of restrictions provided
#'
#' @examples
#' # Example 1: Calculates tree volume for diameter limit of 5 cm (with stump of 0.3 m) 
#' Vmodule_individual(SPECIES=1, zone=1, DBH=22.1, HT=18.2, dmin=5)
#' 
#' # Example 2: Calculates tree volume for a bole length of 6 m (with a stump of 0.3 m)
#' Vmodule_individual(SPECIES=1, zone=1, DBH=22.1, HT=18.2, blength=6)
#' 
#' # Example 3: Two ways to calculate total tree volume (with a stump of 0.3 m)
#' Vmodule_individual(SPECIES=1, zone=1, DBH=22.1, HT=18.2, dmin=0)
#' Vmodule_individual(SPECIES=1, zone=1, DBH=22.1, HT=18.2, blength=18.2)
#' 
#' # Example 4: Calculates total tree volume without discounting for stump
#' Vmodule_individual(SPECIES=1, zone=1, DBH=22.1, HT=18.2, dmin=0, stump=0)

Vmodule_individual <- function(SPECIES=NA, zone=NA, DBH=NA, HT=NA, dmin=NA, blength=NA, stump=0.3){

  if (is.na(blength) & is.na(dmin)){
    stop('Minimum diameter or bole length need to be provided.')
  }
  if (!is.na(blength) & !is.na(dmin)){
    stop('Minimum diameter or bole length are both provided.')
  }  
  # dmin provided
  if (is.na(blength) & !is.na(dmin)){  
    blength<-get_taper(SPECIES=SPECIES, zone=zone, DBH=DBH, HT=HT, di=dmin)$hi
  }

  incr<-0.01  # default increment in h from get_taper
  tree.profile<-get_taper(SPECIES=SPECIES, zone=zone, DBH=DBH, HT=HT, hi=blength)
  d<-tree.profile$d
  h<-tree.profile$h
  ba0<-pi*((d/100)^2)/4
  ba1<-c(ba0[2:length(ba0)],0)
  vsection<-incr*(ba0+ba1)/2
  
  s.start<-which.min(abs(h-stump))
  s.fin<-which.min(abs(h-blength))
  vtree<-sum(vsection[s.start:s.fin])
  
  return(vtree)
}

# Note 
# - If we need a different product stump is the lower height and blength is the upper height
# - remember it is di without bark, hence at hi=1.3, di<>DBH.
