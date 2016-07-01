#' Adds young trees to the stand according to conditions
#'
#' \code{RECRUITmodule} For now it adds a random number of young trees.
#' The species is selected from a distribution and from small sizes.
#'  In the future it should estimate the 'space' for new trees. (high competition, low number of new trees)
#'
#' @param N0 Number of trees (trees/ha) of the stand at current time.
#'
#' @return An updated stand with the recruits added in the list
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @examples


RECRUITmodule <- function(vBA=NA, vN=NA, HD=NA){
  #Estimates the diametric distribution of the stand
  Dd<-diam_distr(vBA=vBA, vN=vN, HD=HD)

  #Estimates the probabilitt based on the current number of trees per species
  #Maybe it should be changed to some other function. Maybe based on dominant species, and density.
  #This is because some species may be recruited differently on different conditions
  #(e.g. rauli may stand shading better so it will be recruited better at higher densities)
  prob <- vN/sum(vN, na.rm = TRUE) #probabilities of recruitmentE

  Nrecruit <- 3 #should be a function of BA, N and HD

  #recruited trees species
  Species <- table(factor(sample(x = c(1,2,3,4), size=Nrecruit, replace=TRUE, prob=prob),  # A list of n.trees species
                    levels = c(1,2,3,4)))

  #Updating the diametric distributinb
  Dd[1,'N.sp1'] <- Dd[1,'N.sp1'] + Species[1]
  Dd[1,'N.sp2'] <- Dd[1,'N.sp2'] + Species[2]
  Dd[1,'N.sp3'] <- Dd[1,'N.sp3'] + Species[3]
  Dd[1,'N.sp4'] <- Dd[1,'N.sp4'] + Species[4]

  return(Dd)
}

