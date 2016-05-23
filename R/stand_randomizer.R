#' Simulated stand randomizer
#'
#' It returns a randomized stand table with NA, ND, NO and XX species.
#'
#' @return A table with tree information
#'
#' @examples
#' stand_randomizer()

stand_randomizer <- function(){
  n.trees <- rpois(n = 1, lambda = 70)     #Random number of trees
  species.options <- c('NA', 'ND', 'NO', 'XX') #options of species
  SPECIES <- sample(species.options, size = n.trees, replace = TRUE)  #A list of n.trees species
  DBH <- 15 *rexp(n = n.trees)  #random diameter list
  HEIGHT <- exp(0.93687 + 0.55204*log(DBH))
  return(data.frame(SPECIES, DBH, HEIGHT))
}
