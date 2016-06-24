#' Generates a random plot based on some conditions
#'
#' \code{stand_conditional_randomizer} Generates a table with individual tree information (species, DBH and HT)
#' to be used in examples and checks. The randomizer takes into account the following variables:
#'
#' @param zone
#' @param dom_sp
#'
#' The number of trees correspons to a Poisson (Poisson(30)) distribution.
#' Species are decided according to probabilities (with p1=0.8, p2=0.1, p3=0.05, p4=0.05).
#' DBH values are generated based on an exponential (Exp(n)) distribution.
#' HT values are obtained based on a simple HT-DBH model with added residuals based on
#' a normal (N(0,5)) distribution.
#'
#' @return A randomized stand table with Species (1:Rauli, 2:Roble, 3:Coigue and 4: Others),
#' DBH (cm) and HT (m).
#'
#' @examples
#' (newplot<-stand_randomizer())
#' plot(newplot[,2:3])  # Plotting HT-DBH data

stand_randomizer <- function(zone = 1, dom_sp = 1, AD=NA, HD=NA, SI=NA, BA=NA, N=NA, QD=NA){

  get_stand_parm <- get_stand(BA = BA, N = N, QD = QD)
  get_site_parm <- get_site(dom_sp = dom_sp, zone = zone, AD = AD, HD = HD, SI = SI)

  if(is.na(AD)){
    AD <- get_site_parm
  } else if(is.na(HD)){
    HD <- get_site_parm
  } else if(is.na(SI)){
    IS <- get_site_parm
  }

  if(is.na(BA)){
    BA <- get_stand_parm
  } else if(is.na(N)){
    N <- get_stand_parm
  } else if(is.na(QD)){
    QD <- get_stand_parm
  }


  #dominant species
  if (dom_sp == 1){
    prob <- c(0.80, 0.10, 0.05, 0.05)
  } else if (dom_sp == 2)


  n.trees <- rpois(n=1, lambda=30)     # Random number of trees based on a Poisson
  species.options <- c(1, 2, 3, 4)     # Options of species

  Species <- sample(species.options, size=n.trees, replace=TRUE,
                    prob=c(0.80, 0.10, 0.05, 0.05))  # A list of n.trees species
  DBH <- round(15*rexp(n=n.trees),2)   # Random DBH values based on exponential dist
  HT <- round(exp(0.93687 + 0.55204*log(DBH)) + rnorm(n.trees, mean=0, sd=sqrt(5)),2)

  return(data.frame(Species, DBH, HT))
}

# Note - We might need to make this more flexible in the future
#      - If we want to put it into the library, it needs better distirbutions
#      - Also, probably link HT with parametric_height plus some random error
