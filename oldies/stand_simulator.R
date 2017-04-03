#' Simulates whole-stand for the complete stand
#'
#' \code{stand_simulator} Simulates plot level growth, mortality (and recruitment) of a given stand
#' requiring stand-level parameters that combine all species. Simulations are done using stand-level
#' models starting from intial age (AD0) until final age (ADF) in increments of 1 year.
#'
#' @param dom_sp Dominant specie (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param zone Growth zone of the corresponding stand
#' @param HD0 dominant height (m) of current stand ata age AD0
#' @param AD0 initial dominant age (year) to start simulations
#' @param BA0 basal area (m2) of all tree species of current stand at age AD0
#' @param N0 number of trees (trees/ha) of all tree species of current stand at age AD0
#' @param ADF final dominant age (year) to finish simulations
#' @param Nmodel Number of fitted model for N estimation (1:Original Reineke, 2:New Reineke)
#' @param BAmodel Number of fitted model for BA to use for estimation (1:non-linear fit, 2:linear fit).
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' #Example 1. Starting from known stand-level data
#' BAest<-BAmodule(AD0=20, HD0=17.20, N0=2730, model=1, projection=FALSE)
#' sims <- stand_simulator(dom_sp=1, zone=1, AD0=20, ADF=80, HD0=12.20, BA0=BAest$BA0, N0=2730, Nmodel=1, BAmodel=1, PropNN=0.85)
#' sims
#' plot_results(sims)
#' 
#' #Example 2. Starting from (simulated) plot data
#' plotnew <- stand_randomizer()
#' head(plotnew)
#' prodal<-stand_parameters(plotdata=plotnew, area=500)
#' (sims<-stand_simulator(dom_sp=prodal$dom.sp, zone=1, AD0=44, ADF=150,
#'                 HD0=prodal$HD, BA0=prodal$sd[5,3], N0=prodal$sd[5,2],
#'                 Nmodel=2, BAmodel=2, PropNN=prodal$PropNN))
#' plot(sims$Age,sims$VOL,type='l',col=3,
#'      xlab='Dominant Age (years)', ylab='Total Volume without bark (m3/ha)')
#' plot_results(sims)

stand_simulator <- function(dom_sp=NA, zone=NA, HD0=NA, AD0=NA, BA0=NA, N0=NA, ADF=80, Nmodel=1, BAmodel=1, PropNN=NA){

  if (dom_sp == 9){
    stop('The stand is not dominated by Nothofagus and we dont have enough information for this simulation.')
  }

  # Completing stand-level information
  SI <- get_site(dom_sp=dom_sp, zone=zone, HD=HD0, AD=AD0)
  QD0 <- get_stand(BA=BA0, N=N0)
  VOL0 <- Vmodule(BA=BA0, HD=HD0, PropNN=PropNN)

  # Create a table to store results
  results <- data.frame(Age=AD0, N=N0, BA=BA0, QD=QD0, HD=HD0, SI=SI, VOL=VOL0)

  for (y in (AD0+1):ADF){

    N1 <- Nmodule(N0=N0, QD0=QD0, model=Nmodel)
    HD1 <- get_site(dom_sp=dom_sp, zone=zone, SI=SI, AD=y)
    BA1 <- BAmodule(AD0=AD0, HD0=HD0, N0=N0, BA0=BA0, HD1=HD1, N1=N1, model=BAmodel, projection=TRUE)$BA1
    QD1 <- get_stand(BA=BA1, N=N1)
    VOL1 <- Vmodule(BA=BA1, HD=HD1, PropNN=PropNN)  # Note that PropNN stays fixed!

    results <- rbind(results, c(y, N1, BA1, QD1, HD1, SI, VOL1))  # in the same order as dataframe above!

    # Variable replacement
    N0 <- N1
    QD0 <- QD1
    BA0 <- BA1
    HD0 <- HD1
    VOL0 <- VOL1

  }

  return(results)
}

# Note
# - Need to make sure when AD0=ADF there is only one year of output
# - We have prediction and projection.
# - It assumes you provide: HD-AD, and BA-N
# - Need to define if we add additional stand level parameters
# - Also, we could add plotting if desired
# - There is a strong assumption that PropNN and PropBA stays fixed
# - The plot for volume increments to much (it is unrealistic, probably projection BA is wrong!)
