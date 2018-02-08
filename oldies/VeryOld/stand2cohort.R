#' Whole-stand simulator for all cohorts in the stand
#' 
#' \code{Nmodule} Simulates growth, mortality and recruitment of a given stand
#' requiring stand paramters cthat combine all species.
#'
#' @param stand the stand to be simulated
#' @param zone zone of the stand
#' @param age initial age of the stand
#' @param sy simuation years
#' @param Nmodel Number of model for NHA estimation (1 is original function, 2 for new function)
#' @param BAmodel 1 for non linear and 2 for linear coefficient estimates
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' plotnew <- stand_randomizer()
#' prodal <- stand_parameters(plotnew, area = 1000)
#' SIM<-stand_simulator(dom_sp=1, zone=1, HD=14, AD=20, BA=12, N=770, ADF=40)
#' SIM
#' 

stand_simulator <- function(dom_sp=NA, zone=NA, HD0=NA, AD0=NA, BA0=NA, N0=NA, ADF=80, Nmodel=1, BAmodel=1){

  SI <- get_site(dom_sp=dom_sp, zone=zone, HD=HD0, E=AD0)
  QD0 <- get_stand(BA=BA0, N=N0)

  # Create a table to store results
  results <- data.frame (Age=AD0, N=N0, BA=BA0, QD=QD0, HD=HD0, SI=SI)
  
  for (y in (AD0+1):ADF){

    N1 <- Nmodule(N0=N0,QD0=QD0,model=Nmodel)
    BA1 <- BAmodule(ED0=AD0, HD0=HD0, N0=N0, BA0=BA0, model=BAmodel, projection=TRUE)$BA1
    QD1 <- get_stand(BA=BA1, N=N1)
    HD1 <- get_site(dom_sp=dom_sp, zone=zone, SI=SI, E=y)
  
    results <- rbind(results, c(y, N1, BA1, QD1, HD1, SI))  #should be in the same order as dataframe above!

    #Variable replacement
    N0 <- N1
    QD0 <- QD1
    BA0 <- BA1
    HD0 <- HD1
  }
  return(results)
}

