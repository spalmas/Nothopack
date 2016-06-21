#' Stand simulator path
#'
#' \code{Nmodule} Simulates growth, mortality and recruitment of a given stand
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
#' SIM<-simulator(prodal=prodal, zone=1, ED0=15, EDOM0=20)

simulator <- function(prodal, zone, ED0, area = 10000, sy = 30, Nmodel = 1, ABmodel = 1,  ...){

  HD0 <- prodal$HD
  N0 <- prodal$NHA
  AB0 <- prodal$BA
  DC0<-prodal$DQ
  Vol0<-Vmodule(BA=AB0, HD=HD0)

  IS<-get_site(dom_sp = prodal$SPDOM, zone = zone, HD = HD0, E = ED0)

  y <- 0   #initial year

  #create a table to store results
  results <- data.frame (y = y, N0 = N0, AB0 = AB0, DC0 = DC0, HD = HD0, VOL = Vol0 )
  for (y in 1:sy){
    N1<-Nmodule(N0=N0,QD0=DC0,model=Nmodel)
    AB1<-BAmodule(ED0=ED0,HD0=HD0,N0=N1,BA0=AB0,model=ABmodel,projection=TRUE)$BA1
    DC1<-get_stand(BA = AB1, N=N1)
    HD1<-HD0  #should be replaced with equations
    Vol1<-Vmodule(BA=AB1, HD=HD1)    #proportion false for now.

    results <- rbind(results, c(y, N1, AB1, DC1, HD1, Vol1))  #should be in the same order as dataframe above!

    #Variable replacement
    N0 <- N1
    DC0 <- DC1
    AB0 <- AB1
    HD <- HD1
  }



  return(results)
}

