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
#' stand <- stand_randomizer()
#' simulator(stand, zone=1, age=15, EDOM0=20)

simulator <- function(stand, zone, ED0, area = 10000, sy = 30, Nmodel = 1, ABmodel = 1,  ...){

  prodal <- stand_parameters(stand, area = area)
  HD0 <- prodal$HD
  N0 <- prodal$NHA
  AB0 <- prodal$BA
  DC0<-prodal$DQ

  IS<-get_site(dom_sp = prodal$SPDOM, zone = zone, HD = HD0, E = ED0)

  y <- 0   #initial year

  #create a table to store results
  results <- data.frame (y = y, N0 = N0, AB0 = AB0, DC0 = DC0)
  for (y in 1:sy){
    N1<-Nmodule(N0=N0,QD0=DC0,model=Nmodel)
    AB1<-BAmodule(ED0=ED0,HD0=HD0,N0=N1,BA0=AB0,model=ABmodel,projection=TRUE)$BA1
    DC1<-get_stand(BA = AB1, N=N1)

    results <- rbind(results, c(y, N1, AB1, DC0))  #should be in the same order as dataframe above!

    #Variable replacement
    N0 <- N1
    DC0 <- DC1
    AB0 <- AB1
  }



  return(results)
}

