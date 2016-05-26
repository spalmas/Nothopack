#' Stand simulator path
#'
#' \code{Nmodule} Simulates growth, mortality and recruitment of a given stand
#'
#' @param stand the stand to be simulated
#' @param zone zone of the stand
#' @param age initial age of the stand
#' @param sy simuation years
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' stand <- stand_randomizer()
#' simulator(stand, zone = 1, age = 15)

simulator <- function(stand, zone, EDOM0, area = 10000, sy = 30, ...){

  prodal <- stand_parameters(stand, area = area)
  HDOM0 <- prodal$HD
  N0 <- prodal$NHA
  AB0 <- prodal$BA
  DC0<-remaining_calc_A(AB=AB0,NHA=N0)
  SPDOM <- prodal$SPDOM
  IS<-remaining_calc_B(dom_sp = SPDOM, ZONE = ZONE, HD = HDOM0, E = EDOM0)

  y <- 0   #initial year

  #create a table to store results
  results <- data.frame (y = y, N0 = N0, AB0 = AB0, DC0 = DC0)

  for (y in 1:sy){
    N1<-Nmodule(N0=N0,QD0=12.43,model=1)
    DC1<-remaining_calc_A(AB = AB0, NHA=N1)
    AB1<-BAmodule(EDOM0=EDOM0,HDOM0=HDOM0,N0=N1,BA0=AB0,model=1,projection=TRUE)$BA1

    results <- rbind(results, c(y, N1, AB1, DC0))

    #Variable replacement
    N0 <- N1
    DC0 <- DC1
    AB0 <- AB1
  }



  return(results)
}

