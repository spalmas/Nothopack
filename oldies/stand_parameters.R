#' Calculates stand-level parameter based on individual tree data
#'
#' \code{stand_parameters} Calculates all relevant stand-level parameters from an inventory plot for
#' each of the SPECIES and for all together (1: Rauli, 2: Roble, 3:Coigue, 4:Others, 0:All).
#' The input table should contain the colums of SPECIES (1: Rauli, 2: Roble, 3:Coigue, 4:Others)
#' together with their diameter at breast height (DBH, cm), and total tree height (HT, m).
#' There should not be missing information in the table. Plot area must be also provided.
#'
#' @param plotdata: data frame with plot data including the columns: SPECIES (1, 2, 3, 4), DBH (cm) and HT (m).
#' @param area: Area of plot (m2).
#'
#' @return A data frame with the columns: SPECIES (1: Rauli, 2: Roble, 3:Coigue, 4:Others, 0:All),
#' N (number of trees per ha), BA (basal area, m2), QD (quadratic diameter, cm). Also, the parameters
#' DOM.SP (dominant specie), HD (dominant height, HD), PropBAN (proportion of basal area of Nothofagus),
#' and PropNN (proportion of number of trees of Nothofagus).
#'
#' @examples
#' simplot<-stand_randomizer()
#' head(simplot)
#' stand_parameters(plotdata=simplot, area=1000)

stand_parameters <- function(plotdata, area = NA){

  if (is.na(area) ){ stop('Plot area must be provided') }
  CF <- 10000 / area  # Correction factor

  # Number of trees by SPECIES and total
  N1 <- CF * sum(plotdata$SPECIES == 1, na.rm = TRUE)    # Rauli
  N2 <- CF * sum(plotdata$SPECIES == 2, na.rm = TRUE)    # Roble
  N3 <- CF * sum(plotdata$SPECIES == 3, na.rm = TRUE)    # Coigue
  N99 <- CF * sum(plotdata$SPECIES == 4, na.rm = TRUE)    # Others
  N0 <- N1 + N2 + N3 + N99

  # Basal area by SPECIES and total
  plotdata$baind <- as.numeric(pi * (plotdata$DBH/2)^2 / 10000 )  # Units: m2
  BA1 <- CF * sum(plotdata[(plotdata$SPECIES == 1),4], na.rm = TRUE)
  BA2 <- CF * sum(plotdata[(plotdata$SPECIES == 2),4], na.rm = TRUE)
  BA3 <- CF * sum(plotdata[(plotdata$SPECIES == 3),4], na.rm = TRUE)
  BA99 <- CF * sum(plotdata[(plotdata$SPECIES == 4),4], na.rm = TRUE)
  BA0 <- BA1 + BA2 + BA3 + BA99

  # Quadratic diameters by SPECIES and total
  QD1<-get_stand(BA=BA1,N=N1)
  QD2<-get_stand(BA=BA2,N=N2)
  QD3<-get_stand(BA=BA3,N=N3)
  QD99<-get_stand(BA=BA99,N=N99)
  QD0<-get_stand(BA=BA0,N=N0)

  # Dominant Height - 100 trees with largest DBH
  # (this is for any of the SPECIES, not only dominant sp)
  N.HD <- 100/CF   # Number of trees to consider for HD
  HT.HD <- plotdata[order(plotdata$DBH,decreasing=TRUE),3]
  HDcalc <- matrix(data=0,nrow=nrow(plotdata),ncol=3)  # As long as trees in stand
  nt <- 0
  while (nt <= N.HD) {
    HDcalc[nt+1,1] <- HT.HD[nt+1]
    HDcalc[nt+1,2] <- CF
    nt <- nt + 1
  }
  if (sum(HDcalc[,2])>100) {
    HDcalc[nt,2] <- 100 - (sum(HDcalc[,2]) - CF)
  }
  HDcalc[,3] <- HDcalc[,1]*HDcalc[,2]
  HD <- sum(HDcalc[,3])/100

  N <- c(N1,N2,N3,N99,N0)
  BA <- c(BA1,BA2,BA3,BA99,BA0)
  QD <- c(QD1,QD2,QD3,QD99,QD0)

  PBAN <- sum(BA[1:3])/BA0
  PNHAN <- sum(N[1:3])/N0
  DOM.SP<-get_domsp(BA=BA)


  # Elements to return: vectors of SPECIES,N,BA,QD, and HD, DOM.SP, PropBAN, PropNN
  v1 <- c((1:4),0)
  v2 <- round(N,6)
  v3 <- round(BA,6)
  v4 <- round(QD,6)
  sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
  names(sdmatrix) <- c('SPECIES','N','BA','QD')

  return(list(sd=sdmatrix, DOM.SP=DOM.SP, HD=HD, PBAN=PBAN, PNHAN=PNHAN))
}

#       - Change Otras DOM.SP to 9 to stop the process.
#       - What to do with Mixed stand...
