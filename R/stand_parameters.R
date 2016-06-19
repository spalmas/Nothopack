#' Calculates stand-level parameter based on individual tree data
#' 
#' \code{stand_parameters} Calculates all relevant stand-level parameters from an inventory plot for
#' each of the Species and for all together (1: Rauli, 2: Roble, 3:Coigue, 4:Others, 0:All).
#' The input table should contain the colums of Species (1: Rauli, 2: Roble, 3:Coigue, 4:Others)
#' together with their diameter at breast height (DBH, cm), and total tree height (HT, m). 
#' There should not be missing information in the table. Plot area must be also provided.
#' 
#' @param plotdata: data frame with plot data including the columns: Species (1, 2, 3, 4), DBH and HT.
#' @param area: Area of plot (m2).
#'
#' @return A data frame with the columns: Species (1: Rauli, 2: Roble, 3:Coigue, 4:Others, 0:All),
#' N (number of trees per ha), BA (basal area, m2), QD (quadratic diameter, cm). Also, the parameters
#' dom.sp (dominant specie), HD (dominant height, HD), PropBAN (proportion of basal area of Nothofagus),
#' and PropNN (proportion of number of trees of Nothofagus).
#'
#' @examples
#' simplot<-stand_randomizer()
#' head(simplot)
#' stand_parameters(plotdata=simplot, area=1000)

stand_parameters <- function(plotdata, area=0){

  if (area == 0 ){ stop('Plot area must be provided') }
  CF <- 10000 / area  # Correction factor

  # Number of trees by species and total 
  N1 <- CF * sum(plotdata$Species == 1, na.rm = TRUE)    # Rauli
  N2 <- CF * sum(plotdata$Species == 2, na.rm = TRUE)    # Roble
  N3 <- CF * sum(plotdata$Species == 3, na.rm = TRUE)    # Coigue
  N4 <- CF * sum(plotdata$Species == 4, na.rm = TRUE)    # Others
  N0 <- N1 + N2 + N3 + N4
  
  # Basal area by species and total
  plotdata$baind <- as.numeric(pi * (plotdata$DBH/2)^2 / 10000 )  # Units: m2
  BA1 <- CF * sum(plotdata[(plotdata$Species == 1),4], na.rm = TRUE) 
  BA2 <- CF * sum(plotdata[(plotdata$Species == 2),4], na.rm = TRUE) 
  BA3 <- CF * sum(plotdata[(plotdata$Species == 3),4], na.rm = TRUE) 
  BA4 <- CF * sum(plotdata[(plotdata$Species == 4),4], na.rm = TRUE) 
  BA0 <- BA1 + BA2 + BA3 + BA4

  # Quadratic diameters by species and total
  QD1<-get_stand(BA=BA1,N=N1)
  QD2<-get_stand(BA=BA2,N=N2)
  QD3<-get_stand(BA=BA3,N=N3)
  QD4<-get_stand(BA=BA4,N=N4)
  QD0<-get_stand(BA=BA0,N=N0)
  
  # Dominant Height - 100 trees with largest DBH
  # (this is for any of the species, not only dominant sp)
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
  
  # Proportion of species by basal area
  PropBA1 <- BA1/BA0   # Rauli
  PropBA2 <- BA2/BA0   # Roble
  PropBA3 <- BA3/BA0   # Coigue
  PropBA4 <- BA4/BA0   # Others
  
  PropBAN <- (BA1 + BA2 + BA3)/BA0   # Proportion BA for all Nothofagus
  PropNN <- (N1 + N2 + N3)/N0   # Proportion N  for all Nothofagus
  
  # Obtaining dominant species 
  if (PropBAN < 0.6 ){    # If BA Nothodagus represent less than 60% of the stand
    dom.sp <- 4  # Others besides Nothofagus
  } else {
    if (PropBA1 >= 0.7){
      dom.sp <- 1  # Rauli
    } else if (PropBA2 >= 0.7){
      dom.sp <- 2  # Roble
    } else if (PropBA3 >= 0.7){
      dom.sp <- 3  # Coigue
    } else {
      dom.sp <- 4  # Mixed Nothofagus
    }
  }

  # Elements to return: vectors of Species,N,BA,QD, and HD, dom.sp, PropBAN, PropNN
  v1 <- c((1:4),0)
  v2 <- round(c(N1,N2,N3,N4,N0),6)
  v3 <- round(c(BA1,BA2,BA3,BA4,BA0),6)
  v4 <- round(c(QD1,QD2,QD3,QD4,QD0),6)
  sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
  names(sdmatrix) <- c('Species','N','BA','QD')

  return(list(sd=sdmatrix, dom.sp=dom.sp, HD=HD, PropBAN=PropBAN, PropNN=PropNN))
}

# Note. - VERY VERY IMPORTANT: - DID SEBASTIAN CALCULATED DOM.SP accorting to BA or NHA???