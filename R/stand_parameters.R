#' Calculates stand-level parameter based on individual tree data (works with tree_simulator)
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
#' plotdata<- read.csv(file= 'data/Plot_example.csv')
#' stand_parameters1(plotdata=plotdata, area=500)
#'
stand_parameters1 <- function(plotdata=NA,area=area){
  CF <- 10000 / area  # Correction factor

  #Calculating invidual basal area
  plotdata$BAind <- plotdata$FT * as.numeric(pi * plotdata$DBH^2/40000 )  # Units: m2

  #Adding a factor to complete
  plotdata$SPECIES = factor(plotdata$SPECIES, levels=c(1:4,0))

  #table with N, BA and QD. Adds 0 if no species is found
  sd <- plotdata %>% group_by(SPECIES) %>%
    summarise(
      N = sum(FT, na.rm = TRUE),
      BA = sum(BAind, na.rm = TRUE),
      QD = get_stand(BA, N)
    ) %>%
    complete(SPECIES, fill = list(N = 0, BA = 0, QD = 0)) %>%
    as.data.frame()

  #Adding total values
  sd[5,2:4] <- c(sum(sd$N, na.rm = TRUE),
                 sum(sd$BA, na.rm = TRUE),
                 get_stand(BA = sum(sd$BA, na.rm = TRUE), N = sum(sd$N, na.rm = TRUE)))

  #proportion values
  PBAN <- sum(sd$BA[1:3])/sd$BA[5]
  PNHAN <- sum(sd$N[1:3])/sd$N[5]
  DOM.SP<-get_domsp(BA=sd$BA)

  # Dominant Height - 100 trees with largest DBH
  # (this is for any of the SPECIES, not only dominant sp)
  N.HD <- area/100   # Number of trees to consider for HD
  HT.HD <- sort(plotdata$DBH, decreasing = TRUE) #sorting the diameters
  FT <- rep(0,length(plotdata$DBH))
  FT[1:ceiling(N.HD)] <- CF
  FT[ceiling(N.HD)] <- 100 - (sum(FT)-CF)  #The remaining proportion
  #HD <- sum(HT.HD*FT)/100   #DBH times CF
  HD <- sum(HT.HD[1:ceiling(N.HD)]*FT[1:ceiling(N.HD)])/100   #DBH times CF

  return(list(sd=sd, DOM.SP=DOM.SP, HD=HD,PBAN=PBAN, PNHAN=PNHAN, BAind=plotdata$BAind))
}
