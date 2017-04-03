#' To obtain Stand and Individual variables for feeding the growth tree model in DBH
#'
#' @param ID Array of identification of each tree
#' @param area Array of Area of the plot
#' @param sp Array of sp of plot
#' @param DBH Array of DBHs
#' @param ZONA Array of zona
#'
#' @return 8 different predictors ID, DBH, ba (basal area individual, m2/ha), bac (basal area individual cohorte, m2/ha),
#' SPZONA (concatenation), N (number of trees per ha), BA (basal area, m2/ha), QD (quadratic diameter, cm), 
#' SDI (Stand Density Index), BAL (Basal Area Larges trees, m2/ha), BALc (Basal Area Larges trees, m2/ha), PSCAL (BAL/BA)
#'
#' @examples
#' ID<-c(1,2,3)
#' area<-c(25,25,25)
#' sp<-c(1,1,2)
#' DBH<-c(15,25,37)
#' ZONA<-c(1,1,1)
#' COV<-covariates(ID,area,sp,DBH,ZONA);COV


## Code for obtaining covariates from ingrowth DBH models
## Require plot with id tree, area, sp, DBH and zone
covariates<-function(ID,area,sp,DBH,ZONA){
  #require(dplyr)
  N <- length(ID)#Getting the number of trees in the plot
  area.plot <- unique(area)   #area of the measurement
  NHA <- N * 10000/area.plot #Getting the number of trees in the stand by hectare
  BA =  sum(pi * (DBH/2/100)^2, na.rm = TRUE) * 10000/area.plot#total basal area by hectare
  QD <-  (100*((4/pi)*(BA/NHA))^0.5)#total quadratic diameter,
  SDI<-NHA*((25.4/QD)^(-1.4112))#Stand density index
  SPZONA<-sp*10+ZONA
  ###BAL & BALc Calculation
  ba<-pi*((DBH)^2)/40000
  bac<-0
  for (fila in (1:(N))) {
    bac[fila] <- if (sp[fila] != 4) {ba[fila]}else {0}
  }
  Temp.data<-data.frame(ID,DBH,ba,bac,SPZONA,NHA,BA,QD,SDI)
  Temp.data1<-arrange(Temp.data, desc(ba))
  Acum=0;Acumc=0
  Temp.data1$BAL[1]=0
  Temp.data1$BALc[1]=0
  for (i in (2:nrow(Temp.data1))) {
    Acum <- Acum + (Temp.data1$ba[i-1]*10000/area.plot)
    Temp.data1$BAL[i] <- Acum
    if(Temp.data1$bac[i]!=0){
      Acumc <- Acumc + (Temp.data1$bac[i-1]*10000/area.plot)
      Temp.data1$BALc[i] <- Acumc
    }
    else {Temp.data1$BALc[i]=NA}
  }
  newBALc<-min(Temp.data1$BALc,na.rm = TRUE)
  for (i in (1:nrow(Temp.data1))){
    if (is.na(Temp.data1$BALc[i])){Temp.data1$BALc[i]=newBALc}
  }
  Temp.data1$PScal<-Temp.data1$BAL/Temp.data1$BA # Sociologis status calculated by competence and stocking
  Temp.data1<-arrange(Temp.data1, ID)
  return(Temp.data1)
}



