#' To obtain Stand and Individual variables for feeding the growth tree model in DBH
#'
#' @param ID Array of identification of each tree
#' @param area Array of Area of the plot
#' @param sp Array of species of plot
#' @param DBH Array of DBHs
#' @param ZONA Array of zona
#'
#' @return 8 different predictors ID, DBH, ba (basal area individual, m2/ha), bac (basal area individual cohorte, m2/ha),
#' SPZONA (concatenation), N (number of trees per ha), BA (basal area, m2/ha), QD (quadratic diameter, cm),
#' SDI (Stand Density Index), BAL (Basal Area Larges trees, m2/ha), BALc (Basal Area Larges trees, m2/ha), PSCAL (BAL/BA)
#'
#' @examples
#' ID<-c(1,2,3,4)
#' Fa<-c(400,400,400,400)
#' sp<-c(1,1,2,4)
#' DBH<-c(15,25,37, 20)
#' ZONE<-1
#' Ss<-c(1,2,2,2)
#' covariates(ID,Fa,sp,DBH,ZONE,Ss)


## Code for obtaining covariates from ingrowth DBH models
## Require plot with id tree, area, sp, DBH and zone

covariates<-function(ID=NA,Fa=NA,sp=NA,DBH=NA,ZONE=NA,Ss=NA){

  N <- length(ID)#Getting the number of trees in the plot
  NHA <- sum(Fa) #Getting the number of trees in the stand by hectare
  BA <-   sum(pi * (DBH/2/100)^2* Fa , na.rm = TRUE) #total basal area by hectare
  QD <-  (100*((4/pi)*(BA/NHA))^0.5)#total quadratic diameter,
  SDI<-NHA*((25.4/QD)^(-1.4112))#Stand density index
  SPZONA<-sp*10+as.numeric(ZONE)   #Why times a zone?
  ###BAL & BALc Calculation
  ba<-pi*((DBH)^2)/40000 #in m2
  bac<-0

  for (fila in (1:N)) {
    bac[fila] <- if (sp[fila] != 4) {ba[fila]} else {0}
  }

  #Creating a table BAL and BALc info
  Temp.data<-data.frame(ID,Fa,DBH,ba,bac,sp,ZONE,SPZONA,NHA,BA,QD,SDI,Ss) %>%
    arrange(desc(ba)) %>%
    mutate(
      BAL = (lag(ba) %>% replace(is.na(.), 0) %>% cumsum) * Fa,
      BALc = (lag(bac) %>% replace(is.na(.), 0) %>% cumsum) * Fa,
      BALc = BALc %>% replace(is.na(.), mean(BALc)),   #replacing to the mean
      PScal = BAL/BA
    )

  # Sociologic status calculated by competence and stocking
  for (i in (1:nrow(Temp.data))){
    if (is.na(Temp.data$Ss[i])){
      Temp.data$Ss[i]<-SS(Temp.data$PScal[i])
    }
  }

  #needs to go back to original order of trees
  Temp.data<-arrange(Temp.data, ID)
  return(Temp.data)
}

