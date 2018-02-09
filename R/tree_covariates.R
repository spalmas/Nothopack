#' Obtains stand- and individual-level covariables to feed the DBH tree growth model
#' 
#' \code{tree.covariates} Obtains/calculates several stand- and individual-level covariables
#' that form part of the individual-tree DBH growth model (AIDBH). The input corresponds to tree data 
#' originated from an inventory of a single plot.
#'
#' @author Paulo Moreno, Salvador A. Gezan
#'
#' @param ID Vector of identification of each tree.
#' @param FT Vector of expansion factor (\emph{i.e.}, FT = 1/area) for each tree.
#' @param SPECIES Vector of species code (1: Rauli, 2: Roble, 3: Coigue, 4:Others) for each tree.
#' @param DBH Vector of diameter breast height (DBH, cm) for each tree.
#' @param SS Vector of total height (m). Must be of the same size and order as TREEID.
#' @param ZONE Numeric value of growth zone (1, 2, 3, 4) for the plot.

#' @return A list containing the following:
#' 
#' 8 different predictors ID, DBH, ba (basal area individual, m2/ha), bac (basal area individual cohorte (Nothofagus), m2/ha),
#' SPZONA (concatenation), N (number of trees per ha), BA (basal area, m2/ha), QD (quadratic diameter, cm),
#' SDI (Stand Density Index), BAL (Basal Area Larges trees, m2/ha), BALc (Basal Area Larges trees cohorte (Nothofagus), m2/ha),
#' PSCAL (BAL/BA)
#'
#'#' @return A list containing the following:
#' \itemize{
#' \item \code{HTFIN} A vector of final tree heigths (m), replacing the missing values for estimated heigths and retaining the observed heights.
#' \item \code{r2} A value with the coefficient of determination from the fitting the DBH-height model when method = 2.
#' }
#'
#' @references
#' Moreno et al. (2017) - XX
#'
#' @examples
#' Example 1 - Simple data
#' ID<-c(1,2,3,4)
#' FT<-c(400,400,400,400)
#' SPECIES<-c(1,1,2,4)
#' DBH<-c(15,25,37, 20)
#' SS<-c(NA,2,NA,2)
#' tree_covariates(ID=ID, FT=FT, SPECIES=SPECIES, DBH=DBH, ZONE=1, SS=SS)
#' 
#' Example 2 - Full single plot data
#' tree.list<-read.csv(file= 'data/Plot_example.csv')
#' head(tree.list)
#' plot<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, type='tree', area=500, tree.list=tree.list, Hest_method=1)$tree.list
#' head(plot)
#' covar<-tree_covariates(ID=plot$ID, FT=plot$FT, SPECIES=plot$SPECIES, DBH=plot$DBH, ZONE=2, SS=plot$SS)
#' head(covar)
#' View(covar)


tree_covariates<-function(ID=NA, FT=NA, SPECIES=NA, DBH=NA, ZONE=NA, SS=NA){

  N <- length(ID)                              # Getting the number of trees in the plot
  NHA <- sum(FT)                               # Getting the number of trees in the stand by hectare
  BA <- sum(pi*(DBH/2/100)^2*FT, na.rm = TRUE) # Total basal area by hectare
  QD <- (100*((4/pi)*(BA/NHA))^0.5)            # Quadratic Diameter,
  SDI<- NHA*((25.4/QD)^(-1.4112))              # Stand density index
  SPZONA<-SPECIES*10+as.numeric(ZONE)          # Why times a zone?
  
  # BAL & BALc calculation
  ba<-pi*((DBH)^2)/40000  # in m2
  bac<-0

  for (fila in (1:N)) {
    bac[fila] <- if (SPECIES[fila] != 4) {ba[fila]} else {0}
  }

  # Creating a table BAL and BALc info
  Temp.data<-data.frame(ID,FT,DBH,ba,bac,SPECIES,ZONE,SPZONA,NHA,BA,QD,SDI,SS) %>%
    arrange(desc(ba)) %>%
    mutate(
      BAL = (lag(ba) %>% replace(is.na(.), 0) %>% cumsum) * FT,
      BALc = (lag(bac) %>% replace(is.na(.), 0) %>% cumsum) * FT,
      BALc = BALc %>% replace(is.na(.), mean(BALc)),   #replacing to the mean
      PScal = BAL/BA
    )

  # Sociological status calculated by competence and stocking
  for (i in (1:nrow(Temp.data))){
    if (is.na(Temp.data$SS[i])){
      Temp.data$SS[i]<-tree.SS(Temp.data$PScal[i])
    }
  }

  # Needs to go back to original order of trees
  Temp.data<-arrange(Temp.data, ID)
  
  return(Temp.data)
  
}
