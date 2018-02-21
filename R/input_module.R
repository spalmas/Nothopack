#' Module that requests input information for all stand- or tree-level current calculations or simulations
#'
#' \code{input_module} Module that requests input information for all stand- or tree-level current
#' calculations or simulations from the user, and completes some of the missing information that is required later.
#'
#' @param ZONE Growth zone (1, 2, 3, 4).
#' @param DOM.SP The dominant specie (1: Rauli, 2: Roble, 3: Coigue, 4:Mixed)
#' @param AD Dominant age (years) of the stand.
#' @param HD Dominant height (m) of the stand.
#' @param SI Site index at reference dominant age of 20 (m) of the stand.
#' @param sp.table
#' @param SDI Stand density index (trees/ha)
#' @param PBAN Proportion of basal area in the stand that correspond to Nothofagus.
#' @param PNHAN Proportion of number of trees per hectarea in the stand that correspond to Nothofagus.
#' @param AF Final dominant age (years) for simulation
#' @param tree.list Optional tree-list for a plot with columns: ID, SPECIE, DBH, HT, SS, FT)
#' @param area Area of plot from tree-list data (m2).
#' @param type Type of simulation required: stand: only stand-level, tree: only tree-level, both: both modules (default=stand)
#' @param ddiam Logical for requesting generation of diameter distribution (default=FALSE)
#' @param comptype NA: No compatibility (Default), PY: Proportional Yield, PG: Proportional Growth
#' @param NHA_model
#' @param V_molel
#' @param IADBH_model
#' @param N Vector of number of trees (trees/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others, 0: Total)
#' @param BA Vector of basal area (m2/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others, 0: Total)
#' @param QD Vector of quadratic diameters (cm) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others, 0: Total)
#'
#'#' @return Series of data input and parameters to be required for downstream modules. Particularly core module. The main
#' output elements are: sp.table    table with stand level summary by species (1,2,3,4) and total (0)
#'                      tree.list   data frame with complete tree list with missing values completed (e.g. HT)
#'                      input       list with all parameters of input and stand level statistics
#'                                  (ZONE, DOM.SP, AD, HD, SI, SDI, PBAN, PNHAN, AF, area, type, ddiam, comp, NHA_model,
#'                                  V_model,type, IADBH_model, sp.table, tree.list)
#'
#' @examples
#' # Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' input<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, N=N, BA=BA, type='stand')
#' input
#' input$sp.table
#'
#' # Example 2: Input from tree-level data (or file)
#' tree.list<-read.csv(file= 'data/Plot_example.csv')
#' head(tree.list)
#' plot<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, type='tree', area=500, tree.list=tree.list, Hest_method=1)
#' head(plot$tree.list)
#' plot$sp.table

input_module <- function(ZONE=NA,
                         AD=NA, HD=NA, SI=NA,
                         N=NA, BA=NA, QD=NA,
                         AF=NA, ddiam=FALSE, comptype=NA, thinning=FALSE,
                         type='stand',
                         tree.list=NA, area=NA,
                         NHA_model=1, V_model=1, IADBH_model=1,
                         Hest_method=1){

  DOM.SP <- NA
  SDIP <- NA
  PBAN <- NA
  PNHAN <- NA
  sdmatrix <- NA
  plotdata <- NA


  ## Basic traps for simulations (all types)
  # Errors with Age
  if(is.na(AF)){
    stop('There is no final age for simulation ')
  }
  # Need for at least two of AD, HD, SI
  if(is.na(AD)==T && is.na(HD)==T && is.na(SI)==T |
     is.na(AD)==F && is.na(HD)==T && is.na(SI)==T |
     is.na(AD)==T && is.na(HD)==F && is.na(SI)==T |
     is.na(AD)==T && is.na(HD)==T && is.na(SI)==F){
    stop("Warning - Please provide information for AD, HD or SI.")
  }

  # Processing stand-level information
  if (type=='stand'){

    if(is.na(N)==T && is.na(BA)==T && is.na()==T){
      stop('There is no enough information for simulations')
    }

    N[5]<-sum(N)
    BA[5]<-sum(BA)

    if (is.na(N[1])){
      (N[1]<-get_stand(QD=QD[1], BA=BA[1]))
      (N[2]<-get_stand(QD=QD[2], BA=BA[2]))
      (N[3]<-get_stand(QD=QD[3], BA=BA[3]))
      (N[4]<-get_stand(QD=QD[4], BA=BA[4]))
      (N[5]<-get_stand(QD=QD[5], BA=BA[5]))
    }
    if (is.na(BA[1])){
      (BA[1]<-get_stand(QD=QD[1], N=N[1]))
      (BA[2]<-get_stand(QD=QD[2], N=N[2]))
      (BA[3]<-get_stand(QD=QD[3], N=N[3]))
      (BA[4]<-get_stand(QD=QD[4], N=N[4]))
      (BA[5]<-get_stand(QD=QD[5], N=N[5]))
    }
    if (is.na(QD[1])){
      (QD[1]<-get_stand(BA=BA[1], N=N[1]))
      (QD[2]<-get_stand(BA=BA[2], N=N[2]))
      (QD[3]<-get_stand(BA=BA[3], N=N[3]))
      (QD[4]<-get_stand(BA=BA[4], N=N[4]))
      (QD[5]<-get_stand(BA=BA[5], N=N[5]))
    }

    # Basic checking for dominant sp
    DOM.SP<-get_domsp(BA=BA[1:4])
    if (DOM.SP==99) {
      stop("This stand is not dominated by Nothofagus")
    }

    # Completing AD, HD or SI
    if (is.na(AD)){
      (AD<-get_site(DOM.SP=DOM.SP, ZONE=ZONE, HD=HD, SI=SI))
    }
    if (is.na(HD)){
      (HD<-get_site(DOM.SP=DOM.SP, ZONE=ZONE, AD=AD, SI=SI))
    }
    if (is.na(SI)){
      (SI<-get_site(DOM.SP=DOM.SP, ZONE=ZONE, AD=AD, HD=HD))
    }

    # Check for AGE
    if(AD >= AF){
      stop('The Final age of simulation should be larger than the Initial Age')
    }

    # Completing the data frame for stand-table
    v1 <- c((1:4),0)
    v2 <- round(N,6)
    v3 <- round(BA,6)
    v4 <- round(QD,6)
    sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
    names(sdmatrix) <- c('SPECIES','N','BA','QD')

    PBAN <- sum(BA[1:3])/(BA[5])
    PNHAN <- sum(N[1:3])/(N[5])

    # Calculation of SDI_percentage
    b1 <- 1.4112
    SDI <- N[5]*(QD[5]/25.4)^b1  # Good for all DOM.SP
    if (DOM.SP==1 | DOM.SP==4) {
      SDIMax <- 1155.0 # Rauli and Mixed
    }
    if (DOM.SP==2) {
      SDIMax <- 908.8 # Roble
    }
    if (DOM.SP==2) {
      SDIMax <- 1336.9   # Coigue
    }
    SDIP <- round(100*SDI/SDIMax,6)

  }


  # Gathering tree-level information
  if (type=='tree'){

    #Check if the range of DBH is appropriate
    if (any(tree.list$DBH <= 5 |tree.list$DBH > 90)){
      warning('Some trees are smaller than 5cm or larger than 90 cm DBH')
    }


    # Some checks from input
    if(nrow(tree.list)==0){
      stop('There is no tree data for type=TREE')
    }
    if(is.na(area)){
      stop('The plot area is not provided for type=TREE')
    }

    # Getting stand level parms from tree-list
    if (sum(is.na(tree.list$FT))>0) {
       FT<-matrix(1,nrow=nrow(tree.list),ncol=1)
       FT<-FT*(10000/area)
    } else {
       FT<-tree.list$FT
    }
    plotdata<-data.frame(tree.list$ID, tree.list$SPECIES, tree.list$DBH, tree.list$HT, tree.list$SS, FT)
    colnames(plotdata)<-c('ID','SPECIES','DBH','HT','SS','FT')
    params<-stand_parameters(plotdata=plotdata,area=area)

    # Completing SS
    if (sum(is.na(plotdata$SS))>0){
      plotdata$SS<-tree_covariates(ID=plotdata$ID,FT=plotdata$FT,SPECIES=plotdata$SPECIES,DBH=plotdata$DBH,ZONE=ZONE)$SS
    }

    # Basic checking for dominant sp
    DOM.SP<-params$DOM.SP
    if (DOM.SP==99) {
      stop("This stand is not dominated by Nothofagus")
    }

    # Completing AD, HD or SI
    if (is.na(AD)){
      (AD<-get_site(DOM.SP=DOM.SP, ZONE=ZONE, HD=HD, SI=SI))
    }
    if (is.na(HD)){
      (HD<-get_site(DOM.SP=DOM.SP, ZONE=ZONE, AD=AD, SI=SI))
    }
    if (is.na(SI)){
      (SI<-get_site(DOM.SP=DOM.SP, ZONE=ZONE, AD=AD, HD=HD))
    }

    # Completing heights using parametrized height-dbh model
    QD<-params$sd[5,4]
    plotdata$HT<-tree.ht(DBH=plotdata$DBH, HT=plotdata$HT,
                         DOM.SP=DOM.SP, ZONE=ZONE, HD=HD, QD=QD, method=Hest_method)$HTFIN

    # Collecting final stand parameters
    sdmatrix <- params$sd

    PBAN <- sum(sdmatrix[1:3,3])/sdmatrix[5,3]
    PNHAN <- sum(sdmatrix[1:3,2])/sdmatrix[5,2]

    # Calculation of SDI_percentage
    b1 <- 1.4112
    SDI <- sdmatrix[5,2]*(sdmatrix[5,4]/25.4)^b1  # Good for all DOM.SP
    if (DOM.SP==1 | DOM.SP==4) {
      SDIMax <- 1155.0 # Rauli and Mixed
    }
    if (DOM.SP==2) {
      SDIMax <- 908.8 # Roble
    }
    if (DOM.SP==2) {
      SDIMax <- 1336.9   # Coigue
    }
    SDIP <- round(100*SDI/SDIMax,6)

  }

  # List that is output from here input somewhere else
  input <- list(ZONE=ZONE, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDIP=SDIP, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
                area=area, type=type, ddiam=ddiam, comptype=comptype, NHA_model=NHA_model, V_model=V_model,
                IADBH_model=IADBH_model, sp.table=sdmatrix, tree.list=plotdata)

  return(input=input)

}

#needs to receive whatever information from the file read in the simulator and turn it into something that
#core_module can use. Can this be done inside the core_module? I feel that these are very similar scripts.
# Note: What if we don't want a simulation? Can it accept AF==AF?
# Note, what happens if the user wants to calculate HD form tree.data - it uses other routines.
# Note, need to make sure that the parametriz height-DBH yields to the same HD as in the input.
# Note: SS is completed if missing
# Note: the only thing no traspased in the retunr(input) is the Hest_method

