#' Simulates individual-tree growth in DBH using AIDBH_module and Nmodule
#'
#' \code{tree_simulator} Simulates tree level growth, mortality (and recruitment) of a given stand
#' requiring stand-level and tree parameters that combine all species. Simulations are done using tree-level
#' models starting from intial age (AD0) until final age (ADF) in increments of 1 year.
#' The model of mortality is the same as whole-stand
#'
#' @param core.stand a stand list corrected from core_module
#' 
#' @references
#' Moreno, P.; Palmas, S.; Escobedo, F.; Cropper, W.; Gezan, S. Individual-tree diameter growth models 
#' for mixed nothofagus second growth forests in southern chile. Forests 2017, 8, 506, http://www.mdpi.com/1999-4907/8/12/506.
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' New Example 1
#' tree.list<-read.csv(file= 'data/Plot_example.csv')
#' head(tree.list)
#' plot<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, type='tree', area=500, tree.list=tree.list, Hest_method=1)
#' sims<-tree_simulator(core.tree=plot)
#' head(plot$tree.list)
#' head(sims$tree.list)
#' plot$sp.table
#' sims$sp.table
#' 
#' Original Example
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' input<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=49,tree.list=plot)## To complete info
#' head(input$tree.list)
#' core.tree<-core_module(input=input$input)## To incorporate volume, BA and Dclass
#' head(core.tree$tree.list)
#' sim.tree<-tree_simulator(core.tree=core.tree$input)## Tree simulator
#' result.tree<-core_module(input=sim.tree$input)
#' result.tree$sp.table
#' result.tree$DDist[5,,]
#' head(result.tree$tree.list)


tree_simulator <- function(core.tree = NULL){

  ### Initializations of variables
  HT <- core.tree$tree.list$HT   #list heights
  ZONE <- core.tree$ZONE
  DOM.SP <- core.tree$DOM.SP
  SI <- core.tree$SI
  HD <- core.tree$HD
  
  FTv<-rep(1,length(core.tree$tree.list$ID))    #expansion factor vector
  input.data1<-tree_covariates(ID=core.tree$tree.list$ID, FT=core.tree$tree.list$FT,    
                               SPECIES=core.tree$tree.list$SPECIES, DBH=core.tree$tree.list$DBH,     
                               ZONE=ZONE, SS=core.tree$tree.list$SS)   #estimating stand and individual variables

  # Initial variables. This are updated every year.
  Fap <- input.data1$FT
  QDp <- input.data1$QD
  DBHp <- input.data1$DBH
  BALcp <- input.data1$BALc
  SDIp <- input.data1$SDI  # Not sure if this is % or trees/ha
  Ap <- FTv*core.tree$AD
  SSp <- input.data1$SS
  DAp <- FTv*core.tree$AD
  PSCALp <- input.data1$PScal
  SP <- input.data1$SPECIES
  
  # FROM INITIAL AGE TO FINAL AGE (LOOP 1)
  for (k in core.tree$AD:(core.tree$AF-1)) {
    #BY TREE (LOOP 2)

    # ANNUAL INCREMENT
    Gest <- AIDBH_module(BALc=BALcp, SDI=SDIp, DBH=DBHp, A=Ap, Ss=SSp,
                       DA=DAp, PSCAL = PSCALp, SP=SP, ZONE=ZONE,
                       Model=core.tree$IADBH_model)
    DBHp1 <- DBHp+(Gest/10)
    # Dif.DBH <- Gest/10

    # MORTALITY
    N1 <- NHAmodule(NHA0=input.data1$NHA,QD0=QDp,NHA_model=core.tree$NHA_model)   # How many survive (done as vector)
    Mortality <- unique(input.data1$NHA)-N1    #How many should die
    BALr <- PSCALp/sum(PSCALp)    #Some BAL for each tree
    Mi <- BALr*Mortality     #Correct mortality based on BAL
    Fap1 <- Fap-Mi     #Corrected expansion factor. The sum is equal to N1.

    # NEW COVARIATES
    #data.temp<-covariates(ID=core.tree$tree.list$ID, Fa=Fap1, sp=input.data1$sp,DBH=DBHp1, ZONE=input.data1$ZONE, Ss=SSp)
    data.temp <- tree_covariates(ID=core.tree$tree.list$ID, FT=Fap1, SPECIES=SP, DBH=DBHp1, ZONE=ZONE, SS=SSp)
    
    #UPDATE VALUES
    DBHp <- data.temp$DBH
    Fap <- data.temp$FT
    BALcp <- data.temp$BALc
    SDIp <- data.temp$SDI
    Ap <- Ap+1    # new age
    DAp <- DAp+1  # new dominant age
    PSCALp <- data.temp$PScal
    NHAp <- data.temp$NHA
    BAp <- data.temp$BA
    QDp <- data.temp$QD
    SSp <- data.temp$SS
    
  }
  
  plot.proy <- data.frame(data.temp,HT,Fap,Ap,DAp)


  # Height Increment Module
  HT0 <- HT
  m <- length(HT0)
  HTparam.0 <- matrix(data=0,nrow=m,ncol=1)
  HTparam.n <- matrix(data=0,nrow=m,ncol=1)
  Dif.HT <- matrix(data=0,nrow=m,ncol=1)
  HDp <- get_site(DOM.SP=DOM.SP, ZONE=ZONE, AD=DAp[1], SI=SI)

  
  (HT<-height_param(DOM.SP=2, ZONE=2, HD=15, QD=12, DBH=24))
  
  # predicted height at the start (AD)
  HTparam.0 <- height_param(DOM.SP=DOM.SP, ZONE=ZONE,
                            HD=HD, QD=input.data1$QD[1],
                            DBH=input.data1$DBH)

  # predicted height at the end (AF)
  HTparam.n <- height_param(DOM.SP=DOM.SP, ZONE=ZONE,
                            HD=HDp, QD=QDp[1],
                            DBH=DBHp)

  # Difference in height growth
  Dif.HT <- HTparam.n-HTparam.0
  Dif.HT <-  Dif.HT %>% replace(.<0, 0)

  # Assigning new height
  HTn <- HT0+Dif.HT
  #print(data.frame(HT0,HTparam.0,HTparam.n,HTn,Dif.HT))

  # Generating new tree.list
  treedata<-data.frame(plot.proy$ID,
                       plot.proy$SPECIES,
                       round(plot.proy$DBH,6),
                       round(HTn,6),
                       round(plot.proy$SS,6),
                       round(plot.proy$FT,6))
  colnames(treedata)<-c('ID','SPECIES','DBH','HT','SS','FT')

  # What is this for???????
  #comp.list <- data.frame(prob.surv=Fap1/core.tree$tree.list$FT,PAI.HT=Dif.HT,PAI.DBH=Gest)

  # Actualization of many things
  updated.plot<-input_module(ZONE=ZONE, AD=core.tree$AF, HD=HDp, SI=SI, AF=core.tree$AF,
                             ddiam=core.tree$ddiam, comp=core.tree$ddiam, thinning=core.tree$thinning,
                             type=core.tree$type, tree.list=treedata, area=core.tree$area,
                             NHA_model=core.tree$NHA_model, V_model=core.tree$V_model, IADBH_model=core.tree$IADBH_model,
                             Hest_method=2)
  
  return(output=updated.plot)
  
  #return(list(input=input, comp.list =  comp.list))

}

