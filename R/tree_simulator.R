#' Simulates individual-tree growth in DBH using AIDBH_module and Nmodule
#'
#' \code{tree_simulator} Simulates tree level growth, mortality (and recruitment) of a given stand
#' requiring stand-level and tree parameters that combine all species. Simulations are done using tree-level
#' models starting from intial age (AD0) until final age (ADF) in increments of 1 year.
#' The model of mortality is the same as whole-stand
#'
#' @param IDplot Identification plot vector
#' @param ID Identification tree vector
#' @param SP Specie tree vector (1:Rauli, 2:Roble, 3:Coigue, 4:Others)
#' @param Zone Growth zone of the corresponding stand vector (1,2,3 and 4)
#' @param DBH DBH tree vector
#' @param H height tree vector
#' @param PS Sociological status tree vector (dominant=1, codominant=2, intermediate=3, supressed=4)
#' @param A Stand age in years
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' input<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=35,tree.list=plot)
#' attributes(input)
#' head(input$tree.list)
#' core.tree<-core_module(input=input$input)
#' head(core.tree$tree.list)
#' sim.tree<-tree_simulator(core.tree=core.tree$input)
#' head(sim.tree$input) ###
#' result.tree<-core_module(input=sim.tree$input)
#' result.tree$sp.table
#' result.tree$stand.table[5,,]
#' head(result.tree$tree.list)

tree_simulator <- function(core.tree = NULL){

  ### Initializations of variables
  HT<-core.tree$tree.list$HT   #list heights
  #Ss<-core.tree$tree.list$SS   #list sociological status NA
  FTv<-rep(1,length(core.tree$tree.list$ID))    #expansion factor for volume?
  input.data1<-covariates(ID=core.tree$tree.list$ID,
                          Fa=core.tree$tree.list$FT,    #list of expansion factors
                          sp=core.tree$tree.list$SPECIES,
                          DBH=core.tree$tree.list$DBH,      #list dbh
                          ZONE=core.tree$zone)   #estimating stand and individual variables

  #---===
  # SIMULATION ----
  #---===

  # Initial variables. This are updated every year.
  Fap<-core.tree$tree.list$FT
  QDp<-input.data1$QD
  DBHp<-input.data1$DBH
  BALcp<-input.data1$BALc
  SDIp<-input.data1$SDI
  Ap<-FTv*core.tree$AD
  SSp<-input.data1$Ss
  DAp<-FTv*core.tree$AD
  PSCALp<-input.data1$PScal

  #FROM INITIAL AGE TO FINAL AGE (LOOP 1)
  for (k in core.tree$AD:core.tree$AF) {
    #BY TREE (LOOP 2)

    #ANNUAL INCREMENT
    Gest<-AIDBH_module(BALc=BALcp,
                       SDI=SDIp,
                       DBH=DBHp,
                       A=Ap,
                       Ss=SSp,
                       DA=DAp,
                       PSCAL = PSCALp,
                       SP=input.data1$sp,
                       ZONE=input.data1$ZONE,
                       Model=core.tree$IADBH_model)
    DBHp1<-DBHp+(Gest/10)

    #Dif.DBH <- Gest/10

    #MORTALITY
    N1<-NHAmodule(NHA0=input.data1$NHA,QD0=QDp,NHA_model=core.tree$NHA_model)
    Mortality<-unique(input.data1$NHA)-N1
    BALr<-PSCALp/sum(PSCALp)
    Mi<-BALr*Mortality
    Fap1<-Fap-Mi

    #NEW PREDICTORS
    data.temp<-covariates(ID=core.tree$tree.list$ID,
                          Fa=Fap1,
                          sp=input.data1$sp,
                          DBH=DBHp1,
                          ZONE=input.data1$ZONE,
                          Ss=SSp)

    #UPDATE VALUES
    DBHp<-data.temp$DBH
    Fap<-data.temp$Fa
    BALcp<-data.temp$BALc
    SDIp<-data.temp$SDI
    Ap<-Ap+1# new age
    DAp<-DAp+1# new dominant age
    PSCALp<-data.temp$PScal
    NHAp<-data.temp$NHA
    BAp<-data.temp$BA
    QDp<-data.temp$QD
    SSp<-data.temp$Ss
  }
  plot.proy<-data.frame(data.temp,HT,Fap,Ap,DAp)


  # Height Increment Module
  HT0<-HT
  m<-length(HT0)
  HTparam.0 <- matrix(data=0,nrow=m,ncol=1)
  HTparam.n <- matrix(data=0,nrow=m,ncol=1)
  Dif.HT <- matrix(data=0,nrow=m,ncol=1)
  HDp<-get_site(dom_sp=core.tree$DOM.SP, zone=core.tree$zone, AD=DAp[1], SI=core.tree$SI)

  #predicted height at the start (AD)
  HTparam.0<-height_param(dom_sp=core.tree$DOM.SP,
                             zone=core.tree$zone,
                             HD=core.tree$HD,
                             QD=input.data1$QD[1],
                             DBH=input.data1$DBH)

  #predicted height at the end (AF)
  HTparam.n<-height_param(dom_sp=core.tree$DOM.SP,
                             zone=core.tree$zone,
                             HD=HDp,
                             QD=QDp[1],
                             DBH=DBHp)

  #Difference in height growth
  Dif.HT<-HTparam.n-HTparam.0
  Dif.HT <-  Dif.HT %>% replace(.<0, 0)

  #assigning new height
  HTn<-HT0+Dif.HT
  #print(data.frame(HT0,HTparam.0,HTparam.n,HTn,Dif.HT))

  #obtaining tree.list
  treedata<-data.frame(plot.proy$ID,
                       plot.proy$sp,
                       round(plot.proy$DBH,2),
                       round(HTn,2),
                       round(plot.proy$Ss,1),
                       round(plot.proy$Fap,3))
  colnames(treedata)<-c('ID','SPECIES','DBH','HT','SS','FT')

  comp.list <- data.frame(prob.surv=Fap1/core.tree$tree.list$FT,PAI.HT=Dif.HT,PAI.DBH=Gest)

  input <- list(zone=core.tree$zone, DOM.SP=core.tree$DOM.SP, AD=DAp[1],
                HD=core.tree$HD, SI=core.tree$SI, SDI=SDIp[1],
                PBAN=core.tree$PBAN, PNHAN=core.tree$PNHAN, AF=DAp[1],
                area=core.tree$area, type=core.tree$type, ddiam=core.tree$ddiam,
                comp=core.tree$comp, NHA_model=core.tree$NHA_model,
                V_model=core.tree$V_model, IADBH_model=core.tree$IADBH_model,
                sp.table=NA, stand.table=NA, tree.list=treedata, comp.list = NA)

  #
  # input <- list(tree.list=treedata, zone=zone, DOM.SP=DOM.SP, AD=DAp[1], SI=SI, HD=HD,
  #                    SDI=unique(SDIp), area=area,type='tree')
  #
  return(list(input=input))
}

