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
#' @param PS Sociologis status tree vector (dominant=1, codominant=2, intermediate=3, supressed=4)
#' @param A Stand age in years 
#' 
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' input.plot<-inputmodule(type='tree',zone=2,AD=28,HD=23.5,area=500,AF=35,tree.list=plot)
#' sim.tree<-tree_simulator(treelist=input.plot$tree.list, zone=input.plot$zone, area=input.plot$area, AD=input.plot$AD, ADF=input.plot$AF,DOM.SP=input.plot$DOM.SP,SI=input.plot$SI,AIDBH_model=1)
#' attributes(input.plot) 
#' head(input.plot$tree.list) 
#' input.plot$sp.table
#' attributes(sim.tree)
#' head(sim.tree$tree.list) 
#' sim.tree$sp.table
tree_simulator <- function(treelist=NA,
                             zone=NA, area=NA,
                             AD=NA, ADF=NA, DOM.SP=NA, SI=NA,
                             AIDBH_model=1){     #,Nmodel=1, theta = 0.0055452){
  ### Initializations of variables
  ID<-treelist$ID
  sp<-treelist$SPECIE
  DBH<-treelist$DBH
  HT<-treelist$HT
  Ss<-treelist$SS
  FT<-treelist$FT
  area<-area
  ZONA<-zone
  A<-AD
  DA<-AD
  input.data1<-covariates(ID=ID,area=area,sp=sp,DBH=DBH,ZONA=ZONA,Ss=Ss)
  Ss<-input.data1$Ss
  ### Simulation
    NARBp<-NA;areap<-NA;SPp<-NA;DBHp<-NA;ZONAp<-NA;BALcp<-NA;SDIp<-NA;Ap<-NA;PSp<-NA;DAp<-NA;PSCALp<-NA;Gest<-NA;DBHp1<-NA
    data.proy<-input.data1
    n<-nrow(data.proy)
    # Initial variables
    n<-nrow(data.proy)
    IDp<-data.proy$ID
    areap<-area
    SPp<-data.proy$sp
    DBHp<-data.proy$DBH
    ZONAp<-data.proy$ZONA
    BALcp<-data.proy$BALc
    SDIp<-data.proy$SDI
    data.proy$A<-A
    Ap<-data.proy$A
    SSp<-data.proy$Ss
    data.proy$DA<-DA
    DAp<-data.proy$DA
    PSCALp<-data.proy$PScal
    
    # identification of proyection period
    period<-ADF-AD
    
    #BY PERIOD
    for (k in 1:period) {
      #BY TREE
      for (j in (1:n)) {
        #Annual Increment in DBH by Tree
        Gest[j]<-AIDBH_module(BALc=BALcp[j], SDI=SDIp[j], DBH=DBHp[j], A=Ap[j], Ss=SSp[j],DA=DAp[j],PSCAL = PSCALp[j], SP=SPp[j],ZONE=ZONAp[j], Model=AIDBH_model)
        DBHp1[j]<-DBHp[j]+(Gest[j]/10)
      }
      
      #NEW PREDICTORS
      data.temp<-covariates(ID=ID,area=areap,sp=SPp,DBH=DBHp1,ZONA=ZONAp,Ss=SSp)
      DBHp<-data.temp$DBH
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
    plot.proy<-data.frame(data.temp,HT,FT,Ap,DAp)
    #obtaining tree.list
    treedata<-data.frame(plot.proy$ID, plot.proy$sp, round(plot.proy$DBH,2),round(plot.proy$HT,2),round(plot.proy$Ss,1),plot.proy$FT)
    colnames(treedata)<-c('ID','SPECIE','DBH','HT','SS','FT')
    #obtaining sp.table
    plotdata<-data.frame(plot.proy$sp, plot.proy$DBH, plot.proy$HT)
    colnames(plotdata)<-c('SPECIES','DBH','HT')
    params<-stand_parameters(plotdata=plotdata,area=area)
    #obtaining new HD
    DA<-unique(DAp)
    HD<-get_site(dom_sp=DOM.SP, zone=zone, AD=DA, SI=SI)
    return(list(sp.table=params, tree.list=treedata, zone=zone, DOM.SP=DOM.SP, AD=DAp, HD=HD, SI=SI, 
                SDI=unique(SDIp), AF=DA, area=area))
}

