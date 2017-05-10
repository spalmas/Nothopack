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
#' tree<-inputmodule(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=35,tree.list=plot)
#' attributes(tree) 
#' head(tree$tree.list) 
#' core.tree<-core_module(input=tree$input)
#' core.tree$sp.table
#' head(core.tree$tree.list)
#' sim.tree<-tree_simulator(core.tree=core.tree$input)
#' attributes(sim.tree)
#' head(sim.tree$input) ###
#' result.tree<-core_module(input=sim.tree$input)
#' result.tree$sp.table
#' result.tree$stand.table[5,,]
#' head(result.tree$tree.list)
#' report(core.stand=result.tree)

tree_simulator <- function(core.tree = NULL){
  
  ### Initializations of variables
  ID<-core.tree$tree.list[,1]
  sp<-core.tree$tree.list[,2]
  DBH<-core.tree$tree.list[,3]
  HT<-core.tree$tree.list[,4]
  Ss<-core.tree$tree.list[,5]
  FT<-core.tree$tree.list[,6]
  FTv<-rep(1,length(ID))
  Fa<-FT
  ZONA<-FTv*core.tree$zone
  A<-FTv*core.tree$AD
  DA<-FTv*core.tree$AD
  ADF<-core.tree$AF
  AD<-core.tree$AD
  AIDBH_model<-core.tree$IADBH_model
  N_model<-core.tree$N_model
  area<-core.tree$area
  zone<-core.tree$zone
  SI<-core.tree$SI
  DOM.SP<-core.tree$DOM.SP
  HD<-core.tree$HD
  input.data1<-covariates(ID=ID,Fa=Fa,sp=sp,DBH=DBH,ZONA=ZONA,Ss=Ss)
  Ss<-input.data1$Ss
  
  ### SIMULATION
  
    NARBp<-NA;QDp<-NA;Fap<-NA;SPp<-NA;DBHp<-NA;ZONAp<-NA;BALcp<-NA;SDIp<-NA;Ap<-NA;PSp<-NA;DAp<-NA;PSCALp<-NA;Gest<-NA;DBHp1<-NA
    data.proy<-input.data1
    n<-nrow(data.proy)
    # Initial variables
    n<-nrow(data.proy)
    IDp<-data.proy$ID
    Fap<-Fa
    NARBp<-data.proy$NHA
    QDp<-data.proy$QD
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
    
    #BY PERIOD (LOOP 1)
    for (k in 1:period) {
      #BY TREE (LOOP 2)
      for (j in (1:n)) {
        #Annual Increment in DBH by Tree
        Gest[j]<-AIDBH_module(BALc=BALcp[j], SDI=SDIp[j], DBH=DBHp[j], A=Ap[j], Ss=SSp[j],DA=DAp[j],PSCAL = PSCALp[j], SP=SPp[j],ZONE=ZONAp[j], Model=AIDBH_model)
        DBHp1[j]<-DBHp[j]+(Gest[j]/10)
      }
  
      #MORTALITY
      N1<-Nmodule(NHA0=NARBp,QD0=QDp,N_model=N_model)
      Mortality<-unique(NARBp)-N1
      BALr<-PSCALp/sum(PSCALp)
      Mi<-BALr*Mortality
      Fap1<-Fap-Mi
      
      #NEW PREDICTORS
      data.temp<-covariates(ID=ID,Fa=Fap1,sp=SPp,DBH=DBHp1,ZONA=ZONAp,Ss=SSp)
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
    HDp<-get_site(dom_sp=DOM.SP, zone=zone, AD=DAp[1], SI=SI)
    
    for (i in (1:m)) {
      HTparam.0[i]<-height_param(dom_sp=DOM.SP, zone=zone, HD=HD, QD=data.proy$QD[1], DBH=DBH[i])
      HTparam.n[i]<-height_param(dom_sp=DOM.SP, zone=zone, HD=HDp, QD=QDp[1], DBH=DBHp[i])
      Dif.HT[i]<-HTparam.n[i]-HTparam.0[i]
      if (Dif.HT[i]<=0) {Dif.HT[i]=0}
    }
    HTn<-HT0+Dif.HT
    #print(data.frame(HT0,HTparam.0,HTparam.n,HTn,Dif.HT))

    #obtaining tree.list
    treedata<-data.frame(plot.proy$ID, plot.proy$sp, round(plot.proy$DBH,2),round(HTn,2),round(plot.proy$Ss,1),round(plot.proy$Fap,3))
    colnames(treedata)<-c('ID','SPECIES','DBH','HT','SS','FT')
    
    input <- list(zone=zone, DOM.SP=DOM.SP, AD=DAp[1], HD=HD, SI=SI, SDI=SDIp[1], 
                  PBAN=core.tree$PBAN, PNHAN=core.tree$PNHAN, AF=DAp[1],
                  area=core.tree$area, type=core.tree$type, ddiam=core.tree$ddiam, comp=core.tree$comp, N_model=core.tree$N_model,
                  V_model=core.tree$V_model, IADBH_model=core.tree$IADBH_model, start_time=core.tree$start_time, 
                  sp.table=NA, stand.table=NA, tree.list=treedata)
    
    # 
    # input <- list(tree.list=treedata, zone=zone, DOM.SP=DOM.SP, AD=DAp[1], SI=SI, HD=HD,
    #                    SDI=unique(SDIp), area=area,type='tree')
    # 
    return(list(input=input))
}

