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
#' 

tree_simulator <- function(tree.list=NA,
                             zone=NA, area=NA,
                             AD=NA, ADF=NA, DOM.SP=NA, SI=NA,
                             AIDBH_model=1){#,Nmodel=1, theta = 0.0055452){
  ### Initializations of variables
  ID<-tree.list$ID
  sp<-tree.list$SPECIE
  DBH<-tree.list$DBH
  HT<-tree.list$HT
  SS<-tree.list$SS
  FT<-tree.list$FT
  area<-area
  ZONA<-zone
  A<-AD
  DA<-AD
  input.data1<-covariates(ID,area,sp,DBH,ZONA,SS)
  
  ### Simulation
    data.proy<-input.data1
    # Initial variables
    IDp<-data.proy$ID
    areap<-area
    SPp<-data.proy$sp
    DBHp<-data.proy$DBH
    ZONAp<-data.proy$ZONA
    BALcp<-data.proy$BALc
    SDIp<-data.proy$SDI
    Ap<-A
    SSp<-data.proy$SS
    DAp<-DA
    PSCALp<-data.proy$PScal
    
    # identification of proyection period
    period<-ADF-AD
    
    #BY PERIOD
    for (k in 1:period) {
      #BY TREE
      for (j in (1:n)) {
        #Annual Increment in DBH by Tree
        Gest[j]<-AIDBH_module(BALc=BALcp[j], SDI=SDIp[j], DBH=DBHp[j], A=Ap[j], PS=PSp[j],DA=DAp[j],PSCAL = PSCALp[j], SP=SPp[j],ZONE=ZONAp[j], Model=AIDBH_model)
        DBHp1[j]<-DBHp[j]+(Gest[j]/10)
      }
      
      #NEW PREDICTORS
      data.temp<-covariates(ID,areap,SPp,DBHp1,ZONAp,SSp)
      DBHp<-data.temp$DBH
      BALcp<-data.temp$BALc
      SDIp<-data.temp$SDI
      Ap<-Ap+1# new age
      DAp<-DAp+1# new dominant age
      PSCALp<-data.temp$PScal
      NHAp<-data.temp$NHA
      BAp<-data.temp$BA
      QDp<-data.temp$QD
      SSp<-data.temp$SS
    }
    plot.proy<-data.frame(data.temp,HT,FT,Ap,DAp)
    #obtaining tree.list
    treedata<-data.frame(plot.proy$ID, plot.proy$SP, plot.proy$DBH,plot.proy$HT,plot.proy$SS,plot.proy$FT)
    #obtaining sp.table
    plotdata<-data.frame(plot.proy$SP, plot.proy$DBH, plot.proy$HT)
    colnames(plotdata)<-c('SPECIES','DBH','HT')
    params<-stand_parameters(plotdata=plotdata,area=area)
    #obtaining new HD
    HD<-get_site(dom_sp=DOM.SP, zone=zone, AD=DAp, SI=SI)
    return(list(sp.table=params, tree.list=treedata, zone=zone, DOM.SP=DOM.SP, AD=DAp, HD=HD, SI=SI, 
                SDI=unique(SDIp), AF=DAp, area=area))
}

