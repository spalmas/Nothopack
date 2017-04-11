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
#' #Example 1. Starting from known stand-level data
#' sims <- stand_simulator3(vBA=c(20,4,0,0), vNHA=c(650,113,0,0), zone = 1, HD0=18.45, AD0 = 20, ADF = 40, Nmodel = 1)
#' sims
#' plot_results(sims)
#'
#' #Example 2. Input in form vBA as c(BAN, BA99) and vNHA as c(NHAN, NHA99)
#' sims <- stand_simulator3(vBA=c(24,0), vNHA=c(763,0), zone = 1, HD0=18.45, AD0 = 20, ADF = 40, Nmodel = 1, dom_sp = 1)
#' sims
#' plot_results(sims)

#BY PLOT
### Initializations of variables
tree.data<- read.csv(file= 'data/Plot_example.csv')
names(tree.data) <- c('ID','SPECIES','DBH','AD','HT','PS')
input.data<-inputmodule(level='tree',zone=2,AD=52,SI=14.53,area=500,tree.data=tree.data)
#pl<-list(unique(input.data$PARCELA))
pl<-1
final.data<-input.data[FALSE,]
for (i in pl) {
  #pl<-i
  input.data1<-input.data[input.data$PARCELA==i,]
  ID<-input.data1$tree.matrix[,1]
  area<-input.data1$area
  sp<-input.data1$tree.matrix[,2]
  DBH<-input.data1$tree.matrix[,3]
  ZONA<-input.data1$zone
  A<-input.data$AD
  DA<-input.data$AD
  source('covariates.R')
  COV<-covariates(ID,area,sp,DBH,ZONA)
  input.data1<-merge(input.data1, COV, by = 'ID')
  final.data<-bind_rows(final.data,input.data1)
}

### Simulation
#plots<-list(unique(final.data$plot))
plots<-1
final.proy<-data.frame(aname=NA, bname=NA)[numeric(0), ]
NARBp<-NA;areap<-NA;SPp<-NA;DBHp<-NA;ZONAp<-NA;BALcp<-NA;SDIp<-NA;Ap<-NA;PSp<-NA;DAp<-NA;PSCALp<-NA;Gest<-NA;DBHp1<-NA
for (i in plots) {
  ID<-NA;areap<-NA;SPp<-NA;DBHp<-NA;ZONAp<-NA;BALcp<-NA;SDIp<-NA;Ap<-NA;PSp<-NA;DAp<-NA;PSCALp<-NA;Gest<-NA;DBHp1<-NA
  #data.proy<-final.data[final.data$plot==i,]
  data.proy<-final.data
  n<-nrow(data.proy)
  # Initial variables
  ID<-data.proy$ID
  areap<-data.proy$area
  SPp<-data.proy$sp
  DBHp<-data.proy$DBH
  ZONAp<-data.proy$ZONA
  BALcp<-data.proy$BALc
  SDIp<-data.proy$SDI
  Ap<-data.proy$A
  PSp<-data.proy$PS
  DAp<-data.proy$DA
  PSCALp<-data.proy$PScal
  
  # identification of proyection period
  period<-inputdata$AF-inputdata$AD
  
  #BY PERIOD
  for (k in 1:period) {
    
    #BY TREE
    # Initial variables
    for (j in (1:n)) {
      #Annual Increment in DBH by Tree
      source('AIDBH_module.R')
      Gest[j]<-AIDBH_module(BALc=BALcp[j], SDI=SDIp[j], DBH=DBHp[j], A=Ap[j], PS=PSp[j],DA=DAp[j],PSCAL = PSCALp[j], SP=SPp[j],ZONE=ZONAp[j], Model=4)
      DBHp1[j]<-DBHp[j]+(Gest[j]/10)
    }
    
    #NEW COVARIATES
    source('covariates.R')
    COVp<-covariates(ID,areap,SPp,DBHp1,ZONAp)
    data.temp<-COVp#merge(data.proy, COVp, by = 'ID')
    DBHp<-data.temp$DBH
    BALcp<-data.temp$BALc
    SDIp<-data.temp$SDI
    Ap<-Ap+1# new age
    DAp<-DAp+1# new dominant age
    PSCALp<-data.temp$PScal
    NHAp<-data.temp$NHA
    BAp<-data.temp$BA
    QDp<-data.temp$QD
  }
  plot.proy<-data.frame(data.temp,Ap,DAp)
  final.proy<-bind_rows(final.proy,plot.proy)
}