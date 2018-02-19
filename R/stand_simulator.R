#' Simulates whole-stand for the complete stand using the BAN and BA99 modules
#'
#' \code{stand_simulator} Simulates plot level growth, mortality (and recruitment) of a given stand
#' requiring stand-level parameters coming from core module.
#' Simulations are done using stand-level models starting from intial age (AD0)
#' until final age (ADF) in increments of 1 year.
#'
#' @param core.stand a stand list corrected from core_module
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' #Example 1. Starting from known stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' stand.input<-input_module(ZONE=2, AD=28, HD=18.5, AF=35, N=N, BA=BA, type='stand')
#' stand_simulator(core.stand=stand.input)
#'
#' #Example 2. Generating a diameter distribution
#' BA<-c(1.09,38.92,0,0.31)
#' N<-c(60,780,0,80)
#' input<-input_module(type='stand',ZONE=2,AD=28,HD=15.5,N=N,BA=BA,AF=35,V_model=2,ddiam=TRUE, comp=FALSE)
#' core.stand<-core_module(input = input)
#' core.stand$sp
#' core.stand$type
#' stand<-stand_simulator(core.stand = core.stand)
#' results.stand<-core_module(input = stand$input)
#' results.stand$sp.table
#' results.stand$DDist[5,,]
#' stand$input$sim.stand
#'
#' #Example 3. Starting from known stand-level data
#' head(plot_example)
#' tree<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=35,tree.list=plot_example)
#' core.tree<-core_module(input=tree)
#' stand<-stand_simulator(core.stand = core.tree)
#' results.stand<-core_module(input = stand$input)
#' results.stand$sp.table
#' report(core.stand=results.stand)

stand_simulator <- function(core.stand=NULL){

  #Error with dominant species
  if (core.stand$DOM.SP == 4){
    stop('The stand is not dominated by Nothofagus. The simulator only works with Nothofagus dominated forests.')
  }

  #Errors with Age
  if(is.na(core.stand$AF)){
    print('There is no Final Age (AF) for simulation ')
  }  else if (core.stand$AD >= core.stand$AF){
    print('The Final age (AF) of simulation should be larger than the Initial Age (AD)')
  }

  core.stand$sp.table$PSP.NHA<-core.stand$sp.table[,2]/sum(core.stand$sp.table[1:3,2])
  core.stand$sp.table$PSP.BA<-core.stand$sp.table[,3]/sum(core.stand$sp.table[1:3,3])
  #print(core.stand$sp.table)

  SI <- core.stand$SI
  DOM.SP <- core.stand$DOM.SP
  ZONE <- core.stand$ZONE
  HD0 <- core.stand$HD

  NHAN0 <- core.stand$sp.table$N[1:3] %>% sum(na.rm = TRUE)
  NHA0 <- core.stand$sp.table$N[5]
  NHA990 <- NHA0-NHAN0
  QD0 <- core.stand$sp.table$QD[5]
  BAN0 <- core.stand$sp.table$BA[1:3] %>% sum(na.rm = TRUE)
  BA990 <- core.stand$sp.table$BA[4]

  PBAN0 <- core.stand$PBAN
  PBAN1 <- PBAN0
  PNHAN0 <- core.stand$PNHAN
  PNHAN1 <- PNHAN0

  # Storing (internally) some output
  BA0 <- BAN0+BA990
  data.sim<-data.frame(AGE=core.stand$AD,HD0,NHA0,QD0,BA0,NHAN0,NHA990,BAN0,BA990,PBAN0,PNHAN0,SI)

  #Yearly simulations
  for (y in (core.stand$AD + 1):core.stand$AF){

    NHA1 <- NHAmodule(NHA0=NHA0, QD0=QD0, NHA_model=core.stand$NHA_model)   #Estimates new number of trees
    BAN1 <- BANmodule(BAN0 = BAN0, AD0=y, SI=SI, NHA0=NHA0, NHA1=NHA1, PBAN0=PBAN0, PBAN1=PBAN1, projection=TRUE)$BAN1   #projects new basal area (needs to change)
    PNHAN1 <- exp(-7.13684+10.29084*PBAN1-0.01404*y)/(1+exp(-7.13684+10.29084*PBAN1-0.01404*y)) # Important change
    PNHAN1 <- PNHAN0 # Important change
    BA991 <- BA99module(BA990=BA990, AD0=y, PNHAN0=PNHAN0, PNHAN1=PNHAN1, PBAN0=PBAN0, PBAN1=PBAN1, projection=TRUE)$BA991   #projects new basal area (needs to change)
    BA1 <- BAN1 + BA991 #Finds total new Basal Area
    PBAN1 <- BAN1/BA1
    NHAN1 <- NHA1*PNHAN1
    NHA991 <- NHA1*(1-PNHAN1)

    QD1 <- get_stand(BA=BA1, N=NHA1)   #New quadratic diameter
    HD1 <- get_site(DOM.SP=DOM.SP, ZONE=ZONE, SI=SI, AD=y)   #New dominant height

    #Variable replacement
    NHA0 <- NHA1
    BAN0 <- BAN1
    BA990 <- BA991
    BA0 <- BA1
    QD0 <- QD1
    HD0 <- HD1

    data.sim<-rbind(data.sim,c(y,HD1,NHA1,QD1,BA1,NHAN1,NHA991,BAN1,BA991,PBAN1,PNHAN1,SI))

  }

  #sp.table is the final stand values. At the end of the simulation
  sp.table<-matrix(data=0,nrow=5,ncol=4)
  sp.table[,1]<-c(seq(1:4),0)
  sp.table[1,2]<-core.stand$sp.table[1,5]*NHAN1
  sp.table[2,2]<-core.stand$sp.table[2,5]*NHAN1
  sp.table[3,2]<-core.stand$sp.table[3,5]*NHAN1
  sp.table[4,2]<-NHA991
  sp.table[5,2]<-NHA1

  sp.table[1,3]<-core.stand$sp.table[1,6]*BAN1
  sp.table[2,3]<-core.stand$sp.table[2,6]*BAN1
  sp.table[3,3]<-core.stand$sp.table[3,6]*BAN1
  sp.table[4,3]<-BA991
  sp.table[5,3]<-BA1

  sp.table[1,4]<-get_stand(BA=sp.table[1,3], N=sp.table[1,2])
  sp.table[2,4]<-get_stand(BA=sp.table[2,3], N=sp.table[2,2])
  sp.table[3,4]<-get_stand(BA=sp.table[3,3], N=sp.table[3,2])
  sp.table[4,4]<-get_stand(BA=sp.table[4,3], N=sp.table[4,2])
  sp.table[5,4]<-get_stand(BA=sp.table[5,3], N=sp.table[5,2])

  sp.table<-data.frame(sp.table)
  colnames(sp.table)<-c('SPECIE','N','BA','QD')

  # Calculation of SDI_percentage
  b1 <- 1.4112
  SDI <- NHA1*(sp.table[5,4]/25.4)^b1  # Good for all DOM.SP
  if (core.stand$DOM.SP==1 | core.stand$DOM.SP==4) {
    SDIMax <- 1155.0 # Rauli and Mixed
  }
  if (core.stand$DOM.SP==2) {
    SDIMax <- 908.8 # Roble
  }
  if (core.stand$DOM.SP==2) {
    SDIMax <- 1336.9   # Coigue
  }
  SDIP <- round(100*SDI/SDIMax,6)
  PBAN <- sum(sp.table[1:3,3])/(sp.table[5,3])
  PNHAN <- sum(sp.table[1:3,2])/(sp.table[5,2])

  output <- list(zone=ZONE, DOM.SP=DOM.SP, AD=core.stand$AF, HD=HD1, SI=SI,
                 SDIP=SDIP, PBAN=PBAN1, PNHAN=PNHAN1, AF=core.stand$AF,
                 area=core.stand$area, type=core.stand$type, ddiam=core.stand$ddiam,
                 comp=core.stand$comp, NHA_model=core.stand$NHA_model, V_model=core.stand$V_model,
                 IADBH_model=core.stand$IADBH_model, sp.table=sp.table,
                 DDdist=NA, tree.list=NA, data.sim=data.sim)

  return(output)

}

# Note
# - We have prediction and projection.
# - It assumes you provide: HD-AD, and BA-N
# - Need to define if we add additional stand level parameters
# - Volume should not be calculated here, it shoud be done in core_module (with ddiam TRUE or FALSE)
# - There is a strong assumption that PBAN and PNHAN stays fixed ????????????? (option to change PNHAN but discrete)
# - The plot for volume increments to much (it is unrealistic, probably projection BA is wrong!)
# - There needs to be a match when converting back to diameter distribution and when the BA is predicted

