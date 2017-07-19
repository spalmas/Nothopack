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
#' BA<-c(1.09,38.92,0,0.31)
#' N<-c(60,780,0,80)
#' input<-inputmodule(type='stand',zone=2,AD=28,HD=15.5,N=N,BA=BA,AF=35,V_model=1)
#' core.stand<-core_module(input = input)
#' core.stand$sp
#' core.stand$type
#' stand<-stand_simulator(core.stand = core.stand)
#' results.stand<-core_module(input = stand$input)
#' results.stand$sp.table
#'
#' #Example 2. Generating a diameter distribution  # Problems for generiting diameter distribution!!!
#' BA<-c(1.09,38.92,0,0.31)
#' N<-c(60,780,0,80)
#' input<-inputmodule(type='stand',zone=2,AD=28,HD=15.5,N=N,BA=BA,AF=35,V_model=2,ddiam=TRUE)
#' core.stand<-core_module(input = input)
#' core.stand$sp
#' core.stand$type
#' stand<-stand_simulator(core.stand = core.stand)
#' results.stand<-core_module(input = stand$input)
#' results.stand$sp.table
#' results.stand$stand.table[5,,]
#'
#' #Example 3. Starting from known stand-level data
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' tree<-inputmodule(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=35,tree.list=plot)
#' attributes(tree)
#' head(tree$tree.list)
#' core.tree<-core_module(input=tree$input)
#' core.tree$sp.table
#' core.tree$type<-"stand"
#' stand<-stand_simulator(core.stand = core.tree)
#' results.stand<-core_module(input = stand$input)
#' results.stand$sp.table
#' report(core.stand=results.stand)
#'
stand_simulator <- function(core.stand = NULL){

  #Error with dominant species
  if (core.stand$DOM.SP == 4){
    stop('The stand is not dominated by Nothofagus. The simulator only works with Nothofagus dominated forests.')
  }

  #Errors with Age
  if(is.na(core.stand$AF)){
    print('There is no Final Age for simulation ')
  }  else if (core.stand$AD >= core.stand$AF){
    print('The Final age of simulation should be larger than the Initial Age')
  }

  core.stand$sp.table$PSP.NHA<-core.stand$sp.table[,2]/sum(core.stand$sp.table[1:3,2])
  core.stand$sp.table$PSP.BA<-core.stand$sp.table[,3]/sum(core.stand$sp.table[1:3,3])
  #print(core.stand$sp.table)

  BAN0 <- core.stand$sp.table$BA[1:3] %>% sum(na.rm = TRUE)
  NHAN <- core.stand$sp.table$N[1:3] %>% sum(na.rm = TRUE)

  # Create a table to store results
  results <- data.frame(Age = core.stand$AD,
                        QD = core.stand$sp.table$QD[5],
                        HD = core.stand$HD,
                        SI = core.stand$SI,
                        BA = core.stand$sp.table$BA[5],
                        BAN = BAN0,
                        BA99 = core.stand$sp.table$BA[4],
                        NHA = core.stand$sp.table$N[5],
                        NHAN = NHAN,
                        NHA99 = core.stand$sp.table$N[4],
                        VOL=core.stand$sp.table$VTHA[5]
                        )

  #Yearly simulations
  for (y in (core.stand$AD + 1) : core.stand$AF){
    NHA1 <- Nmodule(NHA0=core.stand$sp.table$N[5], QD0=core.stand$sp.table$QD[5], N_model=core.stand$N_model)   #Estimates new number of trees
    BAN1 <- BANmodule(BAN0 = BAN0, AD0=y, SI=core.stand$SI, NHA0=core.stand$sp.table$N[5], NHA1=NHA1, PBAN0 = core.stand$PBAN, PBAN1 = core.stand$PBAN, projection=TRUE)$BAN1   #projects new basal area (needs to change)
    BA991 <- BA99module(BA990=core.stand$sp.table$BA[4], AD0=y, PNHAN0=core.stand$PNHAN, PNHAN1=core.stand$PNHAN, PBAN0 = core.stand$PBAN, PBAN1 = core.stand$PBAN, projection=TRUE)$BA991   #projects new basal area (needs to change)
    BA1 <- BAN1 + BA991 #Finds total new Basal Area
    QD1 <- get_stand(BA=BA1, N=NHA1)   #New quadratic diameter
    HD1 <- get_site(dom_sp=core.stand$DOM.SP, zone=core.stand$zone, SI=core.stand$SI, AD=y)   #New dominant height

    NHAN1 <- NHA1 - core.stand$sp.table$N[4]    #If the NHA99 do not change over time
    NHA991 <- NHA1 * (1-core.stand$PNHAN)

    #Update the Number and BA vectors
    #Update proportion of Nothofagus

    # Adds simulation results to table  # in the same order as dataframe above!
    results <- rbind(results, c(y, QD1, HD1, core.stand$SI,
                                BA1, BAN1, BA991,
                                NHA1, NHAN1, NHA991))

    #Variable replacement
    core.stand$sp.table$N[5] <- NHA1
    core.stand$sp.table$QD[5] <- QD1
    core.stand$sp.table$BA[5] <- BA1
    BAN0 <- BAN1   #Needs to replace species BA in sp.stand!
    BA990 <- core.stand$sp.table$BA[4]
    HD0 <- core.stand$HD
  }

  #Estiamting time that the simulation took
  sim_time <- Sys.time() - core.stand$start_time

  # print(results) # might be interesting
  sp.table<-matrix(data=0,nrow=5,ncol=4)
  sp.table[,1]<-c(seq(1:4),0)
  sp.table[1,2]<-core.stand$sp.table[1,6]*NHAN1
  sp.table[2,2]<-core.stand$sp.table[2,6]*NHAN1
  sp.table[3,2]<-core.stand$sp.table[3,6]*NHAN1
  sp.table[4,2]<-NHA991
  sp.table[5,2]<-NHA1

  sp.table[1,3]<-core.stand$sp.table[1,7]*BAN1
  sp.table[2,3]<-core.stand$sp.table[2,7]*BAN1
  sp.table[3,3]<-core.stand$sp.table[3,7]*BAN1
  sp.table[4,3]<-BA991
  sp.table[5,3]<-BA1

  sp.table[1,4]<-get_stand(BA=sp.table[1,3], N=sp.table[1,2])
  sp.table[2,4]<-get_stand(BA=sp.table[2,3], N=sp.table[2,2])
  sp.table[3,4]<-get_stand(BA=sp.table[3,3], N=sp.table[3,2])
  sp.table[4,4]<-get_stand(BA=sp.table[4,3], N=sp.table[4,2])
  sp.table[5,4]<-get_stand(BA=sp.table[5,3], N=sp.table[5,2])

  sp.table<-data.frame(sp.table)
  colnames(sp.table)<-c('SPECIE','N','BA','QD')
  #print(sp.table)

  input <- list(zone=core.stand$zone, DOM.SP=core.stand$DOM.SP, AD=y,
                HD=HD1, SI=core.stand$SI, PBAN=core.stand$PBAN, PNHAN=core.stand$PNHAN, AF=y,
                area=core.stand$area, type=core.stand$type, ddiam=core.stand$ddiam, comp=core.stand$comp,
                N_model=core.stand$N_model, V_model=core.stand$V_model,
                IADBH_model=core.stand$IADBH_model, start_time=core.stand$start_time,
                sp.table=sp.table, stand.table=NA, tree.list=NA)

  return(list(input=input,results=results))

}

# Note
# - We have prediction and projection.
# - It assumes you provide: HD-AD, and BA-N
# - Need to define if we add additional stand level parameters
# - Volume should not be calculated here, it shoud be done in core_module (with ddiam TRUE or FALSE)
# - There is a strong assumption that PropNN and PropBA stays fixed
# - The plot for volume increments to much (it is unrealistic, probably projection BA is wrong!)
# - There needs to be a match when converting back to diameter distribution and when the BA is predicted
