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
#' input<-inputmodule(type='stand',zone=1,AD=28,HD=23.5,N=N,BA=BA, AF = 40)
#' core.stand<-core_module(input = input)
#' stand_simulator(core.stand = core.stand)
#
#' #Example 2. Starting from known stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' plot<-inputmodule(type='stand',zone=1,AD=28,HD=23.5,N=N,BA=BA,AF=40)
#' core.stand<-core_module(input=plot$input)
#' stand_simulator(core.stand=core.stand$input)
#' stand_simulator(core.stand=core.stand$input)

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
    #NHAN1 <- NHA1 * PNHAN
    BAN1 <- BANmodule(BAN0 = BAN0, AD0=y, SI=core.stand$SI, NHA0=core.stand$sp.table$N[5], NHA1=NHA1, PBAN0 = core.stand$PBAN, PBAN1 = core.stand$PBAN, projection=TRUE)$BAN1   #projects new basal area (needs to change)
    #BAN1 <- BANmodule2(BAN0 = BAN0, AD0=y, SI=SI, NHAN0=NHAN0, NHAN1=NHAN1, PBAN0 = PBAN, PBAN1 = PBAN, projection=TRUE)$BAN1   #projects new basal area (needs to change)
    BA991 <- BA99module(BA990=core.stand$sp.table$BA[4], AD0=y, PNHAN0=core.stand$PNHAN, PNHAN1=core.stand$PNHAN, PBAN0 = core.stand$PBAN, PBAN1 = core.stand$PBAN, projection=TRUE)$BA991   #projects new basal area (needs to change)
    BA1 <- BAN1 + BA991 #Finds total new Basal Area
    QD1 <- get_stand(BA=BA1, N=NHA1)   #New quadratic diameter
    #QD1 <- get_stand(BA=BAN1, N=(NHA1*PNHAN))   #Quadratic diameter using only Nothofagus Data
    HD1 <- get_site(dom_sp=core.stand$DOM.SP, zone=core.stand$zone, SI=core.stand$SI, AD=y)   #New dominant height
    VOL1 <- Vmodule(BA=BA1, HD=HD1, PNHAN=core.stand$PNHAN)  # Note that PropNN stays fixed!

    #NHAN1 <- NHA1 * PNHAN
    NHAN1 <- NHA1 - core.stand$sp.table$N[4]    #If the NHA99 do not change over time
    NHA991 <- NHA1 * (1-core.stand$PNHAN)

    #Update the Number and BA vectors
    #Update proportion of Nothofagus

    # Adds simulation results to table  # in the same order as dataframe above!
    results <- rbind(results, c(y, QD1, HD1, core.stand$SI,
                                BA1, BAN1, BA991,
                                NHA1, NHAN1, NHA991,
                                #vBA1[1], vBA1[2], vBA1[3], vBA1[4],
                                #sum(Dd$N.sp1), sum(Dd$N.sp2), sum(Dd$N.sp3), sum(Dd$N.sp4),
                                VOL1))

    #Variable replacement
    core.stand$sp.table$N[5] <- NHA1
    core.stand$sp.table$QD[5] <- QD1
    core.stand$sp.table$BA[5] <- BA1
    BAN0 <- BAN1   #Needs to replace species BA in sp.stand!
    BA990 <- core.stand$sp.table$BA[4]
    HD0 <- core.stand$HD
    VOL0 <- core.stand$sp.table$VTHA[5]
  }

  #Estiamting time that the simulation took
  sim_time <- Sys.time() - core.stand$start_time

  return(list(sim_time = sim_time, type = 'stand_simulation', simulation = results) )
}

# Note
# - We have prediction and projection.
# - It assumes you provide: HD-AD, and BA-N
# - Need to define if we add additional stand level parameters
# - Volume should not be calculated here, it shoud be done in core_module (with ddiam TRUE or FALSE)
# - There is a strong assumption that PropNN and PropBA stays fixed
# - The plot for volume increments to much (it is unrealistic, probably projection BA is wrong!)
# - There needs to be a match when converting back to diameter distribution and when the BA is predicted
