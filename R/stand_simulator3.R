#' Simulates whole-stand for the complete stand using the BAN and BA99 modules
#'
#' \code{stand_simulator3} Simulates plot level growth, mortality (and recruitment) of a given stand
#' requiring stand-level parameters that combine all species. Simulations are done using stand-level
#' models starting from intial age (AD0) until final age (ADF) in increments of 1 year.
#' Differs from the original_stand simulator because it simulates the BAN and BA99
#' in different steps, opposed to simulate total BA of the stand.
#'
#' @param vBA vector de Area Basal para la especies (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param vN vector de numero de arboles por hectarea para la especies (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param zone Growth zone of the corresponding stand
#' @param HD0 dominant height (m) of current stand at age AD0
#' @param AD0 initial dominant age (year) to start simulations
#' @param ADF final dominant age (year) to finish simulations
#' @param Nmodel Number of fitted model for N estimation (1:Original Reineke, 2:New Reineke)
#' @param BAmodel Number of fitted model for BA to use for estimation (1:non-linear fit, 2:linear fit).
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
stand_simulator3 <- function(vBA=NA, vNHA=NA,
                            zone=NA, HD0=NA, AD0=NA,
                            ADF=30, Nmodel=1,
                            dom_sp = 1,
                            PBAN = 0.8, PNHAN = 0.8){

  BA0 <- sum(vBA, na.rm = TRUE)    #Adding all the BA
  NHA0 <- sum(vNHA, na.rm = TRUE)    #Adding all the number of trees

  #if the input is in vector form with all the species
  if (length(vBA) == 4){
    BAN0 <- sum(vBA[1:3], na.rm = TRUE)   #Basal Area of Nothofagus
    vPBA <- vBA/BA0   #Proportion of Basal Area per species
    dom_sp <- which(vPBA==max(vPBA))    #Dominant species

    NHAN0 <- sum(vNHA[1:3], na.rm = TRUE)  #Number of Nothofagus
    vPNHA <- vNHA/NHA0   #Proportion of Number of trees per species. This is a vector of 2

    PBAN <- BAN0 / BA0   #Estimating proportion of Nothofagus   #Does not change over time
    PNHAN <- NHAN0 / NHA0  #Also does not change over time?

  #if the input is (Nothofagus, Others)
  } else if (length(vBA) == 2){
    BAN0 <- sum(vBA[1], na.rm = TRUE)   #Basal Area of Nothofagus
    NHAN0 <- sum(vNHA[1], na.rm = TRUE)  #Number of Nothofagus

    PBAN <- BAN0 / BA0   #Estimating proportion of Nothofagus   #Does not change over time
    PNHAN <- NHAN0 / NHA0  #Also does not change over time?
    warning('Simulating with Rauli as Dominant Species and PNHAN and PBAN of 0.8 . You can change htis in the function call.',
            call. = FALSE)

    #if the input is Total Basal Area and Total number of Trees
  } else if (length(vBA) == 1){
    stop('This is not a valid input. You are a dummy.')
    #BAN0 <- NA   #Basal Area of Nothofagus
    #NHAN0 <- NA  #Number of Nothofagus

    #warning('Simulating with Rauli as Dominant Species and PNHAN and PBAN of 0.8 . You can change htis in the function call.',
    #        call. = FALSE)

  } else {
    stop('This is not a valid input. You are a dummy.')
  }

  if (dom_sp == 4){
    stop('The stand is not dominated by Nothofagus and we dont have enough information for this simulation.')
  }

  BA990 <- BA0 - BAN0   #Basal Area of Other Species
  #Dd <- diam_dist(vBA=vBA, vNHA=vNHA, HD=HD0)    #firsst estimation of diametric distribution

  # Completing stand-level information
  SI <- get_site(dom_sp=dom_sp, zone=zone, HD=HD0, AD=AD0)
  QD0 <- get_stand(BA=BA0, N=NHA0)
  VOL0 <- Vmodule(BA=BA0, HD=HD0, PNHAN=PNHAN) #maybe it can be changed to volume with species specific equatoins?

  # Create a table to store results

  results <- data.frame(Age=AD0, NHA=NHA0, QD=QD0, HD=HD0, SI=SI,
                        BA=BA0, BAN=BAN0, BA99=BA990,
                        #BA1=vBA[1], BA2=vBA[2], BA3=vBA[3], BA4=vBA[4],
                        #NHA1=sum(Dd$N.sp1), NHA2=sum(Dd$N.sp2), NHA3=sum(Dd$N.sp3), NHA4=sum(Dd$N.sp4),
                        VOL=VOL0)

  if (AD0 == ADF){
    return(results)
  }

  for (y in (AD0+1):ADF){
    NHA1 <- Nmodule(NHA0=NHA0, QD0=QD0, model=Nmodel)   #Estimates new number of trees
    BAN1 <- BANmodule(BAN0 = BAN0, AD0=AD0,  IS=IS, NHA0=NHA0, NHA1=NHA1, PBAN0 = PBAN, PBAN1 = PBAN, projection=TRUE)$BAN1   #projects new basal area (needs to change)
    BA991 <- BA99module(BA990=BA990, AD0=AD0, PNHAN0=PNHAN, PNHAN1=PNHAN, PBAN0 = PBAN, PBAN1=PBAN, projection=TRUE)$BA991   #projects new basal area (needs to change)
    BA1 <- BAN1 + BA991 #Finds total new Basal Area
    QD1 <- get_stand(BA=BA1, N=NHA1)   #New quadratic diameter
    HD1 <- get_site(dom_sp=dom_sp, zone=zone, SI=SI, AD=y)   #New dominant height
    VOL1 <- Vmodule(BA=BA1, HD=HD1, PNHAN=PNHAN)  # Note that PropNN stays fixed!

    #Update the Number and BA vectors
    #vN1 <- table(factor(sample(x = c(1,2,3,4), size=NHA1, replace=TRUE, prob=vPNHA),
    #                        levels = c(1,2,3,4))) # A list of n.trees species
    #vBA1 <- BA1*vPBA   #The basal area is just proportional to the Proportoin of Nothofagus?

    #Update diametric distibution
    #Dd <- diam_dist(vBA=vBA1, vN=vN1, HD=HD1)
    #Dd <- RECRUITmodule(vBA=vBA1, vN=vN1, HD=HD1)  #What is this?

    #Update vN1 con los resultados de la RECRUITmodule
    #vNHA1 <- c(sum(Dd$N.sp1), sum(Dd$N.sp2), sum(Dd$N.sp3), sum(Dd$N.sp4))

    #Updates proportion of Nothofagus
    #PABN1 <- sum(vBA1[1:3], na.rm = TRUE)/BA0

    results <- rbind(results, c(y, NHA1, QD1, HD1, SI,
                                BA1, BAN1, BA991,
                                #vBA1[1], vBA1[2], vBA1[3], vBA1[4],
                                #sum(Dd$N.sp1), sum(Dd$N.sp2), sum(Dd$N.sp3), sum(Dd$N.sp4),
                                VOL1))  # in the same order as dataframe above!

    #Variable replacement
    NHA0 <- NHA1
    QD0 <- QD1
    BA0 <- BA1
    BAN0 <- BAN1
    BA990 <- BA991
    HD0 <- HD1
    VOL0 <- VOL1
  }

  return(results)
}

# Note
# - We have prediction and projection.
# - It assumes you provide: HD-AD, and BA-N
# - Need to define if we add additional stand level parameters
# - There is a strong assumption that PropNN and PropBA stays fixed
# - The plot for volume increments to much (it is unrealistic, probably projection BA is wrong!)
# - There needs to be a match when converting back to diameter distribution and when the BA is predicted
