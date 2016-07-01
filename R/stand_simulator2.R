#' Simulates whole-stand for the complete stand
#'
#' \code{stand_simulator2} Simulates plot level growth, mortality (and recruitment) of a given stand
#' requiring stand-level parameters that combine all species. Simulations are done using stand-level
#' models starting from intial age (AD0) until final age (ADF) in increments of 1 year.
#' This version keeps track of the diameter distribution of the species and adds the funcionality
#' of the recruitment.
#'
#' @param vBA vector de Area Basal para la especies (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param vN vector de numero de arboles por hectarea para la especies (1:Rauli, 2:Roble, 3:Coigue, 4:Others or Mixed)
#' @param zone Growth zone of the corresponding stand
#' @param HD0 dominant height (m) of current stand ata age AD0
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
#' sims <- stand_simulator2(vBA=c(20,4,0,0), vN=c(650,113,0,0), zone = 1, HD0=18.45, AD0 = 20, ADF = 40, Nmodel = 1, BAmodel = 1)
#' sims
#' plot_results(sims)
#'
stand_simulator2 <- function(vBA=NA, vN=NA,
                            zone=NA, HD0=NA, AD0=NA,
                            ADF=80, Nmodel=1, BAmodel=1){

  dom_sp <- which(vBA==max(vBA))    #Dominant species

  if (dom_sp == 9){
    stop('The stand is not dominated by Nothofagus and we dont have enough information for this simulation.')
  }

  BA0 <- sum(vBA, na.rm = TRUE)    #Adding all the BA
  N0 <- sum(vN, na.rm = TRUE)    #Adding all the number of trees
  PropNN <- sum(vBA[1:3], na.rm = TRUE)/BA0   #Estimating proportion of Nothofagus
  PropNSpecies <- vN/sum(vN, na.rm = TRUE)
  PropBASpecies <- vBA/sum(vBA, na.rm = TRUE)
  Dd <- diam_distr(vBA=vBA, vN=vN, HD=HD0)    #firsst estimation of diametric distribution

  # Completing stand-level information
  SI <- get_site(dom_sp=dom_sp, zone=zone, HD=HD0, AD=AD0)
  QD0 <- get_stand(BA=BA0, N=N0)
  VOL0 <- Vmodule(BA=BA0, HD=HD0, PropNN=PropNN) #maybe it can be changed to volume with species specific equatoins?

  # Create a table to store results
  results <- data.frame(Age=AD0, N=N0, BA=BA0,QD=QD0, HD=HD0, SI=SI, VOL=VOL0,
                        N1=sum(Dd$N.sp1), N2=sum(Dd$N.sp2), N3=sum(Dd$N.sp3), N4=sum(Dd$N.sp4),
                        BA1=vBA[1], BA2=vBA[2], BA3=vBA[3], BA4=vBA[4])
  if (AD0 == ADF){
    return(results)
  }

  for (y in (AD0+1):ADF){
    N1 <- Nmodule(N0=N0, QD0=QD0, model=Nmodel)   #Estimates new number of trees
    BA1 <- BAmodule(AD0=AD0, HD0=HD0, N0=N0, BA0=BA0, model=BAmodel, projection=TRUE)$BA1   #projects new basal area (needs to change)
    QD1 <- get_stand(BA=BA1, N=N1)   #New quadratic diameter
    HD1 <- get_site(dom_sp=dom_sp, zone=zone, SI=SI, AD=y)   #New dominant height
    VOL1 <- Vmodule(BA=BA1, HD=HD1, PropNN=PropNN)  # Note that PropNN stays fixed!

    #Update the Number and BA vectors
    vN1 <- table(factor(sample(x = c(1,2,3,4), size=N1, replace=TRUE, prob=PropNSpecies),
                            levels = c(1,2,3,4))) # A list of n.trees species
    vBA1 <- BA1*PropBASpecies   #The basal area i sjust proportional to the Proportoin of Nothofagus?

    #Update diametric distibution
    Dd <- diam_distr(vBA=vBA1, vN=vN1, HD=HD1)
    Dd <- RECRUITmodule(vBA=vBA1, vN=vN1, HD=HD1)

    #Update vN1 con los resultados de la RECRUITmodule
    vN1 <- c(sum(Dd$N.sp1), sum(Dd$N.sp2), sum(Dd$N.sp3), sum(Dd$N.sp4))

    #Updates proportion of Nothofagus
    PropNN <- sum(vBA1[1:3], na.rm = TRUE)/BA0

    results <- rbind(results, c(y, N1, BA1, QD1, HD1, SI, VOL1,
                                sum(Dd$N.sp1), sum(Dd$N.sp2), sum(Dd$N.sp3), sum(Dd$N.sp4),
                                vBA1[1], vBA1[2], vBA1[3], vBA1[4]))  # in the same order as dataframe above!

    # Variable replacement
    N0 <- N1
    QD0 <- QD1
    BA0 <- BA1
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
