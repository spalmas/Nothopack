#' Generation of the diametric distribution for a given stand
#'
#' \code{diam_distr} Generates the diametric distribution for a given stand using the
#' methods of parameter recovery. The probabilities are calculated by diameter classes of 
#' 5 cm intervals. Hence, 5-10, 10-15, 15-20, etc. 
#'
#' @param vBA vector of basal areas (m2/ha) for each of the species 
#' (1: Rauli, 2: Roble, 3: Coigue, 4: Others)
#' @param vN vector of number of trees per hectare (trees/ha) for each of the species
#' (1: Rauli, 2: Roble, 3: Coigue, 4: Others)
#' @param HD Dominant height (m) of dominant specie in the stand
#' 
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @return A matrix of probabilities for each diameter class by increments 
#' of 5 cm, starting at 5 cm.
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' # Example: Generation of distribution for 2 species (Rauli and Roble)
#' (Dd<-diam_distr(vBA=c(20,4,0,0), vN=c(650,113,0,0), HD=18.45))
#' # Ploting distribution for each specie
#' barplot(as.matrix(Dd[,3:6]), beside=TRUE)   
#' # Ploting distribution for sp 1 and 2 overlayed
#' barplot(Dd[,3], col=1)
#' barplot(Dd[,4], add=TRUE, col=4)

diam_distr <- function(vBA=NA, vN=NA, HD=NA){

  vQD <- c(get_stand(BA=vBA[1],N=vN[1]),get_stand(BA=vBA[2],N=vN[2]),
             get_stand(BA=vBA[3],N=vN[3]),get_stand(BA=vBA[4],N=vN[4]))
  BA <- sum(vBA)
  N <- sum(vN)
  QD <- get_stand(BA,N)
  N.Noth <- sum(vN[1:3])   # Total number of trees Nothofagus
  PN.Noth <- N.Noth/N # Proportion trees Nothofagus

  A <- 5  # minimum diamater for any distribution

  # Rauli parameters, sp=1
  b0_B_1 <- -4.85344986
  b1_B_1 <-  1.03816837
  b2_B_1 <- -0.01755607
  b3_B_1 <- 11.88124162
  b0_C_1 <-  3.41813315
  b1_C_1 <-  0.46054858
  b2_C_1 <- -0.42701842

  # Roble parameters, sp=2
  b0_B_2 <- -8.83509885
  b1_B_2 <-  1.06360951
  b2_B_2 <-  0.99954695
  b0_C_2 <-  3.57525492
  b1_C_2 <-  0.39042726
  b2_C_2 <- -0.36299573
  b3_C_2 <- -4.83178811

  #Coigue parameters, sp=3
  b0_B_3 <- -6.57263098
  b1_B_3 <-  1.09071507
  b0_C_3 <-  5.72487482
  b1_C_3 <-  0.64094474
  b2_C_3 <- -0.64908497
  b3_C_3 <- -9.04444521

  # Other sps parameters, sp=4 
  b0_B_4 <- -2.06868340
  b1_B_4 <-  0.68162016
  b2_B_4 <- -0.00029245
  b0_C_4 <-  2.15124820
  b1_C_4 <-  0.29765454
  b2_C_4 <- -0.22722466

  # Recover of diameter distribution parameters by species

  # Rauli
  B1 <- b0_B_1 + b1_B_1*vQD[1] + b2_B_1*PN.Noth + b3_B_1/BA
  C1 <- b0_C_1 + b1_C_1*B1 + b2_C_1*vQD[1]
  # Roble
  RS <- 100 * sqrt(10000/N)/HD
  B2 <- b0_B_2 + b1_B_2*vQD[2] + b2_B_1*log(RS)
  C2 <- b0_C_2 + b1_C_2*B2 + b2_C_2*vQD[2] + b3_C_2/QD
  # Coigue
  B3 <- b0_B_3 + b1_B_3*vQD[3]
  C3 <- b0_C_3 + b1_C_3*B3 + b2_C_3*vQD[3] + b3_C_3/QD
  # Others
  B4 <- b0_B_4 + b1_B_4*vQD[4] + b2_B_4*N.Noth
  C4 <- b0_C_4 + b1_C_4*B4 + b2_C_4*vQD[4]

  # Calculation of the probability of NHA per diameter class, from 5 to 80 cm
  DLlow <- matrix(data=0,nrow=0,ncol=6)
  DLup <- matrix(data=0,nrow=0,ncol=6)
  Prob1 <- matrix(data=0,nrow=0,ncol=6)
  Prob2 <- matrix(data=0,nrow=0,ncol=6)
  Prob3 <- matrix(data=0,nrow=0,ncol=6)
  Prob4 <- matrix(data=0,nrow=0,ncol=6)
  diam <- seq(5,80,5)
  for (j in 1:15){
    DLlow[j] <- diam[j]
    DLup[j] <- diam[j+1]
    Prob1[j] <- exp(-((diam[j] - A)/B1)^C1) - exp(-((diam[j+1] - A)/B1)^C1)
    Prob2[j] <- exp(-((diam[j] - A)/B2)^C2) - exp(-((diam[j+1] - A)/B2)^C2)
    Prob3[j] <- exp(-((diam[j] - A)/B3)^C3) - exp(-((diam[j+1] - A)/B3)^C3)
    Prob4[j] <- exp(-((diam[j] - A)/B4)^C4) - exp(-((diam[j+1] - A)/B4)^C4)
    if (is.na(Prob1[j])) {Prob1[j] <- 0} 
    if (is.na(Prob2[j])) {Prob2[j] <- 0} 
    if (is.na(Prob3[j])) {Prob3[j] <- 0} 
    if (is.na(Prob4[j])) {Prob4[j] <- 0} 
  }
  N.sp1 <- round(vN[1]*Prob1,2)
  N.sp2 <- round(vN[2]*Prob2,2)
  N.sp3 <- round(vN[3]*Prob3,2)
  N.sp4 <- round(vN[4]*Prob4,2)
    
  DDist<-data.frame(cbind(DLlow,DLup,N.sp1,N.sp2,N.sp3,N.sp4))
  return(DDist)
}

# Note: - Need to check that PNHAN goes from 0 to 1, and not 0 to 100
#'- Need to check that HD is Dominant height (m) of dominant specie in the stand
#'- Need to put class values in output