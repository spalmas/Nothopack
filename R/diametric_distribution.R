#' Generation of the diametric distribution for a given stand
#'
#' \code{diam_distr} Generates the diametric distribution for a given stand using the
#' methods of parameter recovery. The probabilities are calculated by diameter classes of 
#' 5 cm intervals. Hence, 5-10, 10-15, 15-20, etc. 
#'
#' @param BA Basal area (m2/ha) of the stand
#' @param QD Quadratic diameter (cm) of the stand 
#' @param NHA Number of trees per hectare (trees/ha) for all tree species
#' @param NHAN Number of trees per hectare (trees/ha) for only Nothofagus species
#' @param PNHAN Proportion of trees per hectare of Nothofagus (values range from 0 to 1)
#' @param HD Dominant height (m) of dominant specie in the stand
#' @param QD_1 Quadratic diameter for Rauli (sp=1)
#' @param QD_2 Quadratic diameter for Roble (sp=2)
#' @param QD_3 Quadratic diameter for Coigue (sp=3)
#' @param QD_4 Quadratic diameter for Others (sp=4)
#' 
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065, Chile
#'
#' @return A matrix of probabilities for each diameter class by increments 
#' of 5 cm, starting at 5 cm.
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' diam_distr(BA=24, QD=20, QD_1=20, NHAN=763, PNHAN=1, NHA=763, HD=13)

diam_distr <- function(BA=0, QD=0, NHA=0, NHAN=0, PNHAN=0, HD=0, QD_1=0, QD_2=0, QD_3=0, QD_4=0){

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
  B1 <- b0_B_1 + b1_B_1*QD_1 + b2_B_1*PNHAN + b3_B_1/BA
  C1 <- b0_C_1 + b1_C_1*B1 + b2_C_1*QD_1
  # Roble
  RS <- 100 * sqrt(10000/NHA)/HD
  B2 <- b0_B_2 + b1_B_2*QD_2 + b2_B_1*log(RS)
  C2 <- b0_C_2 + b1_C_2*B2 + b2_C_2*QD_2 + b3_C_2/QD
  # Coigue
  B3 <- b0_B_3 + b1_B_3*QD_3
  C3 <- b0_C_3 + b1_C_3*B3 + b2_C_3*QD_3 + b3_C_3/QD
  # Others
  B4 <- b0_B_4 + b1_B_4*QD_4 + b2_B_4*NHAN
  C4 <- b0_C_4 + b1_C_4*B4 + b2_C_4*QD_4

  # Calculation of the probability of NHA per diameter class, from 5 to 80 cm
  Prob1 <- matrix(data=NA,nrow=0,ncol=6)
  diam <- seq(5,80,5)
  for (j in 1:15){
    Prob1[j] <- exp(-((diam[j] - A)/B1)^C1) - exp(-((diam[j+1] - A)/B1)^C1)
  }
  
  Prob1
  plot(Prob1)
  sum(Prob1)
  
  #P1 <- exp(-((dmin_1 - A)/B1)^C1) - exp(-((dmax_1 - A)/B1)^C1)
  #P2 <- exp(-((dmin_2 - A)/B2)^C2) - exp(-((dmax_2 - A)/B2)^C2)
  #P3 <- exp(-((dmin_3 - A)/B3)^C3) - exp(-((dmax_3 - A)/B3)^C3)
  #P4 <- exp(-((dmin_4 - A)/B4)^C4) - exp(-((dmax_4 - A)/B4)^C4)

  }
  
  return(list(P1=P1, P2=P2, P3=P3, P4=P4))
}

# Note: - Need to check that PNHAN goes from 0 to 1, and not 0 to 100
#'- Need to check that HD is Dominant height (m) of dominant specie in the stand