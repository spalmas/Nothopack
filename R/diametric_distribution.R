#' Diametric distribution calculator
#'
#' \code{diam_distr} Calculates the diametric distributino
#'
#' @param AB the stand total basal area
#' @param QD the stand total quadratic diameter
#' @param NHA total number of trees
#' @param NHAN The number of nothofagus trees per hectare
#' @param PNHAN proportion of nothofagus trees
#' @param HD dominant height of the stand
#' @param QD_i the species quadratic diameter (1 Rauli, 2 Roble, 3 Coigue, 4 Others)
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' diam_distr(AB = 24, QD = 20, QD_1 = 20, NHAN = 763, PNHAN = 1, NHA = 763, HD = 13)

diam_distr <- function(AB = 0, QD = 0, NHA = 0, NHAN = 0, PNHAN = 0, HD = 0, QD_1 = 0, QD_2 = 0, QD_3 = 0, QD_4 = 0){

  A <- 5
  #Rauli parameters
  dmin_1 <- 5
  dmax_1 <- 60
  b0_B_1 <- -4.85344986
  b1_B_1 <-  1.03816837
  b2_B_1 <- -0.01755607
  b3_B_1 <- 11.88124162
  b0_C_1 <-  3.41813315
  b1_C_1 <-  0.46054858
  b2_C_1 <- -0.42701842

  #Roble parameters
  dmin_2 <- 5
  dmax_2 <- 60
  b0_B_2 <- -8.83509885
  b1_B_2 <-  1.06360951
  b2_B_2 <-  0.99954695
  b0_C_2 <-  3.57525492
  b1_C_2 <-  0.39042726
  b2_C_2 <- -0.36299573
  b3_C_2 <- -4.83178811

  #Coigue parameters
  dmin_3 <- 5
  dmax_3 <- 60
  b0_B_3 <- -6.57263098
  b1_B_3 <-  1.09071507
  b0_C_3 <-  5.72487482
  b1_C_3 <-  0.64094474
  b2_C_3 <- -0.64908497
  b3_C_3 <- -9.04444521

  #Others
  dmin_4 <- 5
  dmax_4 <- 60
  b0_B_4 <- -2.06868340
  b1_B_4 <-  0.68162016
  b2_B_4 <- -0.00029245
  b0_C_4 <-  2.15124820
  b1_C_4 <-  0.29765454
  b2_C_4 <- -0.22722466

  #Each species equations

  B1 <- b0_B_1 + b1_B_1*QD_1 + b2_B_1*PNHAN + b3_B_1/AB
  C1 <- b0_C_1 + b1_C_1*B1 + b2_C_1*QD_1

  RS <- 100 * sqrt(10000/NHA)/HD
  B2 <- b0_B_2 + b1_B_2*QD_2 + b2_B_1*log(RS)
  C2 <- b0_C_2 + b1_C_2*B2 + b2_C_2*QD_2 + b3_C_2/QD

  B3 <- b0_B_3 + b1_B_3*QD_3
  C3 <- b0_C_3 + b1_C_3*B3 + b2_C_3*QD_3 + b3_C_3/QD

  B4 <- b0_B_4 + b1_B_4*QD_4 + b2_B_4*NHAN
  C4 <- b0_C_4 + b1_C_4*B4 + b2_C_4*QD_4

  #Probability
  P1 <- exp(-((dmin_1 - A)/B1)^C1) - exp(-((dmax_1 - A)/B1)^C1)
  P2 <- exp(-((dmin_2 - A)/B2)^C2) - exp(-((dmax_2 - A)/B2)^C2)
  P3 <- exp(-((dmin_3 - A)/B3)^C3) - exp(-((dmax_3 - A)/B3)^C3)
  P4 <- exp(-((dmin_4 - A)/B4)^C4) - exp(-((dmax_4 - A)/B4)^C4)

  return(list(P1 = P1, P2 = P2, P3 = P3, P4 = P4))
}


