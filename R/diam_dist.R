#' Generation of the diametric distribution for a given stand to each species/cohort
#'
#' \code{diam_distr} Generates the diametric distribution for a given stand for each of the
#' species/cohorts using the method of parameter recovery. The diameter classes are based on 
#' specified class width.
#'
#' @param vBA vector of basal areas (m2/ha) for each of the species/cohort
#' (1:Rauli, 2:Roble, 3:Coigue, 4:Others, 0:Total)
#' @param vN vector of number of trees per hectare (trees/ha) for each of the species/cohort
#' (1:Rauli, 2:Roble, 3:Coigue, 4:Others, 0:Total)
#' @param vQD vector of quadratic diameters (cm) for each of the species/cohort
#' (1:Rauli, 2:Roble, 3:Coigue, 4:Others, 0:Total)
#' @param HD Dominant height (m) of dominant specie in the current stand
#' @param class Width of diameter class (default = 5 cm)
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065. Chile
#'
#' @return A matrix of probabilities for each specie/cohort by diameter classes on increments
#' of width specified (starting at 5 cm).
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' # Example: Generation of distribution from stand-level input data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' stand.matrix<-inputmodule(level='stand',zone=2,AD=25,HD=23.4,N=N,BA=BA)
#' (Dd<-diam_dist(vBA=stand.matrix$sd[,3], vNHA=stand.matrix$sd[,2], 
#'                vQD=stand.matrix$sd[,4], HD=stand.matrix$HD, class=5))
#' (Dd<-diam_dist(vBA=stand.matrix$sd[,3], vNHA=stand.matrix$sd[,2], 
#'                vQD=stand.matrix$sd[,4], HD=stand.matrix$HD, class=1))
#'                
#' # Ploting distribution for each specie
#' barplot(as.matrix(Dd$StandTable[,3:6]), beside=TRUE)
#' # Ploting distribution for sp 1 and 2 overlayed
#' barplot(Dd$StandTable[,4], col=1)
#' barplot(Dd$StandTable[,5], add=F, col=4)

diam_dist <- function(vBA=NA, vNHA=NA, vQD=NA, HD=NA, class=5){

  BA <- vBA[5]
  NHA <- vNHA[5]
  QD <- vQD[5]

  NHAN <- sum(vNHA[1:3])   # Total number of trees Nothofagus
  PNHAN <- NHAN/NHA        # Proportion trees Nothofagus

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
  B1 <- b0_B_1 + b1_B_1*vQD[1] + b2_B_1*PNHAN + b3_B_1/BA
  C1 <- b0_C_1 + b1_C_1*B1 + b2_C_1*vQD[1]
  # Roble
  RS <- 100 * sqrt(10000/NHA)/HD
  B2 <- b0_B_2 + b1_B_2*vQD[2] + b2_B_1*log(RS)
  C2 <- b0_C_2 + b1_C_2*B2 + b2_C_2*vQD[2] + b3_C_2/QD
  # Coigue
  B3 <- b0_B_3 + b1_B_3*vQD[3]
  C3 <- b0_C_3 + b1_C_3*B3 + b2_C_3*vQD[3] + b3_C_3/QD
  # Others
  B4 <- b0_B_4 + b1_B_4*vQD[4] + b2_B_4*NHAN
  C4 <- b0_C_4 + b1_C_4*B4 + b2_C_4*vQD[4]

  # Calculation of the probability of NHA per diameter class, from 5 to 80 cm

  DBH_LL <- matrix(data=0,nrow=0,ncol=1)
  DBH_UL <- matrix(data=0,nrow=0,ncol=1)
  Prob1 <- matrix(data=0,nrow=0,ncol=1)
  Prob2 <- matrix(data=0,nrow=0,ncol=1)
  Prob3 <- matrix(data=0,nrow=0,ncol=1)
  Prob4 <- matrix(data=0,nrow=0,ncol=1)
  Dclass <- matrix(data=0,nrow=0,ncol=1)
  BAclass <- matrix(data=0,nrow=0,ncol=1)
  Hclass <- matrix(data=0,nrow=0,ncol=1)
  Vclass <- matrix(data=0,nrow=0,ncol=1)
  diam <- seq(from=5,to=100,by=class)

  for (j in 1:19){
    DBH_LL[j] <- diam[j]                 # cm
    DBH_UL[j] <- diam[j+1]               # cm
    Dclass[j] <- (diam[j]+diam[j+1])/2   # cm
    BAclass[j] <- (pi/4)*((Dclass[j])^2) # cm2
    Hclass[j] <- height_param(HD=HD, QD=QD, DBH=Dclass[j], dom_sp=1, zone=1)  # dom_sp and zone should be specified
    Vclass[j] <- Vmodule_individual(dom_sp=1, zone=1, DBH=Dclass[j], HT=Hclass[j], blength=Hclass[j], Tmodel=1) # dom_sp, zone and Tmodel

    Prob1[j] <- exp(-((diam[j] - A)/B1)^C1) - exp(-((diam[j+1] - A)/B1)^C1)
    Prob2[j] <- exp(-((diam[j] - A)/B2)^C2) - exp(-((diam[j+1] - A)/B2)^C2)
    Prob3[j] <- exp(-((diam[j] - A)/B3)^C3) - exp(-((diam[j+1] - A)/B3)^C3)
    Prob4[j] <- exp(-((diam[j] - A)/B4)^C4) - exp(-((diam[j+1] - A)/B4)^C4)
    if (is.na(Prob1[j])) {Prob1[j] <- 0}
    if (is.na(Prob2[j])) {Prob2[j] <- 0}
    if (is.na(Prob3[j])) {Prob3[j] <- 0}
    if (is.na(Prob4[j])) {Prob4[j] <- 0}
  }
  N.sp1 <- round(vNHA[1]*Prob1,3)
  N.sp2 <- round(vNHA[2]*Prob2,3)
  N.sp3 <- round(vNHA[3]*Prob3,3)
  N.sp4 <- round(vNHA[4]*Prob4,3)

  BA.sp1 <- BAclass*N.sp1/(100^2)  # cm2/ha
  BA.sp2 <- BAclass*N.sp2/(100^2)  # cm2/ha
  BA.sp3 <- BAclass*N.sp3/(100^2)  # cm2/ha
  BA.sp4 <- BAclass*N.sp4/(100^2)  # cm2/ha

  VOL.sp1 <- Vclass*N.sp1
  VOL.sp2 <- Vclass*N.sp2
  VOL.sp3 <- Vclass*N.sp3
  VOL.sp4 <- Vclass*N.sp4


  DDist<-data.frame(cbind(DBH_LL,DBH_UL,Dclass,Hclass,
                          N.sp1,N.sp2,N.sp3,N.sp4,BA.sp1,BA.sp2,BA.sp3,BA.sp4,
                          VOL.sp1,VOL.sp2,VOL.sp3,VOL.sp4))


  DDistf = summarise(.data = DDist ,
                         N.sp1f = sum(N.sp1),
                         N.sp2f = sum(N.sp2),
                         N.sp3f = sum(N.sp3),
                         N.sp4f = sum(N.sp4),
                         BA.sp1f = sum(BA.sp1),
                         BA.sp2f = sum(BA.sp2),
                         BA.sp3f = sum(BA.sp3),
                         BA.sp4f = sum(BA.sp4),
                         VOL.sp1f = sum(VOL.sp1),
                         VOL.sp2f = sum(VOL.sp2),
                         VOL.sp3f = sum(VOL.sp3),
                         VOL.sp4f = sum(VOL.sp4)
  )

  N.sptot<-sum(DDistf$N.sp1f,DDistf$N.sp2f,DDistf$N.sp3f,DDistf$N.sp4f)
  BA.sptot<-sum(DDistf$BA.sp1f,DDistf$BA.sp2f,DDistf$BA.sp3f,DDistf$BA.sp4f)
  V.sptot<-sum(DDistf$VOL.sp1f,DDistf$VOL.sp2f,DDistf$VOL.sp3f,DDistf$VOL.sp4f)
  N.sp1f<-DDistf$N.sp1f*100/N.sptot
  N.sp2f<-DDistf$N.sp2f*100/N.sptot
  N.sp3f<-DDistf$N.sp3f*100/N.sptot
  N.sp4f<-DDistf$N.sp4f*100/N.sptot
  BA.sp1f<-DDistf$BA.sp1f*100/BA.sptot
  BA.sp2f<-DDistf$BA.sp2f*100/BA.sptot
  BA.sp3f<-DDistf$BA.sp3f*100/BA.sptot
  BA.sp4f<-DDistf$BA.sp4f*100/BA.sptot
  VOL.sp1f<-DDistf$VOL.sp1f*100/V.sptot
  VOL.sp2f<-DDistf$VOL.sp2f*100/V.sptot
  VOL.sp3f<-DDistf$VOL.sp3f*100/V.sptot
  VOL.sp4f<-DDistf$VOL.sp4f*100/V.sptot

  DDistp<-data.frame(cbind(N.sp1f,N.sp2f,N.sp3f,N.sp4f,BA.sp1f,BA.sp2f,BA.sp3f,BA.sp4f,
                          VOL.sp1f,VOL.sp2f,VOL.sp3f,VOL.sp4f))

  return(list(StandTable=DDist,Total=DDistf,Porc=DDistp))


}

# Note: - Need to check that PNHAN goes from 0 to 1, and not 0 to 100
#'- Need to check that HD is Dominant height (m) of dominant specie in the stand
#'- Need to put class values in output
#'# dom_sp and zone should be specified
