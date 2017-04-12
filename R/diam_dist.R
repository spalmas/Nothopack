#' Generation of the diametric distribution for a given stand to each species/cohort
#'
#' \code{diam_distr} Generates the diametric distribution for a given stand for each of the
#' species/cohorts using the method of parameter recovery. The diameter classes are based on 
#' 1 cm.
#'
#' @param sp.table table with stand-level information by specie and total, columns are: 
#' SPECIES, N, BA, QD. Groups in SPECIES are  1:Rauli, 2:Roble, 3:Coigue, 4:Others, 0:Total.   
#' @param HD Dominant height (m) of dominant specie in the current stand
#' @param DOM.SP The dominant specie (1: Rauli, 2: Roble, 3: Coigue, 4:Mixed)
#' @param zone Growth zone (1, 2, 3, 4).
#' @param class Diameter class width (cm)
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
#' plot<-inputmodule(type='stand',zone=2,AD=28,HD=23.5,N=N,BA=BA)
#' Dd<-diam_dist(sp.table=plot$sp.table, HD=plot$HD,  
#'               DOM.SP=plot$DOM.SP, zone=plot$zone)
#' Dd[5,,]  # Total diameter distribution
#' # Ploting distribution for each specie
#' barplot(as.matrix(Dd[5,,5]), main='Diameter Distribution all species', xlab='DBH Class', beside=TRUE, col=4)

diam_dist <- function(sp.table=NA, HD=NA, DOM.SP=NA, zone=NA, class=5){

  vNHA <- sp.table$N
  vBA <- sp.table$BA
  vQD <- sp.table$QD
  
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
  
  maxD <- 90/class
  diam <- seq(from=5,to=90,by=class)   # Diameter classes

  for (j in 1:(length(diam)-1)){
    DBH_LL[j] <- diam[j]                 # cm
    DBH_UL[j] <- diam[j+1]               # cm
    Dclass[j] <- (diam[j]+diam[j+1])/2   # cm
    BAclass[j] <- (pi/4)*((Dclass[j])^2) # cm2
    Hclass[j] <- height_param(HD=HD, QD=QD, DBH=Dclass[j], dom_sp=DOM.SP, zone=zone)
    if (vNHA[1]==0) { Prob1[j]=0 
    } else { Prob1[j] <- exp(-((diam[j] - A)/B1)^C1) - exp(-((diam[j+1] - A)/B1)^C1) }
    if (vNHA[2]==0) { Prob2[j]=0 
    } else { Prob2[j] <- exp(-((diam[j] - A)/B2)^C2) - exp(-((diam[j+1] - A)/B2)^C2) }
    if (vNHA[3]==0) { Prob3[j]=0 
    } else { Prob3[j] <- exp(-((diam[j] - A)/B3)^C3) - exp(-((diam[j+1] - A)/B3)^C3) }
    if (vNHA[4]==0) { Prob4[j]=0 
    } else { Prob4[j] <- exp(-((diam[j] - A)/B4)^C4) - exp(-((diam[j+1] - A)/B4)^C4) }
     
    if (is.na(Prob1[j])) {Prob1[j] <- 0}
    if (is.na(Prob2[j])) {Prob2[j] <- 0}
    if (is.na(Prob3[j])) {Prob3[j] <- 0}
    if (is.na(Prob4[j])) {Prob4[j] <- 0}
  }
  Hclass <- round(Hclass,2)
  
  N.sp1 <- round(vNHA[1]*Prob1,2)
  N.sp2 <- round(vNHA[2]*Prob2,2)
  N.sp3 <- round(vNHA[3]*Prob3,2)
  N.sp4 <- round(vNHA[4]*Prob4,2)
  N.total <- N.sp1 + N.sp2 + N.sp3 + N.sp4 
  
  # Adjusting N.total with NHA
  K1 <- sum(N.sp1)/vNHA[1]
  K2 <- sum(N.sp2)/vNHA[2]
  K3 <- sum(N.sp3)/vNHA[3]
  K4 <- sum(N.sp4)/vNHA[4]
  N.sp1 <- round(N.sp1/K1,2)
  N.sp2 <- round(N.sp2/K2,2)
  N.sp3 <- round(N.sp3/K3,2)
  N.sp4 <- round(N.sp4/K4,2)
  N.total <- N.sp1 + N.sp2 + N.sp3 + N.sp4 

  BA.sp1 <- round(BAclass*N.sp1/(100^2),2)  # cm2/ha
  BA.sp2 <- round(BAclass*N.sp2/(100^2),2)  # cm2/ha
  BA.sp3 <- round(BAclass*N.sp3/(100^2),2)  # cm2/ha
  BA.sp4 <- round(BAclass*N.sp4/(100^2),2)  # cm2/ha
  BA.total <- BA.sp1 + BA.sp2 + BA.sp3 + BA.sp4 
  
  # Adjusting BA.total with BA
  K1 <- sum(BA.sp1)/vBA[1]
  K2 <- sum(BA.sp2)/vBA[2]
  K3 <- sum(BA.sp3)/vBA[3]
  K4 <- sum(BA.sp4)/vBA[4]
  BA.sp1 <- round(BA.sp1/K1,2)
  BA.sp2 <- round(BA.sp2/K2,2)
  BA.sp3 <- round(BA.sp3/K3,2)
  BA.sp4 <- round(BA.sp4/K4,2)
  BA.total <- BA.sp1 + BA.sp2 + BA.sp3 + BA.sp4 

  #print(sum(BA.total))
  #print(BA)
  
  r_names<-c('DBH_ll','DBH_ul','D_class','H_class','N','BA')
  DDist<-array(data=NA, dim=c(5,(length(diam)-1),6),
               dimnames=list(c(1:5),c(1:(length(diam)-1)),r_names))
  DDist[1,,]<-cbind(DBH_LL,DBH_UL,Dclass,Hclass,N.sp1,BA.sp1)  #1: Rauli
  DDist[2,,]<-cbind(DBH_LL,DBH_UL,Dclass,Hclass,N.sp2,BA.sp2)  #2: Roble
  DDist[3,,]<-cbind(DBH_LL,DBH_UL,Dclass,Hclass,N.sp3,BA.sp3)  #3: Coigue
  DDist[4,,]<-cbind(DBH_LL,DBH_UL,Dclass,Hclass,N.sp4,BA.sp4)  #4: Others
  DDist[5,,]<-cbind(DBH_LL,DBH_UL,Dclass,Hclass,N.total,BA.total)  #0: Total  

  r_names<-c('A','B','C','D','E','F')
  
  return(stand.table=DDist)

}

# Note: - Need to check that PNHAN goes from 0 to 1, and not 0 to 100
#'- Need to check that HD is Dominant height (m) of dominant specie in the stand
#' dom_sp and zone should be specified
