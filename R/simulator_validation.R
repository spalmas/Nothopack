#' Predicts volume for individual trees
#'
#' This code compares the result of the simulator with the ENSAYOS measurements
#' This is not a function but a normal script that can be run from the console
#'

source('startup.R')

#adding columns to right of the ensayos dataset
ensayos.data[,(ncol(ensayos.data) + 1):(ncol(ensayos.data) + 6)] <- NA
colnames(ensayos.data)[(ncol(ensayos.data) -5):(ncol(ensayos.data))] <- c('y', 'N1_pred', 'BA1_pred', 'QD1_pred', 'HD1_pred', 'SI_pred')
#simulation of each row of the validation dataset r=3
for (r in 1:nrow(ensayos.data)){
  SIM <- stand_simulator(
    #SI <- get_site(dom_sp=dom_sp, zone=zone, HD=HD0, E=AD0)
    dom_sp = ensayos.data$dom_sp[r],
    zone = ensayos.data$zone[r],
    HD0 = ensayos.data$HD[r],
    AD0 = ensayos.data$EDAD1[r],
    BA0 = ensayos.data$BA1[r],
    N0 = ensayos.data$NHA1[r],
    ADF = ensayos.data$EDAD2[r],
    Nmodel = 1,
    BAmodel = 1)

  ensayos.data[r, (ncol(ensayos.data) + 1):(ncol(ensayos.data) + 6)] <- SIM[nrow(SIM),]
}




