#' Predicts volume for individual trees
#'
#' This code compares the result of the simulator with the ENSAYOS measurements
#' This is not a function but a normal script that can be run from the console
#'
source('startup2.R')

#lets delete all plots where there is dominant speices is mixed (4)
ensayos.data <- ensayos.data[!ensayos.data$dom_sp ==4,]

#adding columns to right of the ensayos dataset
ensayos.data[,(ncol(ensayos.data) + 1):(ncol(ensayos.data) + 6)] <- NA
colnames(ensayos.data)[(ncol(ensayos.data) -5):(ncol(ensayos.data))] <- c('y', 'NHA1_pred', 'BA1_pred', 'QD1_pred', 'HD1_pred', 'SI_pred', 'VOL1_pred')

#simulation of each row of the validation dataset r=3
for (r in 1:nrow(ensayos.data)){
  SIM <- stand_simulator(
    dom_sp = ensayos.data$dom_sp[r],
    zone = ensayos.data$zone[r],
    HD0 = ensayos.data$HD1[r],
    AD0 = ensayos.data$EDAD0[r],
    BA0 = ensayos.data$BA0[r],
    N0 = ensayos.data$NHA0[r],
    ADF = ensayos.data$EDAD1[r],
    Nmodel = 1,
    BAmodel = 1)

  ensayos.data[r, (ncol(ensayos.data) - 5):(ncol(ensayos.data))] <- SIM[nrow(SIM),]
}

#predicted vs observed BA
plot(ensayos.data$BA1, ensayos.data$BA1_pred)
abline(a=0, b=1)

ensayos.data['BAdif'] <- ensayos.data$BA1 - ensayos.data$BA1_pred
hist(ensayos.data$BAdif)
plot(ensayos.data$Delta.ANHO, ensayos.data$BAdif)
abline(a=0, b=0)

#predicted vs observed NHA
plot(ensayos.data$NHA1, ensayos.data$NHA1_pred)
abline(a=0, b=1)

ensayos.data['NHAdif'] <- ensayos.data$NHA1 - ensayos.data$NHA1_pred
plot(ensayos.data$Delta.ANHO, ensayos.data$NHAdif)
abline(a=0, b=0)

#predicted vs observed DQ
plot(ensayos.data$QD1, ensayos.data$QD1_pred)
abline(a=0, b=1)

ensayos.data['QDdif'] <- ensayos.data$QD1 - ensayos.data$QD1_pred
plot(ensayos.data$Delta.ANHO, ensayos.data$QDdif)
abline(a=0, b=0)

#difference of years and prediction difference

