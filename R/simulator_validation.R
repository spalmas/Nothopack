#' Predicts volume for individual trees
#'
#' This code compares the result of the simulator with the ENSAYOS measurements
#' This is not a function but a normal script that can be run from the console
#'

source('startup.R')

#simulation of each row of the validation dataset
for (row in 1:row(ensayos.data)){
  ensayos.data[(ncol(ensayos.data) + 1):(ncol(ensayos.data) + 6),row] <- stand_simulator(dom_sp = ensayos.data$Dominant[row],
                  zone = ensayos.data$zone[row],
                  HD0 = ensayos.data$HD[row],
                  AD0 = ensayos.data$EDAD1[row],
                  BA0 = ensayos.data$BA1[row],
                  N0 = ensayos.data$NHA1[row],
                  ADF = ensayos.data$EDAD2[row],
                  Nmodel = 1,
                  BAmodel = 1)
}


