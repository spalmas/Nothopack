#' Predicts volume for individual trees
#'
#' This code compares the result of the simulator with the ENSAYOS measurements
#' This is not a function but a normal script that can be run from the console
#'

source('startup.R')

stand_simulator(dom_sp = ensayos.data$Dominant, zone = ensayos.data$zone)

stand_simulator <- function(dom_sp=NA, zone=NA, HD0=NA, AD0=NA, BA0=NA, N0=NA, ADF=80, Nmodel=1, BAmodel=1){
