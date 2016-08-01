#' Predicts volume for individual trees
#'
#' This code compares the result of the simulator with the ENSAYOS measurements
#' This is not a function but a normal script that can be run from the console
#'
#'

setwd("C:/Users/Sebastian/Google Drive/Nothofagus/Nothopack")

source('startup2.R')

prodal_ensayos <- read.csv('data/PRODAL_ENSAYOS.csv')
#the ensayos.data comes from the mortality.rmd code. The file was manually copied to the Nothopack/data folder

#adding a unique plot number to make it easier to find unique plots
prodal_ensayos['unique_plot'] <- paste(prodal_ensayos$ENSAYO, prodal_ensayos$PARCELA, prodal_ensayos$SUBPARC, sep = '-')

#start of a results dataframe
col.names <- c('subplot','dom_sp','zone',
              'EDAD1','EDAD2',
              'N0','BA0','HD0',
              'NF','BAF','HDF',
              'NF_pred','BAF_pred','HDF_pred')
results <- data.frame(matrix(ncol = length(col.names)) )
colnames(results) <- col.names

#simulation of each pair of data of each subplot
for (subplot in prodal_ensayos$unique_plot){
  subset_subplot <- prodal_ensayos[prodal_ensayos$unique_plot == subplot,]

  #do not simulate plots with only one measurement
  #with dominant species mixed or others
  #with unknown age
  if (nrow(subset_subplot) > 1 & all(subset_subplot$dom_sp %in% c(1,2,3))){

    #combinations of each anho of the subplot
    combinations <- combn(subset_subplot$EDAD2, m = 2)

    for (col in 1:ncol(combinations)){
      subset_subplot1<- subset_subplot[subset_subplot$EDAD2 == combinations[1,col],]
      subset_subplot2<- subset_subplot[subset_subplot$EDAD2 == combinations[2,col],]

      #SI <- get_site(dom_sp=dom_sp, zone=zone, HD=HD0, AD=AD0)

      sims <- stand_simulator(
        dom_sp = subset_subplot1$dom_sp,
        zone = subset_subplot1$ZONA,
        HD0 = subset_subplot1$HD,
        AD0 = subset_subplot1$EDAD2,
        BA0 = subset_subplot1$AB_Total,
        N0 = subset_subplot1$NHA,
        ADF = subset_subplot2$EDAD2,
        Nmodel = 1,
        BAmodel = 1)
    }

    fila <- c(subplot, subset_subplot1$dom_sp, subset_subplot1$ZONA,
              subset_subplot1$EDAD2, subset_subplot2$EDAD2,
              subset_subplot1$NHA, subset_subplot1$AB_Total, subset_subplot1$HD,
              subset_subplot2$NHA, subset_subplot2$AB_Total, subset_subplot2$HD,
              sims[nrow(sims),]$N, sims[nrow(sims),]$BA, sims[nrow(sims),]$HD)

    results <- rbind(results, fila)
  }

}
head(results)

#predicted vs observed BA
plot(results$BAF, results$BAF_pred)
abline(a=0, b=1)

#predicted vs observed Number of Trees
plot(results$NF, results$NF_pred)
abline(a=0, b=1)

#predicted vs observed Number of Trees
plot(results$HDF, results$HDF_pred)
abline(a=0, b=1)
