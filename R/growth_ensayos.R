#This script creates a table of stand parameters of the ENSAYOS database.
#The table can be used to validate the results of the stand simulation in Nothopack

#This script calculates several stand parameters from the ENSAYO database.
#This database can be used in the Mortality.rmd files, the new BA equation script and others.
# First we will obtain stand parameters such as quadratic diameterThe stand parameters to fit these curves will come from all the projects inside ENSAYO2 database.
#
#* Only trees above 10 cm will be counted towards the number of trees and quadratic diameters

rm(list = ls())

#########################
#packages need to estimate stand parameters
require(dplyr)
require(ggplot2)
require(grid) # needed for arrow function

#imoprting helper functions
#source('Mortality/helpers.R')

#Reading list of trees, zones and dominant height equation parameters
trees <- read.csv('data/ENSAYO2_TREES_MortalityCLEANED.csv', sep =',')
zones <- read.csv('data/zonas_ensayo_Paulo.csv', sep =',')   #from Paulo
HDcoef <- read.csv('data/hd_coef.csv', sep = ',')

#keep only dominant species Rauli. Is there a better way?
HDcoef <- HDcoef[HDcoef$hd_coef_dom_sp_name ==  'Rauli', ]

#Trees with no age are eliminated. This EDAD2 is the age of the parcela.
trees <- trees[!is.na(trees$EDAD2),]

##Adding NA as factor in species
trees$ESPECIE  = factor(trees$ESPECIE, levels = c(levels(trees$ESPECIE), 'NA'))
#changing NA to 'NA' (Nothofagus alpina)
trees$ESPECIE[is.na(trees$ESPECIE)] <- 'NA'

######################
trees <- dplyr::group_by(trees, ENSAYO, PARCELA, SUBPARC, MEDICION)
#Estimate stand parameters using dplyr function
PRODAL <- summarize (.data = trees,
                           ANHO = unique(F.ANHO), #ANHO of MEDICION
                           EDAD2 = unique(EDAD2), #EDAD of MEDICION
                           SUPERFICIE = unique(SUPERFICIE),   #area of the measurement
                           N = length(ARBOL),        #Getting the number of trees in the plot
                           NHA = length(ARBOL) * 10000/SUPERFICIE,        #Getting the number of trees in the stand by hectare
                           CF = round(100 * unique(SUPERFICIE)/10000), #correction factor (100 trees per hectare)
                           HD = mean(tail(sort(DAP),CF)),   #dominant height
                           DESCRIPCION = unique(Tratamiento_DESCRIP),     #treatment description
                           DQ = sqrt(sum(DAP^2)/N),  #total quadratic diameter,
                           SDI = 100 * N *  (DQ/25.4)^(-1.4112),    #Stand density index per plot
                           AB_NA = sum(pi * (DAP[ESPECIE == 'NA']/2/100)^2, na.rm = TRUE) * 10000/SUPERFICIE,    #basal area of Rauli by hectare
                           AB_NO = sum(pi * (DAP[ESPECIE == 'NO']/2/100)^2, na.rm = TRUE) * 10000/SUPERFICIE,    #basal area of Roble by hectare
                           AB_ND = sum(pi * (DAP[ESPECIE == 'ND']/2/100)^2, na.rm = TRUE) * 10000/SUPERFICIE,    #basal area of coigue by hectare
                           AB_Total =  sum(pi * (DAP/2/100)^2, na.rm = TRUE) * 10000/SUPERFICIE              #total basal area by hectare
)

# Match zones from zones database to stand params
PRODAL['ZONA'] <- zones$zona[match(x = PRODAL$ENSAYO, table = zones$ENSAYO)]

#Estimate SDI Percentage
PRODAL$SDI_perc = 100 * PRODAL$SDI / max(PRODAL$SDI)

#Estimate proportion of basal area of nothofagus
PRODAL$AB_Nothofagus = PRODAL$AB_NA + PRODAL$AB_NO + PRODAL$AB_ND #Basal area of nothofagus

#Proportion of basal area of nothofagis
PRODAL$Noth_prop <- PRODAL$AB_Nothofagus / PRODAL$AB_Total

#Proportion of basal area by nothofagus species
PRODAL$NA_prop = PRODAL$AB_NA / PRODAL$AB_Nothofagus   #Rauli
PRODAL$NO_prop = PRODAL$AB_NO / PRODAL$AB_Nothofagus   #Roble
PRODAL$ND_prop = PRODAL$AB_ND / PRODAL$AB_Nothofagus   #Coigue

#getting a dominant species column
PRODAL$dom_sp <- NA
for (i in 1:nrow(PRODAL)){
  if (PRODAL$Noth_prop[i] < .6 ){    #If the nothodagus represent less than 60% of the stand
    PRODAL$dom_sp[i] = 4     #Others non Nothofagus
  } else {
    if (PRODAL$NA_prop[i] >= .7){
      PRODAL$dom_sp[i] = 1   #Rauli
    } else if (PRODAL$NO_prop[i] >= .7){
      PRODAL$dom_sp[i] = 2   #Roble
    } else if (PRODAL$ND_prop[i] >= .7){
      PRODAL$dom_sp[i] = 3   #Coigue
    } else {
      PRODAL$dom_sp[i] = 4    #Mixed
    }
  }
}

###growth bTable preparation
#This long code constructs a table of the changes found in our database. (Age t to Age t + 1 in one line)
#For each plot, we have a row of the measurements $t$ and $t+1$ with their
#respective number of trees per hectare and quadratic diameter.
#Therefore if a plot only had one measurement, there is no row for that plot.
#If a plot had measurements 1,2, 3 and 5, there will be 3 rows, one for 1 to 2, 2 to 3 and 3 to 5.

#There are several very low values of tree densities.
#I just identified these low points vissualy from the graph above.
#I took the liberty of deleting the these points.

#getting a list of unique plots in our dataset
unique_plots <- unique(cbind(PRODAL$ENSAYO, PRODAL$PARCELA, PRODAL$SUBPARC))

#Starting the table to store results and assigning column names
column.names <- c('ENSAYO', 'PARCELA', 'SUBPARC',
                  'MEDICION0' , 'MEDICION1',
                  'NHA0', 'NHA1', 'QD0', 'QD1', 'dom_sp',
                  'Delta.ANHO', 'EDAD0', 'EDAD1', 'zone', 'HD0', 'HD1', 'BA0', 'BA1')
changes <- matrix(ncol = length(column.names))
colnames(changes) = column.names

#for each of the plots
for (y in 1:nrow(unique_plots)){
  #getting the plot subset
  PRODAL_plot <- PRODAL[PRODAL$ENSAYO == unique_plots[y,1] &
                                      PRODAL$PARCELA == unique_plots[y,2] &
                                      PRODAL$SUBPARC == unique_plots[y,3],]

  #getting sorted list of MEDICIONES in plot
  MEDICION_plot <- sort(PRODAL_plot$MEDICION)

  #For those plots that have more than one MEDICION
  if(!length(MEDICION_plot) == 1){
    for (x in 1:(length(MEDICION_plot)-1)){
      a <- PRODAL_plot$ENSAYO[1] #ENSAYO number
      b <- PRODAL_plot$PARCELA[1] #PARCELA number
      c <- PRODAL_plot$SUBPARC[1] #SUBPARC  number
      d <- MEDICION_plot[x] #MEDICION 1 number
      e <- MEDICION_plot[x+1] #MEDICION 2 number
      f <- PRODAL_plot[PRODAL_plot$MEDICION == MEDICION_plot[x],]$NHA #trees per t1
      g <- PRODAL_plot[PRODAL_plot$MEDICION == MEDICION_plot[x+1],]$NHA #trees per t2
      h <- PRODAL_plot[PRODAL_plot$MEDICION == MEDICION_plot[x],]$DQ #trees per t1
      i <- PRODAL_plot[PRODAL_plot$MEDICION == MEDICION_plot[x+1],]$DQ #trees per t2
      j <- PRODAL_plot$dom_sp[1] #SUBPARC  number
      k <- PRODAL_plot$ANHO[x+1] - PRODAL_plot$ANHO[x] #Difference in years between measurements
      l <- PRODAL_plot$EDAD[x]
      m <- PRODAL_plot$EDAD[x+1]
      n <- PRODAL_plot$ZONA[x]
      o <- PRODAL_plot$HD[x]
      p <- PRODAL_plot$HD[x+1]
      q <- PRODAL_plot$AB_Total[x]
      r <- PRODAL_plot$AB_Total[x+1]

      # adding the values to a row in final dataframe
      changes <- rbind(changes, c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
    }
  }
}

#Eliminating the first blank row of the database
changes <- data.frame(changes[2:nrow(changes),])

#Some columns are weirdly changed from numeric to other thing
changes$NHA0 <- as.numeric(as.vector(changes$NHA0))#as.
changes$NHA1 <- as.numeric(as.vector(changes$NHA1))#as.
changes$QD0 <- as.numeric(as.vector(changes$QD0))#as.
changes$QD1 <- as.numeric(as.vector(changes$QD1))#as.
changes$Delta.ANHO <- as.numeric(as.vector(changes$Delta.ANHO))#as.

#Create a dummy variable for NA =1, Mixed = 0
changes['DummyNA'] <- (changes$dom_sp == 'NA') *1 #sVariable not significant

#Adding a column that had an increase in number of trees
changes['ingrowth'] <- changes$NHA2 > changes$NHA1


#exporting results to a table
write.csv(x = changes, file = 'data/growth_ensayos.csv')
