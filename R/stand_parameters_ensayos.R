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
PRODAL$Dominant <- NA
for (i in 1:nrow(PRODAL)){
  if (PRODAL$Noth_prop[i] < .6 ){    #If the nothodagus represent less than 60% of the stand
    PRODAL$Dominant[i] = 'Others'
  } else {
    if (PRODAL$NA_prop[i] >= .7){
      PRODAL$Dominant[i] = 'NA'
    } else if (PRODAL$NO_prop[i] >= .7){
      PRODAL$Dominant[i] = 'NO'
    } else if (PRODAL$ND_prop[i] >= .7){
      PRODAL$Dominant[i] = 'ND'
    } else {
      PRODAL$Dominant[i] = 'Mixed'
    }
  }
}


##Site Index
#We need to estimate the site index. We have the dominant height from our plots and we have the dominant age or age at DAP from each plot (maybe for some part of them). We can use the dominant height curves published in Gezan y Moreno (2000a) to estimate our site index. Site index is estimated by bisection method of the equation below.
#
#The model is
#$$\mathrm{HD} = 0.3 + a\left[1-\left(1-(\mathrm{IS}/a)^c\right)^{(E+0.5)/18}\right]^{1/c}$$
#$$c = b_0 + b_1 \mathrm{IS}$$
#
#where $\mathrm{HD}$ is Dominant height (m), $E$ is the dominant age or age at DAP (years), $\mathrm{IS}$ is the Site Index at year 20 (m), $a$, $b_0$ and $b_1$ are parameters to be estimated and $c$ is a Site index constant

#There is no zone 3 for dominant species Rauli in the HDcoef table. Zones 3 and NA were changes to zones 2 in PRODAL
PRODAL$ZONA[!PRODAL$ZONA %in% c(1,3,4)] <- 2

#It will only include plots with known age (EDAD2 != NA)
PRODAL <- PRODAL[!is.na(PRODAL$EDAD2),]


#exponent definition to allow negative numbers to be elevated
exponent <- function(a, pow) (abs(a)^pow)*sign(a)

require(pracma)  #bisection method package
PRODAL['IS'] <- NA  #blank column

for (i in 1:nrow(PRODAL)){
  ZONA <-   PRODAL$ZONA[i]   #getting zone
  EDAD <- PRODAL$EDAD2[i]
  HD <- PRODAL$HD[i]

  #getting parameters from HDcoef table
  a1 <- HDcoef$hd_coef_a[HDcoef$hd_coef_zone == ZONA]
  b0 <- HDcoef$hd_coef_b0[HDcoef$hd_coef_zone == ZONA]
  b1 <- HDcoef$hd_coef_b1[HDcoef$hd_coef_zone == ZONA]

  #function that defines the bisection
  IS.eq <- function(x){
    c1 <- b0 + b1 * x
    0.3 + a1 * (1 - exponent( 1 - (x/a1)^c1, (EDAD+0.5)/18) )^(1/c1) - HD
  }

  #x = 50
  #IS.eq(50)

  #assigning the Site Index
  PRODAL$IS[i] <- tryCatch(bisect(IS.eq,
                               a=10, b=30,
                               maxiter = 100)$root,
                               error = function(e) {NA})

}

#write.csv(PRODAL, file = 'data/stand_parameters_ensayos.csv')

