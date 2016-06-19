rm(list=ls()) # It removes ALL objects

require(ggplot2)

source('R/BAmodule.R')
source('R/diagnostics.R')
source('R/diametric_distribution.R')
source('R/exponent.R')
source('R/fitness_stats.R')     # Replaces fitness_table
#source('R/fitness_table.R')  # Discontinued
<<<<<<< HEAD
source('R/get_site.R')          # Replaces remaining_calc_A
source('R/get_stand.R')         # Replaces remaining_calc_B
source('R/get_taper.R')      # It calculates relevant variables for taper equations.
=======
source('R/fitness_stats.R')     # Replaces fitness_table
source('R/get_stand.R')         # Replaces remaining_calc_B
source('R/get_site.R')          # Replaces remaining_calc_A
>>>>>>> 53afe04adc245e851276e67c5919ecc03723ad5c
source('R/hd_coef.R')
source('R/hparam_coef.R')
source('R/Nmodule.R')
source('R/parametric_height.R')
# source('R/remaining_calc_A.R') # Discontinued
# source('R/remaining_calc_B.R') # Discontinued
# source('R/simulator.R') # Discontinued
<<<<<<< HEAD
# source('stand2cohort.R') # Is this discontinued and replaced by stand_simulator?
source('R/stand_parameters.R')   # Fixed with lots of changes and 1 important query
source('R/stand_randomizer.R')   # Fixed with many small changes (particular names)
source('R/stand_simulator.R')
# source('R/tree_simulator.R')  # This has no work and needs to be implemented
source('R/Vmodule.R') 
# source('R/Vmodule_individual.R) # This has some work but it is not complete
=======
source('R/parametric_height.R')
source('R/stand_parameters.R')
source('R/stand_randomizer.R')
source('R/stand_simulator.R')
source('R/Vmodule.R')
>>>>>>> 53afe04adc245e851276e67c5919ecc03723ad5c

hd_coef <- read.csv(file = 'data/hd_coef.csv')
hparam_coef <- read.csv(file = 'data/hparam_coef.csv')
ensayos.data <- read.csv(file = 'data/growth_ensayos.csv')

library(pracma)

# Name Variables
# N, number of trees (trees/ha)
# BA, basal area (m2/ha)
# QD, quadratic diameter (cm)
# SI, site index (m)
# HD, dominant height (m)
# AD, dominant age (years)   ### - To change from ED  ###
# VOL, total stand level volume (m3/ha)
# dom_sp, dominant specie
# zone, growth zone
# HT, total tree height (m)
# DBH, diameter breast height (cm)
# N.Noth total number of trees Nothofagus (trees/ha)
# PropNN proportion trees Nothofagus (0-1??)
# PropBAN proportion trees Nothofagus (0-1??)