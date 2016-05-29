rm(list=ls()) # It removes ALL objects

source('R/BAmodule.R')
source('R/diagnostics.R')
source('R/exponent.R')
#source('R/fitness_table.R')  # Discontinued
source('R/hd_coef.R')
source('R/hparam_coef.R')
source('R/Nmodule.R')
# source('R/remaining_calc_A.R') # Discontinued
# source('R/remaining_calc_B.R') # Discontinued
source('R/simulator.R')
source('R/stand_randomizer.R')
source('R/parametric_height.R')
source('R/stand_parameters.R')
source('R/get_stand.R')         # Replaces remaining_calc_B
source('R/get_site.R')          # Replaces remaining_calc_A
source('R/fitness_stats.R')     # Replaces fitness_table

hd_coef <- read.csv(file = 'data/hd_coef.csv')
hparam_coef <- read.csv(file = 'data/hparam_coef.csv')
