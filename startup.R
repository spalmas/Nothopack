rm(list=ls()) # It removes ALL objects

source('R/BAmodule.R')
source('R/diagnostics.R')
source('R/diametric_distribution.R')
source('R/exponent.R')
#source('R/fitness_table.R')  # Discontinued
source('R/hd_coef.R')
source('R/hparam_coef.R')
source('R/Nmodule.R')
# source('R/remaining_calc_A.R') # Discontinued
# source('R/remaining_calc_B.R') # Discontinued
source('R/simulator.R') # Discontinued
source('R/stand_simulator.R')
source('R/stand_randomizer.R')
source('R/parametric_height.R')
source('R/stand_parameters.R')
source('R/get_stand.R')         # Replaces remaining_calc_B
source('R/get_site.R')          # Replaces remaining_calc_A
source('R/fitness_stats.R')     # Replaces fitness_table

hd_coef <- read.csv(file = 'data/hd_coef.csv')
hparam_coef <- read.csv(file = 'data/hparam_coef.csv')
ensayos.data <- read.csv(file = 'data/growth_ensayos.csv')

# Name Variables
# N, number of trees
# BA, basal area
# QD, quadratic diameter
# SI, site index
# HD, dominant height
# AD, dominant age     ### - To chance from ED  ###
# dom_sp, dominant specie
# zone, growth zone
# HT, total tree height
# DBH, diameter breast height

