rm(list=ls()) # It removes ALL objects

library(ggplot2)
library(grid)
library(pracma)

source('R/BAmodule.R')
source('R/diagnostics.R')
source('R/diametric_distribution.R')
source('R/fitness_stats.R')
source('R/get_site.R')
source('R/get_stand.R')
source('R/get_taper.R')      # It calculates relevant variables for taper equations.
source('R/hd_coef.R')
source('R/hparam_coef.R')
source('R/Nmodule.R')
source('R/parametric_height.R')
source('R/plot_results.R')
source('R/stand_parameters.R')
source('R/stand_randomizer.R')
source('R/stand_simulator.R')     # This seems only projection
# source('R/tree_simulator.R')    # This has no work and needs to be implemented
source('R/Vmodule.R')
# source('R/Vmodule_individual.R) # This has some work but it is not complete

hd_coef <- read.csv(file = 'data/hd_coef.csv')
hparam_coef <- read.csv(file = 'data/hparam_coef.csv')
ensayos.data <- read.csv(file = 'data/growth_ensayos.csv')

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
