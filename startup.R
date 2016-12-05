rm(list=ls()) # It removes ALL objects

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

#source('R/BAmodule.R')
source('R/BANmodule.R')
source('R/BANmodule2.R')
source('R/BA99module.R')
source('R/diagnostics.R')
source('R/diam_dist.R')      # New but still with issues: too slow
source('R/fitness_stats.R')
source('R/get_props.R')
source('R/get_site.R')
source('R/get_stand.R')
source('R/get_taper.R')      # It calculates relevant variables for taper equations !INCOMPLETE missing models.
source('R/hd_coef.R')
source('R/hparam_coef.R')
source('R/multiplot.R')
source('R/Nmodule.R')
source('R/parametric_height.R')
source('R/plot_results.R')
source('R/RECRUITmodule.R')
source('R/stand_parameters.R')
source('R/stand_randomizer.R')
#source('R/stand_simulator.R')     # This seems only projection
#source('R/stand_simulator2.R')    # Using vectorized input
source('R/stand_simulator3.R')    # Using vectorized input and BAN and BA99 models
# source('R/tree_simulator.R')    # This has no work and needs to be implemented
source('R/Vmodule.R')
source('R/Vmodule_individual.R')

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


# MIXED STANDS... they have some issues that need to be fixed..
# (mainly we need to assign it to the main  specie)
