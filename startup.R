rm(list=ls()) # It removes ALL objects

library(dplyr)
library(grid)
library(gridExtra)

source('R/AIDBH_module.R')
source('R/BAmodule.R')
source('R/BANmodule.R')
source('R/BA99module.R')
source('R/covariates.R')
source('R/diagnostics.R')
source('R/diam_dist.R')
source('R/fitness_stats.R')
source('R/get_site.R')
source('R/get_stand.R')
source('R/get_taper.R')      # It calculates relevant variables for taper equations !INCOMPLETE missing models.
source('R/get_domsp.R')
source('R/hd_coef.R')
source('R/height_param.R')
source('R/hparam_coef.R')
source('R/Nmodule.R')
source('R/plot_results.R')
source('R/RECRUITmodule.R')
source('R/report.R')
source('R/Social_Status.R')
source('R/stand_parameters.R')
source('R/stand_randomizer.R')
source('R/stand_simulator.R')     # This seems only projection
source('R/tree_simulator1.R')    # until now only growth
source('R/Vmodule.R')
source('R/Vmodule_individual.R')
source('R/inputmodule.R')
source('R/core_module.R')

hd_coef      <- read.csv(file = 'data/hd_coef.csv')
hparam_coef  <- read.csv(file = 'data/hparam_coef.csv')
ensayos.data <- read.csv(file = 'data/growth_ensayos.csv')
taper_params <- read.csv(file = 'data/taper_parameters.csv')
plot_roble   <- read.csv(file = 'data/Plot_example.csv')
products_setup <- read.csv(file = 'data/products_setup.csv')

# Name Variables
# N, number of trees (trees/ha)
# BA, basal area (m2/ha)
# QD, quadratic diameter (cm)
# SI, site index (m)
# HD, dominant height (m)
# AD, dominant age (years)   ### - To change from ED  ###
# VOL, total stand level volume (m3/ha)
# DOM.SP, dominant specie
# zone, growth zone
# HT, total tree height (m)
# DBH, diameter breast height (cm)
# N.Noth total number of trees Nothofagus (trees/ha)
# PNHAN proportion trees Nothofagus (0-1??)
# PBAN proportion trees Nothofagus (0-1??)
