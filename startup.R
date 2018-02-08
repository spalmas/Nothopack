rm(list=ls()) # It removes ALL objects
###---
# IMPORTING PACKAGES ----------------------
###---

library(shiny)
library(grid)
library(gridExtra)
library(tidyverse)
library(knitr)
library(pracma)  ## ? errors?

###---
# IMPORTING FILES ----------------------
###---

source('R/AIDBH_module.R')
source('R/BA_module.R')
source('R/BA99_module.R')
source('R/BAN_module.R')
source('R/comp_simulator.R')
source('R/CORE_simulator.R')
source('R/DDIAM_module.R')
source('R/get_covariates.R')
source('R/get_domsp.R')
source('R/get_percentile.R')
source('R/get_site.R')
source('R/get_stand.R')
source('R/get_taper.R')   
source('R/hd_coef.R')  # Move, integrate somewhere else?
source('R/height_param.R')  # Ditto
source('R/hparam_coef.R')   # Ditto
source('R/INPUT_module.R')
source('R/NHA_module.R')
source('R/RECRUIT_module.R')
source('R/report.R')
source('R/report_plots.R')  #For multiplot of simulation results
source('R/social_status.R')
source('R/stand_parameters.R')
source('R/stand_simulator.R') 
source('R/tree_simulator.R')  
source('R/VIND_module.R')
source('R/VOL_module.R')
source('R/VPROD_module.R')


###---
# IMPORTING DATA ----------------------
# Chhange all of this to Rd files
###---

hd_coef      <- read.csv(file = 'data/hd_coef.csv')
hparam_coef  <- read.csv(file = 'data/hparam_coef.csv')
ensayos.data <- read.csv(file = 'data/growth_ensayos.csv')
taper_params <- read.csv(file = 'data/taper_parameters.csv')
plot_example   <- read.csv(file = 'data/Plot_example.csv')
products_setup <- read.csv(file = 'data/products_setup.csv')
