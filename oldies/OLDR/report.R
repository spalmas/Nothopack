#' Reports stand table parameters in pretty form.
#'
#' \code{report} Reads sp.table from stand_parameters or the stand output and prints it in a pretty way.
#' Optionally, the user can choose some printing formats.
#
#' @param tree.list lista completa de arboles
#' @param stand.table tabla de rodal con clases diametricas de 1 cm.
#' @param sp.table matriz de resumen de specie con parámetros de rodal
#' @param print.diam.dist To show or not to show diametric distribution
#' @param class.width Minimum Diameter to show on Diametric Distribution
#' @param dmax Maximum Diameter to show on Diametric Distribution
#' @param dmax Maximum Diameter to show on Diametric Distribution
#' @param pdf If the user wants a pdf output
#'
#' @return Prints pretty report tables
#'
#' @author
#' S. Palmas, S.A. Gezan and P. Moreno
#'
#' @examples
#' library(tidyverse)
#' library(knitr)
#'
#' # Example 1: From stand level table
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' input<-inputmodule(type='stand',zone=1,AD=28,HD=23.5,N=N,BA=BA, AF = 40)
#' core.stand<-core_module(input = input, ddiam = TRUE)
#' report(core.stand = core.stand)
#'
#' Example 2: From a simulation
#' BA<-c(1.09,38.92,0,0.31)
#' N<-c(60,780,0,80)
#' input<-inputmodule(type='stand',zone=2,AD=28,HD=15.5,N=N,BA=BA,AF=35,V_model=1)
#' core.stand<-core_module(input = input)
#' stand_simulation<-stand_simulator(core.stand = core.stand)
#' report(core.stand = stand_simulation)
#'
report <- function(core.stand,
                   print.diam.dist = TRUE,
                   class.width = 5, dmin = 5, dmax = 100,
                   pdf = FALSE,
                   export.csv = FALSE,
                   ind.simulation = TRUE){


  #Table with species codes and species names for printing
  sp.names <- tibble(SPECIE = c(1,2,3,4,0),
                     SPECIES.NAME = c('Rauli', 'Roble', 'Coigue', 'Companion', 'All'))


  #CHanging Stand parameters species to vernacular names
  core.stand$sp.table <- core.stand$sp.table %>% left_join(sp.names, by = 'SPECIE') %>%
    mutate(SPECIES = SPECIES.NAME) %>%
    select(SPECIES, N, BA, QD, VTHA)

  #Chaning dominant species to vernacular names
  core.stand$DOM.SP <- sp.names$SPECIES.NAME[core.stand$DOM.SP]

  #PRINT TABLE PREPARATIONS
  results.print <- core.stand[1:16] %>% as.matrix() %>%   # %>% as.data.frame() %>% t %>%
    `colnames<-` (c('Value')) %>%
    `rownames<-` (c('Zone',
                  'Dominant Species',
                  'Dominant Age (years)',
                  'Dominant Height (m)',
                  'Site Index (m)',
                  'SDI (m)',
                  'Proportion of BA of Nothofagus',
                  'Proportion of NHA of Nothofagus',
                  'Final Age  (years)',
                  'Area (m)',
                  'type',
                  'ddiam',
                  'comp',
                  'Mortality model',
                  'Volume model',
                  'IADBH model'
                  )) %>%
    as.data.frame() %>%
    rownames_to_column()


  #BEGIN PRINTING
  print('----------   ----------   ----------   ----------')
  print('----------     SIMULATION RESULTS      ----------')
  print('----------   ----------   ----------   ----------')
  Sys.time() %>% format("%x") %>% paste0('Current Date: ', .) %>% print
  Sys.time() %>% format("%X") %>% paste0('Current Time: ', .) %>% print


  print('----------     Model parameters      ----------')
  results.print [11:16,] %>% kable %>% print

  print('---------- STAND PARAMETERS   ----------')
  results.print %>% kable %>% print

  print('----------   CURRENT STAND PARAMETERS BY SPECIES   ----------')
  core.stand$sp.table %>% kable %>% print
}

