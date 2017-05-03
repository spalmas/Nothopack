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
#' core.stand<-core_module(input)
#' report(core.stand = core.stand)
#' report(sp.table = sp.table)
#'
#' Example 2: From a simulation

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
  sp.table$sd <- sp.table$sp.table %>% left_join(sp.names, by = 'SPECIE') %>%
    mutate(SPECIES = SPECIES.NAME) %>%
    select(-SPECIES.NAME)

  #Chaning dominant species to vernacular names
  sp.table$DOM.SP <- sp.names$SPECIES.NAME[sp.names$SPECIE == sp.table$DOM.SP]

  #PRINT TABLE PREPARATIONS
  results.print <- sp.table[1:16] %>% as_tibble() %>% t %>% as_tibble()
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
                  ))


  #BEGIN PRINTING
  print('----------   ----------   ----------   ----------')
  print('----------     SIMULATION RESULTS      ----------')
  print('----------   ----------   ----------   ----------')
  Sys.time() %>% format("%x") %>% paste0('Date: ', .) %>% print
  Sys.time() %>% format("%X") %>% paste0('Time: ', .) %>% print


  print('----------     Model parameters      ----------')
  results.print %>% slice(11:16) %>% kable %>% print

  print('---------- STAND PARAMETERS   ----------')
  results.print %>% kable %>% print

  print('----------   CURRENT STAND PARAMETERS BY SPECIES   ----------')
  sp.table$sd %>% kable %>% print
}

