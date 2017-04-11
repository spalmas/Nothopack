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
#' sp.table <- inputmodule(level='stand',zone=2,AD=25,HD=23.4,N=N,BA=BA)
#' report(tree.list = tree.list)
#'
#' # Example 3: From stand level table
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' sp.table <- inputmodule(level='stand',zone=2,AD=25,HD=23.4,N=N,BA=BA)
#' report(sp.table = sp.table)

report <- function(tree.list = NULL, stand.table = NULL,  sp.table = NULL,
                   print.diam.dist = TRUE,
                   class.width = 5, dmin = 5, dmax = 100){

  #Table with species codes and species names for printing
  sp.names <- tibble(SPECIES = c(1,2,3,4,0),
                     SPECIES.NAME = c('Rauli', 'Roble', 'Coigue', 'Companion', 'All'))


  #Three options... SP.TABLE, stand.table and tree.list
  if(!is.null(tree.list)){

  sp.table <- tree.list %>% group_by(SPECIES) %>%
    summarise()


  } else if (!is.null(stand.table)){
    #BUILDING DIAMETER DISTRIBUTION

  } else if (!is.null(sp.table)){

    #CHanging Stand parameters species to vernacular names
    sp.table$sd <- sp.table$sd %>% left_join(sp.names, by = 'SPECIES') %>%
      mutate(SPECIES = SPECIES.NAME) %>%
      select(-SPECIES.NAME)

    #Chaning dominant species to vernacular names
    sp.table$DOM.SP <- sp.names$SPECIES.NAME[sp.names$SPECIES == sp.table$DOM.SP]

    #Building a table of all other stand parameters and changing col and rownames
  }

  #TABLE PREPARATIONS
  print.sp <- (sp.table %>% as.data.frame())[1,5:14] %>% t %>%
  `colnames<-` (c('Value')) %>%
  `rownames<-` (c('Dominant Species', 'Dominant Height (m): ',
                'Proportion of BA of Nothofagus', 'Proportion of NHA of Nothofagus',
                'Site Index (m)', 'Dominant Age (years)',
                'Final Age  (years)','Area (m)', 'comp', 'Zone'))

  #BEGIN PRINTING
  print('----------   CURRENT STAND PARAMETERS   ----------')
  print.sp %>% kable %>% print

  print('----------   CURRENT STAND PARAMETERS BY SPECIES   ----------')
  sp.table$sd %>% kable %>% print
}

