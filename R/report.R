#' Reports stand table parameters in pretty form.
#'
#' \code{report} Reads input from stand_parameters or the stand output and prints it in a pretty way.
#' Optionally, the user can choose some printing formats.
#
#' @param input Input
#' @param dmin Minimum Diameter to show on Diametric Distribution
#' @param dmax Maximum Diameter to show on Diametric Distribution
#' @param diam_distr To show or not to show diametric distribution
#'
#' @references
#'
#' @return Prints pretty report tables
#'
#' @author
#' S. Palmas, S.A. Gezan and P. Moreno
#'
#' @examples
#' library(tidyverse)
#' library(knitr)
#' # Example 1: From list of Trees
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' input <- inputmodule(level='stand',zone=2,AD=25,HD=23.4,N=N,BA=BA)
#' report(input = input)

report <- function(input = NULL, class_width=5, dmin=5, dmax = 100){
  #Table with species codes and species names for printing
  sp.table <- tibble(SPECIES = c(1,2,3,4,0), SPECIES.NAME = c('Rauli', 'Roble', 'Coigue', 'Companion', 'All'))

  #Two options... STANDLIST OR STAND LEVEL PARAMETERS
  if(is.null(input)){

    print('There is no stand information')

  } else {

    #CHanging Stand parameters species to vernacular names
    input$sd <- input$sd %>% left_join(sp.table, by = 'SPECIES') %>%
      mutate(SPECIES = SPECIES.NAME) %>%
      select(-SPECIES.NAME)

    #Chaning dominant species to vernacular names
    input$DOM.SP <- sp.table$SPECIES.NAME[sp.table$SPECIES == input$DOM.SP]

    #Building a table of all other stand parameters
    print.sp <- (input %>% as.data.frame())[1,5:14] %>% t
    #Chaning rownames for prettier printing names
    rownames(print.sp) <- c('Dominant Species',
                            'Dominant Height (m): ',
                            'Proportion of BA of Nothofagus',
                            'Proportion of NHA of Nothofagus',
                            'Site Index (m)',
                            'Dominant Age (years)',
                            'Final Age  (years)',
                            'Area (m)',
                            'comp',
                            'Zone'
    )

    #BEGIN PRINTING
    print('----------   CURRENT STAND PARAMETERS   ----------')
    print.sp %>% kable %>% print

    print('----------   CURRENT STAND PARAMETERS BY SPECIES   ----------')
    input$sd %>% kable %>% print
  }
}

