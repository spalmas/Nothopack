#' Stand Parameter calculation
#'
#' When given a stand table it returns a table with stand parameters
#' @param stand: table. It must include the columns
#' @param area: Area of plot in square meters. Default to one hectare plot.
#' @param species.col: name of species column name. Default to ESPECIES.
#' @param dbh.col: name of dbh column name. Default to DAP.
#'
#' @return A table with the columns BA (m2), NHA, DC, DAPM, MEDAD, HDOM, EDOM
#'
#' @examples
#' library(Nothopack)
#' stand_parameters(stand=stand, area=2000)

stand_parameters <- function(stand, area = 10000,
                             species.col = 'SPECIES', dbh.col = 'DBH', height.col = 'HEIGHT'){

  CF <- 10000 / area  #correction factor
  BA <- CF * sum(pi * (stand[dbh.col]/2)^2, na.rm = TRUE) / 10000
  NHA <- CF * nrow(stand)
  DQ <- sqrt(sum(stand[dbh.col]^2)/NHA)

  HD <- mean(head(stand[order(stand[height.col], decreasing = TRUE), height.col], CF))

  #Proportion by spcies and dominance
  NA_NHA <- CF * sum (stand[species.col] == 'NA', na.rm = TRUE)    #number of Rauli trees by hectare
  NO_NHA <- CF * sum (stand[species.col] == 'NO', na.rm = TRUE)    #number of Roble trees by hectare
  ND_NHA <- CF * sum (stand[species.col] == 'ND', na.rm = TRUE)    #number of Coigue trees by hectare

  #Proportion of nothofagus species numbers
  NA_prop <- NA_NHA / NHA   #Rauli
  NO_prop <- NO_NHA / NHA   #Roble
  ND_prop <- ND_NHA / NHA   #Coigue

  #Proportion of nothofagus tree number
  Noth_prop <- NA_prop + NO_prop + ND_prop

  #getting a dominant species column
  if (Noth_prop < .6 ){    #If the nothodagus represent less than 60% of the stand
    SPDOM = '4'  #Others besides Nothofagus
  } else {
    if (NA_prop >= .7){
      SPDOM <- '1'  #Rauli
    } else if (NO_prop >= .7){
      SPDOM <- '2'  #Roble
    } else if (ND_prop >= .7){
      SPDOM <- '3'  #Coigue
    } else {
      SPDOM <- '4'   #Mixed Nothofagus
    }
  }

  return(data.frame(BA, NHA, DQ, HD, Noth_prop, SPDOM))
}
