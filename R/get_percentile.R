#' Calculates percentiles correcting for expansion factors or N
#'
#' \code{get_percentile} It corrects for expansion factors or N
#'
#' @param DBH Diameter list
#' @param FT expansion factors (or N, in case of diametric distribution)
#' @param percentiles list of desired percentile values
#'
#' @references
#' None for now
#'
#' @return List of desired percentile values
#'
#' @examples
#' DBH<-c(36.5,2.8,0,2.4)
#' N<-c(464,23,0,48)
#' get_percentile(DBH = DBH, FT = N)
get_percentile <- function(DBH = NA, FT = NA, percentiles = c(0.15, 0.85)){
  df <- tibble(DBH = DBH, FT = FT) %>%
    arrange(DBH) %>% #sorting from small diameter to largest
    mutate(CumFT = cumsum(FT)) %>%   #Calculating cumulative sum of FT
    filter(FT > 0) %>%    #Removing those lines without FT. Otherwise the cut function will fail because of repeated values
    rbind(c(0, 0, 0)) %>%  #Because some percentile may fall below the lowest DDist class
    arrange(DBH) #sorting from small diameter to largest
  Cperc <- sum(df$FT, na.rm = TRUE) * percentiles    #Percentiles adjusted for total FT
  places <- Cperc %>% cut(., breaks = df$CumFT, right = FALSE) %>% as.numeric    #Places of the percentiles inside the CumFT array
  values <- (df$CumFT[places]*df$DBH[places]+df$DBH[places+1]*(Cperc - df$CumFT[places]))/Cperc    #Calculating the adjusted percentiles
  df <- tibble(Percentile = percentiles, DBH = values)  #Building return dataframe
  return(df)
}
