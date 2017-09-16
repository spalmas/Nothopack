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
#' plot<-input_module(type='stand',zone=2,AD=28,HD=23.5,N=N,BA=BA)
#' DDist<-diam_dist(sp.table=plot$sp.table, HD=plot$HD,DOM.SP=plot$DOM.SP, zone=plot$zone)[5,,]
#' get_percentile(DBH = DDist[,3], FT = DDist[,5])
get_percentile <- function(DBH = NA, FT = NA, percentiles = c(0.15, 0.85)){
  df <- tibble(DBH = DBH, FT = FT) %>%
    arrange(DBH) %>%
    mutate(CumFT = cumsum(FT)) %>%
    filter(FT > 0)
  Cperc <- sum(df$FT) * percentiles
  places <- Cperc %>% cut(., breaks = df$CumFT, right = FALSE) %>% as.numeric
  values <- (df$CumFT[places]*df$DBH[places]+df$DBH[places+1]*(Cperc - df$CumFT[places]))/Cperc
  df <- tibble(Percentile = percentiles, DBH = values)
  return(df)
}
