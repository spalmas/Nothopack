#' To obtain PS from PScal
#'
#' @param PScal Array of Relative BAL (BAL/BA) for each tree
#'
#' @return Sociologic Status PS (In this case continuous) 
#'
#' @examples
#' PS<-SS(PScal=1);PS
#'

tree.SS<-function(PScal){
  PS <- 1+ PScal*3#New Sociologic Status
  return(PS)
}
