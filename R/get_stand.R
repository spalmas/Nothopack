#' Calculates remaining stand level variable from the set: N, BA, QD
#'
#' \code{get_stand} When two of the stand level variables: basal area (BA, m2), number of trees (N, trees/ha) or quadratic diameter (QD, cm)
#' are given, it returns the value of the remaining stand level parameter.
#'
#' @param BA Basal area (m2).
#' @param N Number of trees (trees/ha).
#' @param QD Quadratic diameter (cm)
#' 
#' @return The missing stand level parameter
#' 
#' @examples
#' get_stand(BA=25.2,N=1400)
#' get_stand(QD=12,N=1400)
#' get_stand(BA=25.2,QD=8.1)

get_stand <- function(BA=NA, N=NA, QD=NA){

  if (sum(is.na(c(BA, QD, N))) >2 ){
    stop('There must be at least two stand parameters provided')
  
  } else if ( sum(is.na(c(BA, QD, N))) == 0 ){
    warning('Why would you use this calculation? You allready have the three variables')
  
  } else if (is.na(BA)) {      # Estimation of BA witn N and QD
    return((pi/4)*N*(QD/100)^2)
  
  } else if (is.na(N)) {       # Estimation of N with BA and QD
    return((4/pi)*BA*(100/QD)^2)
  
  } else if (is.na(QD)) {      # Estimation of QD with N and BA
    return(100*((4/pi)*(BA/N))^0.5)
  }
}
