#' Calculates goodness-of-fit statistics for segments of the lists
#'
#' \code{fitness_stats_s} Returns a table of goodness-of-fit statistics (n, R2emp, RMSE, BIAS, RMSE% and BIAS%)
#' using provided observed and fitted values, list and limits of the segments
#'
#' @param data table of data
#' @param obs vector of observed values or name of column if data is present
#' @param pred vector of predicted values or name of column if data is present
#' @param var array of segments variable or name of column if data is present
#' @param limits list of limits for the segments of var
#'
#' @return A table with goodness-of-fit statistics:
#' n: number of measures
#' R2emp: empirical coefficient of correlation
#' RMSE (and RMSE%) root mean square error (raw and in percentage of mean of observed data)
#' BIAS (and BIAS%) bias (raw and in percentage of mean of observed data)
#'
#' @examples
#' x <- 1:20
#' obs <- 10 + 0.1*x + rnorm(20)
#' pred <- predict(lm(obs ~ x))
#' var <- 1:20
#' limits <- c(5,10,15)
#' data <- data.frame(obs, pred, var)
#' fitness_stats_s(data = data, obs = 'obs', pred = 'pred', var = 'var', limits = limits)
#'

fitness_stats_s <- function( data = NULL, obs = NULL, pred = NULL, var = NULL, limits = NULL){
  #if data is supplied use the predicted and data texts to dind the column. It avoids the need to write thedatabase several times.
  if(!is.null(data)){
    data <- as.data.frame(data)
    obs = data[,obs]
    pred = data [,pred]
    var = data [,var]
  }

  #where is the number in the the interval?
  intervals <- findInterval(x = var, vec = limits)

  #create empty table
  tabla <- fitness_stats()
  for(s in unique(intervals)){
    tabla <- rbind(tabla, fitness_stats(obs = obs[intervals == s], pred = pred[intervals == s] ))
  }

  #creating row names with intervals
  rownames <- matrix(c(limits[-length(limits)], limits[-1]), ncol=2)
  rownames <- apply(format(rownames), 1, paste, collapse="-")
  row.names(tabla) <- c(paste0('-',limits[1]), rownames, paste0(limits[length(limits)], '-'))

  return (tabla)
}

