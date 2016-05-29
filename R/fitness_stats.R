#' Calculates goodness-of-fit statistics
#' 
#' \code{fitness_stats} Obtains goodness-of-fit statistics (R2emp, RMSE, BIAS, RMSE% and BIAS%) 
#' using provided observed and fitted values. Missing values are removed from the list.
#'
#' @param obs vector of observed values.
#' @param pred vector of predicted values.
#' 
#' @return A table with goodness-of-fit statistics:
#' R2emp: empirical coefficient of correlation
#' RMSE (and RMSE%) root mean square error (raw and in percentage of mean of observed data) 
#' BIAS (and BIAS%) bias (raw and in percentage of mean of observed data) 
#' 
#' @examples
#' x <- c(1:20)
#' yobs <- 10 + 0.1*x + rnorm(20)
#' fit <- lm(yobs ~ x)
#' fitness_stats(yobs,predict(fit))

fitness_stats <- function(obs, pred){
  
  mean_obs  <- mean(obs, na.rm=TRUE)
  fit_r2emp <- round(1 - sum((obs - pred)^2, na.rm=TRUE) / sum((obs - mean_obs)^2, na.rm=TRUE), digits=4)
  fit_rmse  <- round(sqrt( sum( (pred - obs)^2, na.rm=TRUE) / (length(obs) - 1) ), digits=4)  # This was fixed
  fit_rmsep <- round(100 * fit_rmse/mean_obs, digits=4)
  fit_bias  <- round( sum( pred - obs , na.rm=TRUE) , digits=4)
  fit_biasp <- round(100 * fit_bias/mean_obs, digits=4)
  
  tabla <- data.frame(c(fit_r2emp, fit_rmse, fit_rmsep, fit_bias, fit_biasp),
                      row.names = c('R2emp', 'RMSE', 'RMSE%', 'BIAS', 'BIAS%'))
  colnames(tabla) <- ('value')

  return (tabla)
}

# Note: - Several corrections in the statistics.