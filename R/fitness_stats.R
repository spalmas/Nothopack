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

fitness_stats <- function(obs, pred, digits = 2){

  mean_obs  <- mean(obs, na.rm=TRUE)
  fit_r2emp <- 1 - sum((obs - pred)^2, na.rm=TRUE) / sum((obs - mean_obs)^2, na.rm=TRUE)
  fit_rmse  <- sqrt( sum( (obs - pred)^2, na.rm=TRUE) / (length(obs) - 1) )  # This was fixed
  fit_rmsep <- 100 * fit_rmse/mean_obs
  fit_bias  <- sum( obs - pred, na.rm=TRUE)
  fit_biasp <- 100 * fit_bias/sum(obs, na.rm = TRUE)

  tabla <- matrix(round( c(fit_r2emp,
                  fit_rmse,
                  fit_rmsep,
                  fit_bias,
                  fit_biasp), 
                digits = digits), 
         ncol = 5)

  tabla <- as.data.frame(tabla)

  colnames(tabla) <- c('r2emp', 'RMSE', 'RMSE_perc', 'BIAS', 'BIAS_perc')

  return (tabla)
}

#fitness_stats(obs = PRODAL$PNHA_NOTH, pred = PRODAL$PNHA_NOTH.lm1, digits = 3)