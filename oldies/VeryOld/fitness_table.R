#' Calculates goodness-of-fit statistics
#' 
#' \code{fitness} Obtains goodness-of-fit statistics (R2emp, RMSE, BIAS, RMSE% and BIAS%) 
#' using provided observed and fitted values.
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
#' fit <- lm(sr ~ ., data = LifeCycleSavings)
#' fitness_table(LifeCycleSavings$sr, predict(fit))

fitness_table <- function(obs, pred){
  fit_r2 <- round(1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE), digits = 2)
  fit_rmse <- round(100 * sqrt( sum( (pred - obs)^2, na.rm = TRUE) / (length(obs) - 1) ), digits = 2)
  fit_bias <- round(100 * ( sum( pred - obs , na.rm = TRUE) / sum( obs, na.rm = TRUE)), digits = 2)

    tabla <- data.frame(c(fit_r2, fit_rmse, fit_bias),
                      row.names = c('R2_emp', 'RMSE', 'Bias%'))
  colnames(tabla) <- ('value')
  return (tabla)

}
