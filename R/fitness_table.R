#' Fitness values for observed and fitted values.
#'
#' @param obs Array of observed values.
#' @param pred Array of predicted values.
#' @return A table with R^2 empirical, RMSE, and Bias%.
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
