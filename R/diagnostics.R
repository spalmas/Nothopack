#' Diagnostic plots for regression fitted and predicted values
#'
#' @param obs Array of observed values.
#' @param pred Array of predicted values.
#'
#' @return Four different diagnostic plots
#'
#' @examples
#' x <- c(1:20)
#' yobs <- 10 + 0.1*x + rnorm(20)
#' fit <- lm(yobs ~ x)
#' diagnostics(residuals(fit),predict(fit))

diagnostics<-function(obs,pred){

  resid <-  obs - pred
  par(mfrow=c(2,2))

  # Plot 1 -Predicted vs observed
  plot(y=resid,x=pred, main='Fitted value plot',
       ylab="Residual", xlab="Fitted value")
  abline(h=0,col="blue")

  # Plot 2 - Index Plot
  index<-seq(1,length(resid))
  plot(y=resid,x=index, main='Index plot', ylab="Residual", xlab="Index")
  abline(h=0,col="blue")

  # Plot 3 - A histogram with normal distribution for studentized residuals
  hist(resid, freq=FALSE, main='Residuals distribution',
       ylab='Frequency',xlab='Residual')
  xfit<-seq(min(resid)*1.1,max(resid)*1.1,length=80)
  yfit<-dnorm(xfit,mean=mean(resid),sd=sd(resid))
  lines(xfit, yfit,col="blue",lwd=1)

  # Plot 4 - QQ-Plot
  qqnorm(resid, main='Normal Q-Q plot', ylab='Ordered residual',xlab='Normal quantile')
  qqline(resid,col='blue')

  par(mfrow=c(1,1))
}
