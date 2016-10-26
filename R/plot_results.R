#' Module to plot results table
#' Creates many different plots that illustrate the simulation results
#'
#'
#'
#'
library(grid)
library(gridExtra)


plot_results <- function(SIM){

  N.plot <- ggplot(SIM, aes(x = Age, y = NHA)) + geom_line() +
    xlab("Year") + ylab("N (N/ha)") + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  BA.plot <- ggplot(SIM, aes(x = Age, y = BA)) + geom_line() +
    xlab("Year") + ylab("BA (m2)") + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  DQ.plot <- ggplot(SIM, aes(x = Age, y = QD)) + geom_line() +
    xlab("Year") + ylab("QD (cm)") + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  Vol.plot <- ggplot(SIM, aes(x = Age, y = VOL)) + geom_line() +
    xlab("Year") + ylab("Volume (m^3)")
  HD.plot <- ggplot(SIM, aes(x = Age, y = HD)) + geom_line() +
    xlab("Year") + ylab("Dominant Height (m)")

  grid.newpage()
  grid.arrange(N.plot, BA.plot, DQ.plot, Vol.plot, HD.plot,
               heights=c(2/4, 2/4, 2/4), ncol=2, nrow = 3)

}
