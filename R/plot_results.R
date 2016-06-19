#' Module to plot results table
#' Creates many different plots that illustrate the simulation results
#'
#'
#'
#'

#A results table can be obtain through the simulator_validation script.
#SIM is the name of the last stored results table

require (ggplot2)
require(grid)
SIM <- read.csv(file = 'data/results.csv', sep = ' ')   #example results table

plot_results <- function(results){

  BA.plot <- ggplot(SIM, aes(x = Age, y = BA)) + geom_line() +
    xlab("Year") + ylab("BA (m2)") + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  DQ.plot <- ggplot(SIM, aes(x = Age, y = QD)) + geom_line() +
    xlab("Year") + ylab("QD (cm)")
  #Vol.plot <- ggplot(SIM, aes(x = Age, y = Vol)) + geom_line() +
  #  xlab("Year") + ylab("QD (cm)")

  grid.newpage()
  #grid.draw(rbind(ggplotGrob(BA.plot), ggplotGrob(DQ.plot), ggplotGrob(Vol.plot), size = "last"))
  grid.draw(rbind(ggplotGrob(BA.plot), ggplotGrob(DQ.plot), size = "last"))

}
