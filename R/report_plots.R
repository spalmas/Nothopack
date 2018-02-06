#' Prints a chosen set of graphs and tables with the simulation results.
#'
#' \code{report_plots} Receives data core_stand and outputs a set of chosen graphs and tables
#
#' @param core_stand An object from core_module that has been simulated with stand_simulator or tree_simulator
#'
#' @return Prints pretty report graphs and tables
#'
#' @author
#' S. Palmas, S.A. Gezan and P. Moreno
#'
#' @examples
#' # Example 1: From stand_simulator
#' BA<-c(1.09,38.92,0,0.31)
#' N<-c(60,780,0,80)
#' input<-input_module(type='stand',zone=2,AD=28,HD=15.5,N=N,BA=BA,AF=35,V_model=1)
#' core.stand<-core_module(input = input)
#' stand_simulation<-stand_simulator(core.stand = core.stand)
#' report(core.stand = stand_simulation)
#'
report_plots <- function(SIM){

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
