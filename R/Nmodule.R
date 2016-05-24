#' Projects number of trees (N) from stand level parameters to the next year
#'
#' \code{Nmodule} Evaluates stand level input variables to project number of trees per hectare to the following year.
#' This is a mortality model, as it does not incorporate ingrowth.
#'
#' @param N0 Number of trees (trees/ha) of the stand at current time.
#' @param QD0 Quadratic Diameter (m2/ha) of the stand at current time.
#' @param model available fitted model to use for estimation.
#'
#' @return Number of trees (N1, m2/ha) for the next year (at EDOM0+1)
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' N1<-Nmodule(N0=2730,QD0=12.43,model=1)
#' N1

Nmodule <- function(N0=NA,QD0=NA,model=1){

  # Model: log(n_trees_ha2) = log(n_trees_ha1)*(1 - theta*Delta.ANHO*(dq1/dq_max_original))
  theta<-0.0055452 # Based on model 1, but we might have more?

  QDmax<-QD0*1.5;  # This needs to change to correct calculation maybe extra input
  N1<-exp(log(N0)*(1-theta*(QD0/QDmax)))

  return(N1)
}

