#' To obtain Stand and Individual variables for feeding the growth tree model in DBH
#'
#' @param ID Array of identification of each tree
#' @param area Array of Area of the plot
#' @param sp Array of sp of plot
#' @param DBH Array of DBHs
#' @param ZONA Array of zona
#'
#' @return 8 different predictors ID, DBH, ba (basal area individual, m2/ha), bac (basal area individual cohorte, m2/ha),
#' SPZONA (concatenation), N (number of trees per ha), BA (basal area, m2/ha), QD (quadratic diameter, cm), 
#' SDI (Stand Density Index), BAL (Basal Area Larges trees, m2/ha), BALc (Basal Area Larges trees, m2/ha), PSCAL (BAL/BA)
#'
#' @examples
#' ID<-c(1,2,3)
#' area<-c(25,25,25)
#' sp<-c(1,1,2)
#' DBH<-c(15,25,37)
#' ZONA<-c(1,1,1)
#' COV<-covariates(ID,area,sp,DBH,ZONA);COV


## Code for obtaining covariates from ingrowth DBH models
## Require plot with id tree, area, sp, DBH and zone


