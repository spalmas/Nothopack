#' Estimates the total height of trees for a single plot.
#'
#' \code{tree.HT} Estimates the total height of trees that have missing height from tree-level data.
#' For the missing trees there are two methods to use: 1) Estimates heights according
#' to a parametrized height-DBH model, or 2) Estimates heights by fitting a simple height-DBH model that
#' requires at least 10 measurements. Missing values are indentified as 'NA', and regression is fitted for 
#' without making distinction by species.
#'
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param DBH    Vector of diameter at breast height (DBH, cm). Must be complete and have the same size and order as TREEID.
#' @param HT     Vector of total height (m). Must be of the same size and order as TREEID.
#' @param AREA   Numeric value of area of the inventory plot (m2).
#' @param AGE    Numeric value of stand age (years). Required if method = 1.
#' @param BA     Numeric value of Basal Area (m2/ha). Required if method = 1.
#' @param method Numeric value that identifies the method to estimate missing heights. 1: parametrized DBH-height model
#' that requires DBH, BA and AGE, 2: fits a simple DBH-height model from available measurements using the equation:
#' ln(HT) = b0 + b1/DBH. Default method = 1.
#'
#' @return A list containing the following:
#' \itemize{
#' \item \code{HTFIN} A vector of final tree heigths (m), replacing the missing values for estimated heigths and retaining the observed heights.
#' \item \code{r2} A value with the coefficient of determination from the fitting the DBH-height model when method = 2.
#' }
#'
#' @references
#' Moreno-Gezan XXXX
#' 
#' @examples
#' # Example 1 - Method 1 - Parametrized DBH-height model
#' DBH <- c(9.3,11.1,15.5,9,14.8,27.3,11.4,6.6,12.6,17.5,6.3,7.2,11.5,13.6,7.3,12,11.9,8.1,7.6,5)
#' HT <- c(11.8,12.3,NA,NA,15.3,18,12,NA,14.5,NA,NA,NA,NA,NA,10.3,14.6,NA,NA,NA,NA)
#' tree.HT(DBH=DBH, HT=HT, DOM.SP=2, ZONE=2, HD=15, QD=12, method=1)
#'
#' # Example 2 - Method 2 - Simple DBH-height model
#' DBH <- c(9.3,11.1,15.5,9,14.8,27.3,11.4,6.6,12.6,17.5,6.3,7.2,11.5,13.6,7.3,12,11.9,8.1,7.6,5)
#' HT <- c(11.8,12.3,NA,NA,15.3,18,12,NA,14.5,NA,NA,NA,NA,NA,10.3,14.6,NA,NA,NA,NA)
#' tree.HT(DBH=DBH, HT=HT, method=2)


tree.ht  <-  function(DBH, HT, DOM.SP=NA, ZONE=NA, HD=NA, QD=NA, method=1){

                        if(length(DBH)==length(HT) & sum(is.na(DBH))==0){

                          if(method==1){

                            if(is.na(DOM.SP)==T | is.na(ZONE)==T | is.na(HD)==T | is.na(QD)==T){
                              stop("Warning - Incomplete information. Please check input.")
                            }

                            aux <- data.frame(DBH=DBH, HT=HT, HTEST=NA, HTFIN=NA)
                            aux$HTEST <- height_param(DOM.SP=DOM.SP, ZONE=ZONE, HD=HD, QD=QD, DBH=DBH)
                            aux$HTFIN <- ifelse(is.na(aux$HT),aux$HTEST,aux$HT)
                            r2 <- NA
                            
                          }

                          if(method==2){

                            aux <- data.frame(DBH=DBH, HT=HT)
                            aux2 <-subset(aux, HT!='NA')
                            model <- stats::lm(log(aux2$HT)~I(1/aux2$DBH),na.action='na.omit')
                            r2 <- summary(model)$r.squared
                            HTEST <- exp(model$coefficients[1]+model$coefficients[2]/DBH)
                            aux$HTFIN <- ifelse(is.na(aux$HT),HTEST,aux$HT)

                          }

                        } else {

                          stop("Warning - Incomplete information. Please check input.")

                          }

                  return(list(HTFIN=aux$HTFIN, r2=r2))

}
