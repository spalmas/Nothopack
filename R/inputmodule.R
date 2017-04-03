#' Module that requests input information for all stand- or tree-level current calculations or simulations
#'
#' \code{inputmodule} Module that requests input information for all stand- or tree-level current
#' calculations or simulations
#'
#' @param level Data level for input information (1:tree, 2:stand).
#' @param zone Growth zone (1, 2, 3, 4).
#' @param area Area of plot for tree level data (m2).
#' @param AD Dominant age (years) of the stand.
#' @param HD Dominant height (m) of the stand.
#' @param SI Site index at reference dominant age of 20 (m) of the stand.
#' @param N Vector of number of trees (trees/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others)
#' @param BA Vector of basal area (m2/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others)
#' @param QD Vector of quadratic diameters (cm) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others)
#' @param AF Final dominant age (years) for simulation
#' @param comp Logical for requesting compatibility between stand- and tree-level simulations (default=FALSE)
#' @param AF Final dominant age (years) for simulation
#' 
#' 
#' @return Series of data input and parameters to be required for downstream modules.
#'
#' @examples
#' # Example 1: Input from tree list file
#' BAest<-BAmodule(AD0=20,HD0=17.20,N0=2730,model=1,projection=FALSE)
#' BAest$BA0
#'
#' # Example 2: Projects Basal Area
#' (BAest<-BAmodule(AD0=20,HD0=17.20,N0=2730,BA0=33.11,HD1=19.1,N1=2610,model=1,projection=TRUE)$BA1)
#' (BAest<-BAmodule(AD0=20,HD0=17.20,N0=2730,BA0=33.11,HD1=19.1,N1=2610,model=2,projection=TRUE)$BA1)

v1 <- c((1:4),0)
v2 <- round(c(N1,N2,N3,N99,N0),6)
v3 <- round(c(BA1,BA2,BA3,BA99,BA0),6)
v4 <- round(c(QD1,QD2,QD3,QD99,QD0),6)
sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
names(sdmatrix) <- c('SPECIES','N','BA','QD')

inputmodule <- function(level='stand', zone=NA, AD=NA, HD=NA, SI=NA, N=NA, BA=NA, QD=NA, 
                        AF=NA,area=0,comp=FALSE){

  DOM.SP<-get_domsp()
  if (is.na(AD)){
     (AD<-get_site(dom_sp=DOM.SP, zone=zone, HD=HD, SI=SI))
  }
  if (is.na(HD)){
     (HD<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, SI=SI))
  }
  if (is.na(SI)){
     (SI<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, HD=HD))  
  }

  # Reading from stand-level information  
  if (level=='stand'){
    v1 <- c((1:4),0)
    v2 <- round(c(N[1],N[2],N[3],N[4],sum(N)),6)
    v3 <- round(c(BA[1],BA[2],BA[3],BA[4],sum(BA)),6)
    v4 <- round(c(QD[1],QD[2],QD[3],QD[4],sum(QD)),6)
    sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
    names(sdmatrix) <- c('SPECIES','N','BA','QD')
  }
  
  
  # Reading from tree-level information
  if (level=='tree'){
    bm<-bm2
  }


  return(list(BA0=BA0,BA1=BA1))
}

