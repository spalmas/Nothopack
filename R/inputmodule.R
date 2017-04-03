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
#' @param filename Name of the tree-level file with columns: ID, SP, DBH, H, PS
#' 
#' @return Series of data input and parameters to be required for downstream modules.
#'
#' @examples
#' # Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' inputmodule(level='stand',zone=2,AD=25,HD=23.4,N=N,BA=BA)
#'
#' # Example 2: Input from tree-level data (or file)
#' 
#' 
#' 

inputmodule <- function(level='stand', zone=NA, AD=NA, HD=NA, SI=NA, N=NA, BA=NA, QD=NA, 
                        AF=NA,area=0,comp=FALSE, filename=NA){

  DOM.SP<-get_domsp(BA=BA)
  if (is.na(AD)){
     (AD<-get_site(dom_sp=DOM.SP, zone=zone, HD=HD, SI=SI))
  }
  if (is.na(HD)){
     (HD<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, SI=SI))
  }
  if (is.na(SI)){
     (SI<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, HD=HD))  
  }

  # Gathering stand-level information
  if (level=='stand'){
  
    N<-c(N,sum(N))
    BA<-c(BA,sum(BA))
    QD<-c(QD,sum(QD))
    if (is.na(N[1])){
      (N[1]<-get_stand(QD=QD[1], BA=BA[1]))
      (N[2]<-get_stand(QD=QD[2], BA=BA[2]))
      (N[3]<-get_stand(QD=QD[3], BA=BA[3]))
      (N[4]<-get_stand(QD=QD[4], BA=BA[4]))
      (N[5]<-get_stand(QD=QD[5], BA=BA[5]))
    }
    if (is.na(BA[1])){
      (BA[1]<-get_stand(QD=QD[1], N=N[1]))
      (BA[2]<-get_stand(QD=QD[2], N=N[2]))
      (BA[3]<-get_stand(QD=QD[3], N=N[3]))
      (BA[4]<-get_stand(QD=QD[4], N=N[4]))
      (BA[5]<-get_stand(QD=QD[5], N=N[5]))
    }
    if (is.na(QD[1])){
      (QD[1]<-get_stand(BA=BA[1], N=N[1]))
      (QD[2]<-get_stand(BA=BA[2], N=N[2]))
      (QD[3]<-get_stand(BA=BA[3], N=N[3]))
      (QD[4]<-get_stand(BA=BA[4], N=N[4]))
      (QD[5]<-get_stand(BA=BA[5], N=N[5]))
    }
    
    v1 <- c((1:4),0)
    v2 <- round(N,6)
    v3 <- round(BA,6)
    v4 <- round(QD,6)
    sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
    names(sdmatrix) <- c('SPECIES','N','BA','QD')
    
    PBAN <- sum(BA[1:3])/sum(BA)
    PNHAN <- sum(N[1:3])/sum(N)
  }
  
  # Gathering tree-level information
  if (level=='tree'){
  
     # Reading file
    
    
    #PLOTS2$hest<-exp(4.62373-2.94247/PLOTS2$dbh)
    #PLOTS2$htfin<-PLOTS2$ht
    #for (i in 1:253) {
    #  if(is.na(PLOTS2$ht[i])) {
    #    PLOTS2$htfin[i]<-PLOTS2$hest[i]
    #  }
    #}
    #head(PLOTS2)
    
     # Ouput tree-list database
     # ID, SP, DBH, H, PS & (zone, AD, area, AF, DOM.SP, HD, SI)    
  }

  return(list(sd=sdmatrix, DOM.SP=DOM.SP, HD=HD, PBAN=PBAN, PNHAN=PNHAN, 
              SI=SI, AD=AD, AF=AF, area=area, comp=comp, zone=zone))

}

