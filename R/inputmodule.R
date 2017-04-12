#' Module that requests input information for all stand- or tree-level current calculations or simulations
#'
#' \code{inputmodule} Module that requests input information for all stand- or tree-level current
#' calculations or simulations from the user, and completes missing information as required.
#'
#' @param zone Growth zone (1, 2, 3, 4).
#' @param DOM.SP The dominant specie (1: Rauli, 2: Roble, 3: Coigue, 4:Mixed)
#' @param AD Dominant age (years) of the stand.
#' @param HD Dominant height (m) of the stand.
#' @param SI Site index at reference dominant age of 20 (m) of the stand.
#' @param N Vector of number of trees (trees/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others, 0: Total)
#' @param BA Vector of basal area (m2/ha) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others, 0: Total)
#' @param QD Vector of quadratic diameters (cm) of the stand (1: Rauli, 2: Roble, 3: Coigue, 4:Others, 0: Total)
#' @param PBAN Proportion of basal area in the stand that correspond to Nothofagus.
#' @param SDI Stand density index (trees/ha)
#' @param PNHAN Proportion of number of trees per hectarea in the stand that correspond to Nothofagus.
#' @param AF Final dominant age (years) for simulation
#' @param tree.list Optional tree-list for a plot with columns: ID, SPECIE, DBH, HT, SS, FT)
#' @param area Area of plot from tree-list data (m2).
#' @param type Type of simulation required: stand: only stand-level, tree: only tree-level, both: both modules (default=stand)
#' @param ddiam Logical for requesting generation of diameter distribution (default=FALSE)
#' @param comp Logical for requesting compatibility between stand- and tree-level simulations (default=FALSE)
#' 
#' @return Series of data input and parameters to be required for downstream modules. Particularly core module.
#'
#' @examples
#' # Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' inputmodule(type='stand',zone=2,AD=28,HD=23.5,N=N,BA=BA)
#'
#' # Example 2: Input from tree-level data (or file)
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' input.plot<-inputmodule(type='tree',zone=2,AD=28,HD=23.5,area=500,tree.list=plot)
#' attributes(input.plot) 
#' head(input.plot$tree.list) 
#' input.plot$sp.table

inputmodule <- function(zone=NA, DOM.SP=NA, AD=NA, HD=NA, SI=NA, sp.table=NA, 
                        SDI=NA, PBAN=NA, PNHAN=NA, AF=NA, tree.list=NA, area=0, 
                        type='stand', ddiam=FALSE, comp=FALSE, 
                        N_model=1, V_model=1, IADBH_model=1,
                        N=NA, BA=NA, QD=NA){
  
  sdmatrix <- NA

  # Gathering stand-level information
  if (type=='stand'){
  
    N[5]<-sum(N)
    BA[5]<-sum(BA)
    QD[5]<-sum(QD)
    
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
   
    DOM.SP<-get_domsp(BA=BA[1:4])
    if (is.na(AD)){
      (AD<-get_site(dom_sp=DOM.SP, zone=zone, HD=HD, SI=SI))
    }
    if (is.na(HD)){
      (HD<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, SI=SI))
    }
    if (is.na(SI)){
      (SI<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, HD=HD))  
    }
      
    v1 <- c((1:4),0)
    v2 <- round(N,6)
    v3 <- round(BA,6)
    v4 <- round(QD,6)
    sdmatrix <- data.frame(cbind(v1,v2,v3,v4))
    names(sdmatrix) <- c('SPECIE','N','BA','QD')
    
    PBAN <- sum(BA[1:3])/sum(BA)
    PNHAN <- sum(N[1:3])/sum(N)
    SDI <- N[5]*(25.4/QD[5])^-1.4112   # This needs to be checked
    
  }
  
  # Gathering tree-level information
  if (type=='tree'){
  
    # Getting stand level parms from tree-list
    plotdata<-data.frame(tree.list$SPECIE, tree.list$DBH, tree.list$HT)
    colnames(plotdata)<-c('SPECIES','DBH','HT')
    params<-stand_parameters(plotdata=plotdata,area=area)
    
    DOM.SP<-get_domsp(BA=params$sd[1:4,3])
    if (is.na(AD)){
      (AD<-get_site(dom_sp=DOM.SP, zone=zone, HD=HD, SI=SI))
    }
    if (is.na(HD)){
      (HD<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, SI=SI))
    }
    if (is.na(SI)){
      (SI<-get_site(dom_sp=DOM.SP, zone=zone, AD=AD, HD=HD))  
    }
    
    # Completing heights
    QD0<-params$sd[5,4]
    n<-nrow(tree.list)
    for (i in (1:n)) {
      if(is.na(tree.list$HT[i])) {
        tree.list$HT[i]<-round(height_param(dom_sp=DOM.SP, zone=zone, HD=HD, QD=QD0, DBH=tree.list$DBH[i]),4)
      }
    }

    # Ouput tree-list database
    FT<-rep(1,length(tree.list$ID))
    tree.list<-data.frame(tree.list$ID, tree.list$SPECIE, tree.list$DBH, 
                          round(tree.list$HT,3), tree.list$SS, FT)
    colnames(tree.list)<-c('ID','SPECIE','DBH','HT','SS','FT')
    
    # Collecting final stand parameters
    sdmatrix <- params$sd  
    PBAN <- sum(sdmatrix[1:3,3])/sum(sdmatrix[1:4,3])
    PNHAN <- sum(sdmatrix[1:3,2])/sum(sdmatrix[1:4,2])
    
    }

  return(list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
              area=area, type=type, ddiam=ddiam, comp=comp, N_model=N_model, V_model=V_model, 
              IADBH_model=IADBH_model, sp.table=sdmatrix, tree.list=tree.list))
  
}

# Note, still to decide how to complete the PS
# Note, SDI needs to be completed.
# HD can be calculated form the tree.list but HT does not contain missing.

