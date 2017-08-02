#' Module that requests input information for all stand- or tree-level current calculations or simulations
#'
#' \code{inputmodule} Module that requests input information for all stand- or tree-level current
#' calculations or simulations from the user, and completes some of the missing information that is required later.
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
#' @return Series of data input and parameters to be required for downstream modules. Particularly core module. The main 
#' output elements are: sp.table    table with stand level summary by species (1,2,3,4) and total (0)
#'                      tree.list   data frame with complete tree list with missing values completed (e.g. HT)
#'                      input       list with all parameters of input and stand level statistics
#'                                  (zone, DOM.SP, AD, HD, SI, SDI, PBAN, PNHAN, AF, area, type, ddiam, comp, N_model,
#'                                  V_model,IADBH_model,start_time)
#'
#' @examples
#' # Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' input<-inputmodule(type='stand',zone=2,AD=28,HD=23.5,N=N,BA=BA)
#' input
#' input$sp.table
#'
#' # Example 2: Input from tree-level data (or file)
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' plot<-inputmodule(type='tree',zone=2,AD=28,HD=23.5,area=500,tree.list=plot)
#' attributes(plot)
#' head(plot$tree.list)
#' plot$sp.table
#' plot$input

inputmodule <- function(zone=NA, DOM.SP=NA, AD=NA, HD=NA, SI=NA, sp.table=NA,
                        SDI=NA, PBAN=NA, PNHAN=NA, AF=NA, tree.list=NA, area=0,
                        type='stand', ddiam=FALSE, comp=FALSE,
                        N_model=1, V_model=1, IADBH_model=1,
                        N=NA, BA=NA, QD=NA){

  start_time <- Sys.time()   #Useful to estimate time that the simulation takes

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
    if (DOM.SP==99) { warning("This stand is not dominated by Nothofagus",call. = FALSE)
      #stop("This stand is not dominated by Nothofagus",call.= TRUE)
      return()
      }
    
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

    PBAN <- sum(BA[1:3])/(BA[5])
    PNHAN <- sum(N[1:3])/(N[5])
    SDI <- N[5]*(25.4/QD[5])^-1.4112   # This needs to be checked for model selected

  }

  # Gathering tree-level information
  if (type=='tree'){

    # Getting stand level parms from tree-list
    plotdata<-data.frame(tree.list$SPECIE, tree.list$DBH, tree.list$HT, tree.list$FT)
    colnames(plotdata)<-c('SPECIES','DBH','HT','FT')
    plotdata$FT<-plotdata$FT* (10000/area)
    params<-stand_parameters1(plotdata=plotdata,area=area)
    
    # ## Getting Individual heigths by linear regression
    # # Linear Regression
    # # model log(ht)=b0+b1/dbh
    # plotdata$x<-1/plotdata$DBH
    # plotdata$y<-log(plotdata$HT)
    # modelo<-lm(y~x,data=plotdata)
    # modelo
    # plotdata$hest<-exp(modelo$coefficients[1]-modelo$coefficients[2]/plotdata$DBH)
    # plotdata$htfin<-plotdata$HT
    # 
    # for (i in 1:(length(plotdata$HT))) {
    #   if(is.na(plotdata$HT[i])) {
    #     plotdata$htfin[i]<-plotdata$hest[i]
    #   }
    # }
    # plotdata$HT<-plotdata$htfin

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

    # Completing heights using parametrized height-dbh model
    QD0<-params$sd[5,4]
    n<-nrow(tree.list)
    for (i in (1:n)) {
     if(is.na(tree.list$HT[i])) {
       tree.list$HT[i]<-round(height_param(dom_sp=DOM.SP, zone=zone, HD=HD, QD=QD0, DBH=tree.list$DBH[i]),4)
     }
    }

    # Ouput tree-list database
    FT<-rep(1,length(tree.list$ID))* (10000/area)
    tree.list<-data.frame(tree.list$ID, tree.list$SPECIE, tree.list$DBH,
                          round(tree.list$HT,3), tree.list$SS, FT)
    colnames(tree.list)<-c('ID','SPECIES','DBH','HT','SS','FT')

    # Collecting final stand parameters
    sdmatrix <- params$sd
    PBAN <- sum(sdmatrix[1:3,3])/sum(sdmatrix[1:4,3])
    PNHAN <- sum(sdmatrix[1:3,2])/sum(sdmatrix[1:4,2])

    }

  # List that is output from here input somewhere else
  input <- list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
                    area=area, type=type, ddiam=ddiam, comp=comp, N_model=N_model, V_model=V_model,
                    IADBH_model=IADBH_model,start_time=start_time, sp.table=sdmatrix, tree.list=tree.list)
  
  return(list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
              area=area, type=type, ddiam=ddiam, comp=comp, N_model=N_model, V_model=V_model,
              start_time=start_time,
              IADBH_model=IADBH_model, sp.table=sdmatrix, tree.list=tree.list, input=input))

}

# Note, still to decide how to complete the PS (SS, from covariates code)
# Note, SDI needs to be checked (also, should we put SDI%, depends on DOM.SP)
# Note, what happens if the user wants to calculate HD form tree.data.

