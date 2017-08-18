#' Module that coordinates the communication between the different updating and simulation modules
#'
#' \code{core_module} Module that coordinates the communication between the different critical modules of
#' updating and simulation including the stand- or tree-level current. It works as the core between input,
#' calculations, simulations and output. Some characteristics are:
#' 1) Requires initially all elements that original from an input module that is assumes to be complete.
#' 2) Input/output can be tree-list or stand parameters.
#' 3) If input is tree-list: updates stand-parameters, obtains species and stand-table
#' 4) If input is stand parameters: obtain species and stand-table (if required)
#' 5) Update species and stand-table to include volume (stand- and tree-level)
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
#' @param input List created by inputmodule, that is used to pass the variables as a list
#' @param NHA_model
#' @param V_model
#' @param stand_simulation XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#'
#' @return A series of elements and parameters with updated tables (adding volume). The main output elmenets are:
#'                 sp.table    table with stand level summary by species (1,2,3,4) and total (0), and volume
#'                 stand.tbale data feame with the complete stand table by specie in classes of 5 cm.
#'                 tree.list   data frame with complete tree list
#'                 input       list with all parameters of input and stand level statistics
#'                             (zone, DOM.SP, AD, HD, SI, SDI, PBAN, PNHAN, AF, area, type, ddiam, comp, NHA_model,
#'                             V_model,IADBH_model,start_time)
#'
#' @examples
#' # Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,0.0,2.4)
#' N<-c(464,23,0,48)
#' plot<-input_module(type='stand',zone=1,AD=28,AF=40,HD=23.5,N=N,BA=BA,V_model=2,ddiam=FALSE)
#' plot$sp.table
#' # Without generation of stand-table
#' core.stand<-core_module(input=plot$input)
#' core.stand$sp.table
#' # With generation of stand-table
#' plot$input$ddiam<-TRUE
#' core.stand<-core_module(input=plot$input)
#' core.stand$stand.table[5,,]
#' core.stand$input
#'
#' # Example 2: Input from tree-level data (or file)
#' plot2<- read.csv(file= 'data/Plot_example.csv')
#' plot2<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,tree.list=plot2)
#' plot2$sp.table
#' head(plot2$tree.list)
#' core.tree<-core_module(input=plot2$input, type='tree')
#' core.tree$sp.table
#' core.tree$stand.table[5,,]
#' head(core.tree$tree.list)

core_module <- function(zone=NA, DOM.SP=NA, AD=NA, HD=NA, SI=NA, sp.table=NA,
                        SDI=NA, PBAN=NA, PNHAN=NA, AF=NA, tree.list=NA, area=0,
                        type='stand', ddiam=FALSE, comp = FALSE,
                        NHA_model=1, V_model=1, IADBH_model=1, input=NA,
                        stand_simulation=NA){


  # this is in case input is the only variable.
  # We can delete all variables in function definition
  # core module only adds volume?
  if (is.list(input)){
    zone <- input$zone
    DOM.SP <- input$DOM.SP
    AD <- input$AD
    HD <- input$HD
    SI <- input$SI
    sp.table <- input$sp.table
    SDI <- input$SDI
    PBAN <- input$PBAN
    PNHAN <- input$PNHAN
    AF <- input$AF
    tree.list <- input$tree.list
    area <- input$area
    type <- input$type
    ddiam <- input$ddiam
    comp <- input$comp
    NHA_model <- input$NHA_model
    V_model <- input$V_model
    IADBH_model <- input$IADBH_model
    stand_simulation <- input$stand_simulation
    start_time <- input$start_time
  }

  stand.table <- NA

  if (type=='stand'){    # tree.list not provided then generate one - always

    if (ddiam==FALSE) {

      # Calculating Stand-level volume (total)
      sp.table<-cbind.data.frame(sp.table,VTHA=c(1:5)*0)
      if (V_model==1){ 
        VTHA <- Vmodule(BA=sp.table$BA[5], HD=HD, PNHAN=PNHAN)
      } else {
        VTHA <- Vmodule(BA=sp.table$BA[5], HD=HD) 
      }
      sp.table[5,5]<-round(VTHA,3)

      # Assigning volume proportional to PBA
      PBA<-sp.table[1:4,3]/sp.table[5,3]
      sp.table[1:4,5]<-round(VTHA*PBA,3)

      DDist=NA

    }

    if (ddiam==TRUE) {

      # Generate stand-table from diameter distibution (does not contain volumes)
      stand.table<-diam_dist(sp.table=sp.table, HD=HD, DOM.SP=DOM.SP, zone=zone)
      m <- length(stand.table[1,,1])

      # Calculating class-level volume (total) by specie and class
      r_names<-c('DBH_ll','DBH_ul','D_class','H_class','N','BA','VT')
      DDist<-array(data=NA, dim=c(5,m,7), dimnames=list(c(1:5),c(1:m),r_names))
      DDist[,,-7]<-stand.table

      minN <- 0.1 # minimum Number of trees per diameter class to consider
      for (i in 1:m) {
        if (stand.table[1,i,5] < minN){ Vi.sp1<-0
        } else { Vi.sp1<-Vmodule_individual(SPECIES=1, zone=zone, DBH=stand.table[1,i,3],
                                            HT=stand.table[1,i,4], blength=stand.table[1,i,4]) }
        if (stand.table[2,i,5] < minN){ Vi.sp2<-0
        } else { Vi.sp2<-Vmodule_individual(SPECIES=2, zone=zone, DBH=stand.table[2,i,3],
                                            HT=stand.table[2,i,4], blength=stand.table[2,i,4]) }
        if (stand.table[3,i,5] < minN){ Vi.sp3<-0
        } else { Vi.sp3<-Vmodule_individual(SPECIES=3, zone=zone, DBH=stand.table[3,i,3],
                                            HT=stand.table[3,i,4], blength=stand.table[3,i,4]) }
        if (stand.table[4,i,5] < minN){ Vi.sp4<-0
        } else { Vi.sp4<-Vmodule_individual(SPECIES=DOM.SP, zone=zone, DBH=stand.table[4,i,3],
                                            HT=stand.table[4,i,4], blength=stand.table[4,i,4])}
        DDist[1,i,7]<-round(Vi.sp1*stand.table[1,i,5],3)
        DDist[2,i,7]<-round(Vi.sp2*stand.table[2,i,5],3)
        DDist[3,i,7]<-round(Vi.sp3*stand.table[3,i,5],3)
        DDist[4,i,7]<-round(Vi.sp4*stand.table[4,i,5],3)
        DDist[5,i,7]<-DDist[1,i,7]+DDist[2,i,7]+DDist[3,i,7]+DDist[4,i,7]
      }

      # Assigning volume from generated stand-table
      sp.table<-cbind.data.frame(sp.table,VTHA=c(1:5)*0)
      sp.table[1,5]<-round(sum(DDist[1,,7]),3)
      sp.table[2,5]<-round(sum(DDist[2,,7]),3)
      sp.table[3,5]<-round(sum(DDist[3,,7]),3)
      sp.table[4,5]<-round(sum(DDist[4,,7]),3)
      sp.table[5,5]<-round(sum(DDist[5,,7]),3)

    }
  }

  if (type=='tree'){   # tree.list is provided

    params<-stand_parameters1(plotdata=tree.list, area=area)
    # Collecting final stand parameters
    sp.table <- params$sd
    PBAN <- sum(sp.table[1:3,3])/sum(sp.table[1:4,3])
    PNHAN <- sum(sp.table[1:3,2])/sum(sp.table[1:4,2])

    n <- length(tree.list$ID)
    vind <- matrix(NA,nrow=n)

    # Completing heights using parametrized height-dbh model
    QD0<-params$sd[5,4]
    for (i in (1:n)) {
      if(is.na(tree.list$HT[i])) {
        tree.list$HT[i]<-round(height_param(dom_sp=DOM.SP, zone=zone, HD=HD, QD=QD0, DBH=tree.list$DBH[i]),4)
      }
    }

    #if (is.na(area) ){ stop('Plot area must be provided') }
    #CF <- 10000 / area  # Correction factor
    #baind <- as.numeric(pi*(tree.list$DBH/2)^2/10000)  # Units: m2
    for (i in 1:n) {
      if (tree.list$SPECIES[i]==4) { SP<-DOM.SP
      } else { SP<-tree.list$SPECIES[i] }
      vind[i] <- Vmodule_individual(SPECIES=SP, zone=zone, DBH=tree.list$DBH[i],
                                    HT=tree.list$HT[i], blength=tree.list$HT[i])
    }
    Dclass <- ceiling(tree.list$DBH/5)*5-2.5
    N <- tree.list$FT
    BA <- params$BAind
    VT <- N*vind
    tree.list<-data.frame(tree.list,BA,VT,Dclass)
    HT.agg<-aggregate(HT~Dclass+SPECIES,FUN=mean,data=tree.list)
    N.agg<-aggregate(N~Dclass+SPECIES,FUN=sum,data=tree.list)
    BA.agg<-aggregate(BA~Dclass+SPECIES,FUN=sum,data=tree.list)
    VT.agg<-aggregate(VT~Dclass+SPECIES,FUN=sum,data=tree.list)
    data.agg<-data.frame(HT.agg,N.agg[3],BA.agg[3],VT.agg[3])

    class <- 5
    diam <- seq(from=5,to=90,by=class)   # Diameter classes
    nclass <- length(diam)-1
    DBH_LL <- matrix(data=0,nrow=nclass,ncol=1)
    DBH_UL <- matrix(data=0,nrow=nclass,ncol=1)
    Dclass <- matrix(data=0,nrow=nclass,ncol=1)
    BAclass <- matrix(data=0,nrow=nclass,ncol=1)
    Hclass <- matrix(data=0,nrow=nclass,ncol=1)
    Vclass <- matrix(data=0,nrow=nclass,ncol=1)

    for (j in 1:(nclass)){
      DBH_LL[j] <- diam[j]                 # cm
      DBH_UL[j] <- diam[j+1]               # cm
      Dclass[j] <- (diam[j]+diam[j+1])/2   # cm
    }
    head<-data.frame(DBH_LL,DBH_UL,Dclass)

    r_names<-c('DBH_ll','DBH_ul','D_class','H_class','N','BA','VT')
    DDist<-array(data=NA, dim=c(5,(length(diam)-1),7),
                 dimnames=list(c(1:5),c(1:(length(diam)-1)),r_names))

    data.agg1<-data.agg[which(data.agg$SPECIES==1),]
    Dclass1<-merge(head,data.agg1,by="Dclass",all=TRUE)
    data.agg2<-data.agg[which(data.agg$SPECIES==2),]
    Dclass2<-merge(head,data.agg2,by="Dclass",all=TRUE)
    data.agg3<-data.agg[which(data.agg$SPECIES==3),]
    Dclass3<-merge(head,data.agg3,by="Dclass",all=TRUE)
    data.agg4<-data.agg[which(data.agg$SPECIES==4),]
    Dclass4<-merge(head,data.agg4,by="Dclass",all=TRUE)

    Dclass1<-data.frame(head,HT=round(Dclass1[,5],2),N=round(Dclass1[,6],2),BA=round(Dclass1[,7],2),VT=round(Dclass1[,8],3))
    Dclass2<-data.frame(head,HT=round(Dclass2[,5],2),N=round(Dclass2[,6],2),BA=round(Dclass2[,7],2),VT=round(Dclass2[,8],3))
    Dclass3<-data.frame(head,HT=round(Dclass3[,5],2),N=round(Dclass3[,6],2),BA=round(Dclass3[,7],2),VT=round(Dclass3[,8],3))
    Dclass4<-data.frame(head,HT=round(Dclass4[,5],2),N=round(Dclass4[,6],2),BA=round(Dclass4[,7],2),VT=round(Dclass4[,8],3))
    Dclass5<-Dclass4

    for (j in 1:(nclass)){
      if (is.na(Dclass1$HT[j])) {
        Dclass1$HT[j]=0
        Dclass1$N[j]=0
        Dclass1$BA[j]=0
        Dclass1$VT[j]=0
      }
      if (is.na(Dclass2$HT[j])) {
        Dclass2$HT[j]=0
        Dclass2$N[j]=0
        Dclass2$BA[j]=0
        Dclass2$VT[j]=0
      }
      if (is.na(Dclass3$HT[j])) {
        Dclass3$HT[j]=0
        Dclass3$N[j]=0
        Dclass3$BA[j]=0
        Dclass3$VT[j]=0
      }
      if (is.na(Dclass4$HT[j])) {
        Dclass4$HT[j]=0
        Dclass4$N[j]=0
        Dclass4$BA[j]=0
        Dclass4$VT[j]=0
      }

    }

    Dclass5$N=Dclass1$N+Dclass2$N+Dclass3$N+Dclass4$N
    Dclass5$BA=Dclass1$BA+Dclass2$BA+Dclass3$BA+Dclass4$BA
    Dclass5$VT=Dclass1$VT+Dclass2$VT+Dclass3$VT+Dclass4$VT
    Dclass5$HT=round((Dclass1$N*Dclass1$HT+Dclass2$N*Dclass2$HT+Dclass3$N*Dclass3$HT+Dclass4$N*Dclass4$HT)/Dclass5$N,2)

    DDist[1,,]<-as.matrix(Dclass1)  #1: Rauli
    DDist[2,,]<-as.matrix(Dclass2)  #2: Roble
    DDist[3,,]<-as.matrix(Dclass3)  #3: Coigue
    DDist[4,,]<-as.matrix(Dclass4)  #4: Others
    DDist[5,,]<-as.matrix(Dclass5)  #0: Total

    # Assigning volume from generated stand-table
    sp.table<-cbind.data.frame(sp.table,VTHA=c(1:5)*0)
    sp.table[1,5]<-round(sum(DDist[1,,7]),3)
    sp.table[2,5]<-round(sum(DDist[2,,7]),3)
    sp.table[3,5]<-round(sum(DDist[3,,7]),3)
    sp.table[4,5]<-round(sum(DDist[4,,7]),3)
    sp.table[5,5]<-round(sum(DDist[5,,7]),3)

  }

  # Need to add compatibility between tree and stand


  # Special calculations if the input is in form stand_simulation
  # (if the stand_simulation exists)
  # Probably needs to be before to avoid some errors
  if (is.list(stand_simulation)){

  }

  # List that is output from here input somewhere else
  input <- list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
                area=area, type=type, ddiam=ddiam, comp=comp, NHA_model=NHA_model, V_model=V_model,
                IADBH_model=IADBH_model, start_time=start_time, sp.table=sp.table, stand.table=DDist, tree.list=tree.list )


  return(list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
              area=area, type=type, ddiam=ddiam, comp=comp, NHA_model=NHA_model, V_model=V_model,
              IADBH_model=IADBH_model, start_time = start_time,
              sp.table=sp.table, stand.table=DDist, tree.list=tree.list, input=input))
}

# Note: V_model=1 or anything else is always the same
# Note: it needs some standarization on output of sp.table and stand.table between stand and tree-level (e.g. SPECIE or SPECIES)
# Deifnition of stand simulation is not incorporated
# This module should now separate VT for all products(additional columns)
