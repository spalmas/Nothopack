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
#' @param input List created by inputmodule, that is used to pass the variables as a list
#'
#' @return A series of elements and parameters with updated tables (adding volume). The main output elmenets are:
#'                 sp.table    table with stand level summary by species (1,2,3,4) and total (0), and volume
#'                 stand.table data frame with the complete stand table by specie in classes of 5 cm.
#'                 tree.list   data frame with complete tree list
#'                 input       list with all parameters of input and stand level statistics
#'                             (ZONE, DOM.SP, AD, HD, SI, SDI, PBAN, PNHAN, AF, area, type, ddiam, comp, NHA_model,
#'                             V_model,IADBH_model,start_time)
#'
#' @examples
#' #Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' stand.input<-input_module(ZONE=2, AD=28, HD=18.5, AF=35, N=N, BA=BA, type='stand', ddiam=FALSE)
#' sims.stand<-core_module(input=stand.input)
#' sims.stand
#' stand.input$ddiam=TRUE
#' report(core.stand=sims.stand)
#'
#' #Example 2: Input from tree-level data
#' tree.list<-read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, type='tree', area=500, tree.list=tree.list, Hest_method=1, ddiam=FALSE)
#' sims.tree<-core_module(input=input)
#' head(sims.tree$tree.list)
#' plot$sp.table
#' sims.tree$sp.table
#' sims.tree$DDdist[5,,]

#' #Example 3: Input from stand-level data
#' BA<-c(36.5,2.8,0.0,2.4)
#' N<-c(464,23,0,48)
#' input<-input_module(type='stand',ZONE=1,AD=28,AF=40,HD=23.5,N=N,BA=BA,V_model=2,ddiam=FALSE)
#' input$sp.table
#' # Without generation of stand-table
#' core.stand<-core_module(input=input)
#' core.stand$sp.table
#' # With generation of stand-table
#' input$ddiam<-TRUE
#' core.stand<-core_module(input=input)
#' core.stand$DDist[5,,]
#' core.stand
#'
#' # Example 2: Input from tree-level data (or file)
#' plot2<- read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(type='tree',ZONE=2,AD=28,AF=28,HD=15.5,area=500,tree.list=plot2)
#' input$sp.table
#' head(input$tree.list)
#' core.tree<-core_module(input=input)
#' core.tree$sp.table
#' core.tree$DDist[5,,]    #NOT WORKING
#' head(core.tree$tree.list)
#' # Ploting distribution for all species
#' barplot(as.matrix(core.tree$DDist[5,,5]), main='Diameter Distribution all species', xlab='DBH Class', beside=TRUE, col=4)
#'
#' New Example - Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' input<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, N=N, BA=BA, type='stand')
#' input$sp.table
#' input$ddiam<-TRUE # Requesting diameter distribution
#' core_module(input=input)$DDist[5,,]
#' core_module(input=input)
#'
#' New Example - Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' input<-input_module(ZONE=2, AD=28, HD=23.5, AF=40, N=N, BA=BA, type='stand', comp=FALSE, ddiam=FALSE)
#' input$sp.table
#' core_module(input=input)
#'
#' #' New Example - Compatibility
#' plot2<- read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(type='tree',ZONE=2,AD=28,AF=28,HD=15.5,area=500,tree.list=plot2, comptype='PG')
#' input$sp.table
#' head(input$tree.list)
#' core.tree<-core_module(input=input)
#' core.tree$sp.table
#' core.tree$DDist[5,,]
#' head(core.tree$tree.list)

core_module <- function(input=NULL){
  # core module only adds volume?

  ZONE <- input$ZONE
  DOM.SP <- input$DOM.SP
  AD <- input$AD
  HD <- input$HD
  SI <- input$SI
  SDIP <- input$SDIP  # Fix is now SDIP
  PBAN <- input$PBAN
  PNHAN <- input$PNHAN
  AF <- input$AF
  area <- input$area
  type <- input$type
  ddiam <- input$ddiam
  comptype <- input$comptype
  NHA_model <- input$NHA_model
  V_model <- input$V_model
  IADBH_model <- input$IADBH_model
  sp.table <- input$sp.table
  tree.list <- input$tree.list

  stand.table <- NA

  # Flow with options: type (stand, tree), ddiam (TRUE, FALSE), comp (TRUE, FALSE)

  #############################
  # STAND simulation (no comp.)
  if (type=='stand') {

    # Perform simulations form AD to AF
    sims <- stand_simulator(core.stand=input)

    # Calculates Stand-level volume for simulations (by stand-level equations)
    if (ddiam==FALSE) {

      # Calculating Stand-level volume (total)
      sims$sp.table<-cbind(sims$sp.table,VTHA=c(1:5)*0)
      if (V_model==1){
        VTHA <- Vmodule(BA=sims$sp.table$BA[5], HD=sims$HD, PNHAN=sims$PNHAN)
      } else {
        VTHA <- Vmodule(BA=sims$sp.table$BA[5], HD=sims$HD)
      }
      sims$sp.table[5,5]<-round(VTHA,6)

      # Assigning volume proportional to PBA
      PBA<-sims$sp.table[1:4,3]/sims$sp.table[5,3]
      sims$sp.table[1:4,5]<-round(VTHA*PBA,6)

      sims$DDdist<-NA

    }
    # Calculates Stand-level volume for simulations (by generating diameter distributions)
    if (ddiam==TRUE) {

      # Generate stand-table from diameter distibution (does not contain volumes)
      stand.table<-diam_dist(sp.table=sims$sp.table, HD=HD, DOM.SP=DOM.SP, ZONE=ZONE)
      m <- length(stand.table[1,,1])

      # Calculating class-level volume (total) by specie and class
      r_names<-c('DBH_ll','DBH_ul','D_class','H_class','N','BA','VT')
      DDist<-array(data=NA, dim=c(5,m,7), dimnames=list(c(1:5),c(1:m),r_names))
      DDist[,,-7]<-stand.table

      minN <- 0.1 # minimum Number of trees per diameter class to consider
      for (i in 1:m) {
        if (stand.table[1,i,5] < minN){ Vi.sp1<-0
        } else { Vi.sp1<-Vmodule_individual(SPECIES=1, ZONE=ZONE, DBH=stand.table[1,i,3],
                                            HT=stand.table[1,i,4], blength=stand.table[1,i,4]) }
        if (stand.table[2,i,5] < minN){ Vi.sp2<-0
        } else { Vi.sp2<-Vmodule_individual(SPECIES=2, ZONE=ZONE, DBH=stand.table[2,i,3],
                                            HT=stand.table[2,i,4], blength=stand.table[2,i,4]) }
        if (stand.table[3,i,5] < minN){ Vi.sp3<-0
        } else { Vi.sp3<-Vmodule_individual(SPECIES=3, ZONE=ZONE, DBH=stand.table[3,i,3],
                                            HT=stand.table[3,i,4], blength=stand.table[3,i,4]) }
        if (stand.table[4,i,5] < minN){ Vi.sp4<-0
        } else { Vi.sp4<-Vmodule_individual(SPECIES=DOM.SP, ZONE=ZONE, DBH=stand.table[4,i,3],
                                            HT=stand.table[4,i,4], blength=stand.table[4,i,4])}
        DDist[1,i,7]<-round(Vi.sp1*stand.table[1,i,5],3)
        DDist[2,i,7]<-round(Vi.sp2*stand.table[2,i,5],3)
        DDist[3,i,7]<-round(Vi.sp3*stand.table[3,i,5],3)
        DDist[4,i,7]<-round(Vi.sp4*stand.table[4,i,5],3)
        DDist[5,i,7]<-DDist[1,i,7]+DDist[2,i,7]+DDist[3,i,7]+DDist[4,i,7]
      }

      # Assigning volume from generated stand-table
      sims$sp.table<-cbind(sims$sp.table,VTHA=c(1:5)*0)
      sims$sp.table[1,5]<-round(sum(DDist[1,,7]),3)
      sims$sp.table[2,5]<-round(sum(DDist[2,,7]),3)
      sims$sp.table[3,5]<-round(sum(DDist[3,,7]),3)
      sims$sp.table[4,5]<-round(sum(DDist[4,,7]),3)
      sims$sp.table[5,5]<-round(sum(DDist[5,,7]),3)

      sims$DDdist<-DDist
    }

  }

  #############################
  # TREE simulation (both w and w/o compatibility)
  if (type=='tree') {

    sims<-tree_simulator(core.tree=input)
    
    ####
    sims$tree.list$DBH0<-input$tree.list$DBH  # eliminate at the end before return!!
    ####
    
    sims$DDdist<-NA
    m <- nrow(sims$tree.list)

    # Estimating individual volume for each tree
    sims$tree.list$VOL <- mapply(Vmodule_individual, SPECIES=sims$tree.list$SPECIES,
                                 ZONE=sims$ZONE, DBH=sims$tree.list$DBH,
                                 HT=sims$tree.list$HT, blength=sims$tree.list$HT)


    #Definition of diameter groups
    class <- 5
    diam <- seq(from=5,to=90,by=class)   # Diameter classes
    breaks. <- seq(from = 7.5, to = 87.5, by = 5)  #improved Dclass calculation. Faster

    # Assigning trees to each diameter class (Dclass)
    sims$tree.list$Dclass <- breaks.[findInterval(x = sims$tree.list$DBH,
                                                  vec = breaks.,
                                                  all.inside = TRUE)]
    sims$tree.list$Nst <- sims$tree.list$FT  #is it needed? Duplicate?
    sims$tree.list$BAst <- sims$tree.list$FT*(pi/4)*(sims$tree.list$DBH^2/10000)
    sims$tree.list$VOLst <- sims$tree.list$FT*sims$tree.list$VOL

    HT.agg <-aggregate(HT~Dclass+SPECIES,FUN=mean,data=sims$tree.list)
    N.agg  <-aggregate(Nst~Dclass+SPECIES,FUN=sum,data=sims$tree.list)
    BA.agg <-aggregate(BAst~Dclass+SPECIES,FUN=sum,data=sims$tree.list)
    VT.agg <-aggregate(VOLst~Dclass+SPECIES,FUN=sum,data=sims$tree.list)
    data.agg<-data.frame(HT.agg,N.agg[3],BA.agg[3],VT.agg[3])


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
    DDist<-array(data=0, dim=c(5,(length(diam)-1),7),
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
    sims$sp.table<-cbind(sims$sp.table,VTHA=c(1:5)*0)
    sims$sp.table[1,5]<-round(sum(DDist[1,,7]),6)
    sims$sp.table[2,5]<-round(sum(DDist[2,,7]),6)
    sims$sp.table[3,5]<-round(sum(DDist[3,,7]),6)
    sims$sp.table[4,5]<-round(sum(DDist[4,,7]),6)
    sims$sp.table[5,5]<-round(sum(DDist[5,,7]),6)

    sims$tree.list<-sims$tree.list[,c(1:7)]
    sims$DDdist<-DDist

  }

  #############################
  # Compatibility simulation
  if (type=='comp') {

    input$type<-'stand'
    input$ddiam<-FALSE
    sim.stand<-core_module(input=input) # simulate stand

    input$type='tree'
    sim.tree<-core_module(input=input) # simulate tree

    sims<-comp_module(sim.tree=sim.tree, sim.stand=sim.stand)

  }

  return(output=sims)

}


# Note: V_model=1 or anything else is always the same
# This module should now separate VT for all products(additional columns)
# Note: this module could store the sp.table for each simulated year.
# Volume is calculated after each simulation???(maybe not... need to decide.)
# Needs to improve compatability
