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
#' 
#' @return A series of stand-level parameters (same as input), plus a tree-list (based on current tree-list if
#' provided otherwise generated from a diameter distribution),  together with parameters provided from the user
#' in input module.
#'
#' @examples
#' # Example 1: Input from stand-level data
#' BA<-c(36.5,2.8,1.6,2.4)
#' N<-c(464,23,16,48)
#' plot<-inputmodule(type='stand',zone=1,AD=28,HD=23.5,N=N,BA=BA)
#' core.stand<-core_module(zone=plot$zone, DOM.SP=plot$DOM.SP, AD=plot$AD, 
#'                          HD=plot$HD, SI=plot$SI, PBAN=plot$PBAN, PNHAN=plot$PNHAN,
#'                          type='stand',sp.table=plot$sp.table, V_model=2)
#' attributes(core.stand)
#' core.stand$sp.table
#' core.stand$stand.table[5,,]
#' 
#' # Example 2: Input from tree-level data (or file)
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' plot<-inputmodule(type='tree',zone=2,AD=28,HD=23.5,area=500,tree.list=plot)
#' head(plot$tree.list) 
#' plot$sp.table
#' core.tree<-core_module(zone=plot$zone, DOM.SP=plot$DOM.SP, AD=plot$AD, 
#'                          HD=plot$HD, SI=plot$SI, PBAN=plot$PBAN, PNHAN=plot$PNHAN,
#'                          type='tree', area=500,
#'                          sp.table=plot$sp.table, tree.list=plot$tree.list,V_model=1)
#' 
#' 
#' core.tree$sp.table
#' core.output$tree.list
#'  

core_module <- function(zone=NA, DOM.SP=NA, AD=NA, HD=NA, SI=NA, sp.table=NA, 
                        SDI=NA, PBAN=NA, PNHAN=NA, AF=NA, tree.list=NA, area=0, 
                        type='stand', ddiam=FALSE, comp=FALSE, 
                        N_model=1, V_model=1, IADBH_model=1){

  stand.table <- NA

  # Forming Stand-table
    
  if (type=='stand'){    # tree.list not provided then generate one - always
    
    # Calculating Stand-level volume (total)
    sp.table<-cbind.data.frame(sp.table,VTHA=c(1:5)*0)
    if (V_model==1){ VTHA <- Vmodule(BA=sp.table$BA[5], HD=HD, PNHAN=PNHAN) 
    } else { VTHA <- Vmodule(BA=sp.table$BA[5], HD=HD) }
    sp.table[5,5]<-round(VTHA,3)
  
    # Assigning volume proportional to PBA
    PBA<-sp.table[1:4,3]/sp.table[5,3]
    sp.table[1:4,5]<-round(VTHA*PBA,3) 
 
    # Generate stand-table from diameter distibution (does not contain volumes)
    stand.table<-diam_dist(sp.table=sp.table, HD=HD, DOM.SP=DOM.SP, zone=zone)
    m <- length(stand.table[1,,1])

    # Calculating class-level volume (total) by specie and class
    r_names<-c('DBH_ll','DBH_ul','D_class','H_class','N','BA','VT')
    stand.table.updated<-array(data=NA, dim=c(5,m,7), dimnames=list(c(1:5),c(1:m),r_names))
    stand.table.updated[,,-7]<-stand.table

    for (i in 1:m) {
      if (stand.table[1,i,5] < 0.25){ Vi.sp1<-0 
      } else { Vi.sp1<-Vmodule_individual(SPECIES=1, zone=zone, DBH=stand.table[1,i,3], 
                                 HT=stand.table[1,i,4], blength=stand.table[1,i,4]) }
      if (stand.table[2,i,5] < 0.25){ Vi.sp2<-0 
      } else { Vi.sp2<-Vmodule_individual(SPECIES=2, zone=zone, DBH=stand.table[2,i,3], 
                                 HT=stand.table[2,i,4], blength=stand.table[2,i,4]) }
      if (stand.table[3,i,5] < 0.25){ Vi.sp3<-0 
      } else { Vi.sp3<-Vmodule_individual(SPECIES=3, zone=zone, DBH=stand.table[3,i,3], 
                                 HT=stand.table[3,i,4], blength=stand.table[3,i,4]) }
      if (stand.table[4,i,5] < 0.25){ Vi.sp4<-0 
      } else { Vi.sp4<-Vmodule_individual(SPECIES=DOM.SP, zone=zone, DBH=stand.table[4,i,3], 
                                 HT=stand.table[4,i,4], blength=stand.table[4,i,4])}
      stand.table.updated[1,i,7]<-round(Vi.sp1*stand.table[1,i,5],3)
      stand.table.updated[2,i,7]<-round(Vi.sp2*stand.table[2,i,5],3)
      stand.table.updated[3,i,7]<-round(Vi.sp3*stand.table[3,i,5],3)
      stand.table.updated[4,i,7]<-round(Vi.sp4*stand.table[4,i,5],3)
      stand.table.updated[5,i,7]<-stand.table.updated[1,i,7]+stand.table.updated[2,i,7]+
                                  stand.table.updated[3,i,7]+stand.table.updated[3,i,7]
    }
    
    if (comp==TRUE){         # then we do compatibility if requested
      comp      
    }
    
  }
  
  if (type=='tree'){   # tree.list is provided
    
    stand.table.updated <- NA
    n <- length(tree.list$ID)
    vind <- matrix(NA,nrow=n)
    Dclass <- matrix(NA,nrow=n)
    
    if (is.na(area) ){ stop('Plot area must be provided') }
    CF <- 10000 / area  # Correction factor
    baind <- as.numeric(pi*(tree.list$DBH/2)^2/10000)  # Units: m2
    for (i in 1:n) {
      if (tree.list$SPECIE[i]==4) { SP<-DOM.SP 
      } else { SP<-tree.list$SPECIE[i] }
      vind[i] <- Vmodule_individual(SPECIES=SP, zone=zone, DBH=tree.list$DBH[i], 
                                        HT=tree.list$HT[i], blength=tree.list$HT[i]) 
    }
    Dclass <- ceiling(tree.list$DBH/5)*5-2.5
    F <- CF*tree.list$FT
    BA <- F*baind
    VT <- F*vind
    tree.list<-data.frame(tree.list,F,BA,VT,Dclass)
    res<-aggregate(BA~Dclass+SPECIE,FUN=sum,data=tree.list)
    print(res)
    print(tree.list)
    print('It needs to aggregate and form stand.table')
  }  
  
  return(list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
              area=area, type=type, ddiam=ddiam, comp=comp, N_model=N_model, V_model=V_model, 
              IADBH_model=IADBH_model,
              sp.table=sp.table, stand.table=stand.table.updated, tree.list=tree.list))
}

# Note: V_model=1 or anything else is always the same

