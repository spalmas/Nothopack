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
#' plot<-inputmodule(level='stand',zone=2,AD=25,HD=23.4,N=N,BA=BA)
#' core.output<-core_module(zone=plot$zone, DOM.SP=plot$DOM.SP, AD=plot$AD, HD=plot$HD, SI=plot$SI,
#'             sp.table=plot$sd)
#' 
#' # Example 2: Input from tree-level data (or file)
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' head(plot)
#' plot<-inputmodule(type='tree',zone=2,AD=28,HD=23.5,area=500,tree.list=plot)
#' attributes(plot) 
#' head(plot$tree.list) 
#' plot$sp.table
#' 
#' core.output<-core_module(zone=plot$zone, DOM.SP=plot$DOM.SP, AD=plot$AD, HD=plot$HD, SI=plot$SI,
#'             sp.table=plot$sd, tree.list=plot$tree.list)
#' core.output$sd
#' core.output$tree.list
#'  

core_module <- function(zone=NA, DOM.SP=NA, AD=NA, HD=NA, SI=NA, sp.table=NA, 
                        SDI=NA, PBAN=NA, PNHAN=NA, AF=NA, tree.list=NA, area=0, 
                        type='stand', ddiam=FALSE, comp=FALSE, 
                        N_model=1, V_model=1, IADBH_model=1){

  stand.table <- NA

  # Forming Stand-table
    
  if (is.na(tree.list)){    # tree.list not provided then generate one - always
    
    # Generate stand-table from diameter distibution (does not contain volumes)
    stand.table<-diam_dist(sp.table=sp.table, HD=HD, DOM.SP=DOM.SP, zone=zone)
    #print(stand.table[5,,])
    
    # Calculating Stand-level volume (total)
    VHA <- Vmodule(BA=sp.table$BA[5], HD=HD, PNHAN=PNHAN)  # It calls model that requires PNHAN

    # Calculating class-level volume (total) by specie and class
    r_names<-c('DBH_ll','DBH_ul','D_class','H_class','N','BA','VOL')
    stand.table.updated<-array(data=NA, dim=c(5,85,7), dimnames=list(c(1:5),c(1:85),r_names))
    stand.table.updated[,,-7]<-stand.table
    #stand.table.updated[5,,]
    
    #for (i in 1:85) {
    #  Vi.sp1<-Vmodule_individual(SPECIES=1, zone=zone, DBH=stand.table[1,i,3], 
    #                             HT=stand.table[1,i,4], blength=stand.table[1,i,4])
    #  Vi.sp2<-Vmodule_individual(SPECIES=2, zone=zone, DBH=stand.table[2,i,3], 
    #                             HT=stand.table[2,i,4], blength=stand.table[2,i,4])
    #  Vi.sp3<-Vmodule_individual(SPECIES=3, zone=zone, DBH=stand.table[3,i,3], 
    #                             HT=stand.table[3,i,4], blength=stand.table[3,i,4])
    #  Vi.sp4<-Vmodule_individual(SPECIES=DOM.SP, zone=zone, DBH=stand.table[4,i,3], 
    #                             HT=stand.table[4,i,4], blength=stand.table[4,i,4])
    #  stand.table.updated[1,i,7]<-Vi.sp1
    #  stand.table.updated[2,i,7]<-Vi.sp2
    #  stand.table.updated[3,i,7]<-Vi.sp3
    #  stand.table.updated[4,i,7]<-Vi.sp4
    #}
    #print(stand.table.updated[1,,])
    
    if (comp==TRUE){         # then we do compatibility if requested
      comp      
    }
    
  }
  
  if (!is.na(tree.list)){   # tree.list is provided
    (x=-99)
  }  
  
  return(list(zone=zone, DOM.SP=DOM.SP, AD=AD, HD=HD, SI=SI, SDI=SDI, PBAN=PBAN, PNHAN=PNHAN, AF=AF,
              area=area, type=type, ddiam=ddiam, comp=comp, N_model, V_model, IADBH_model,
              sp.table=sp.table, stand.table=stand.table, tree.list=tree.list))
}


