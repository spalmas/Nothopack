#' General Compatibility/Tree Simulator.
#'
#' \code{comp_module} General simulator for Tree simulations. It can run a tree simulator if core.tree$comp == 'None' or go
#' to the PY or PG pathways. It could be improved by avoiding running stand_simulator if core.tree$comp == 'None'.
#'
#' @param sim.tree a simulated tree list from tree_simulator
#' @param sim.stand a simulated stand from stand_simulator
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' # Example 1: Proportional Yield
#' tree.list<-read.csv(file= 'data/Plot_example.csv')
#' plot.tree<-input_module(ZONE=2, AD=28, HD=23.5, AF=32, type='tree', area=500, tree.list=tree.list, Hest_method=1, ddiam=FALSE)
#' sim.tree<-core_module(input=plot.tree)
#'
#' BA<-c(1.086,38.915,0.0,0.313)
#' N<-c(60,780,0,80)
#' plot.stand<-input_module(ZONE=2, AD=28, HD=23.5, AF=32, N=N, BA=BA, type='stand', ddiam=FALSE)
#' sim.stand<-core_module(input=plot.stand)
#'
#' sim.tree$sp.table
#' sim.stand$sp.table
#'
#' head(sim.tree$tree.list)
#' head(sim.stand$tree.list)
#'
#' sim.tree$comptype='PY'
#' sim.comp<-comp_module(sim.tree=sim.tree, sim.stand=sim.stand)
#'
#' # Example 2: Proportional Growth
#'
#' # Example 3: No compatibility

comp_module <- function(sim.tree=NA, sim.stand=NA){

    #p1.SIM <- sim.tree$comp.list$prob.surv        # Survival probability  ????????????
    NHA1.SIM <- sim.stand$sp.table$N[5]   # From stand_simulation
    BA.SIM <- sim.stand$sp.table$BA[5]    # Simulated BA from stand_simulator
    DBH1.SIM <- sim.tree$tree.list$DBH    # Simulated new diameter from tree_simulator
    #DBH0 <- core.tree$tree.list$DBH      # Original DBH from tree_simulatior
    FT.SIM <- sim.tree$tree.list$FT       # Expansion factor from tree_simulation

    # Proportional compatibility - PY
    if (sim.tree$comptype == 'PY'){
      DBH1.SIM.COMP <- sqrt(((DBH1.SIM)^2)*(BA.SIM/(pi/40000))/(sum(FT.SIM*(DBH1.SIM)^2)))  # Adjusting diameter
      FT.COMP <- FT.SIM*(NHA1.SIM/sum(FT.SIM))   # Adjusting FT (mortality)
    }
    # Individual compatibility
    else if (sim.tree$comptype == 'PG'){
      #Adjusted DBH1
      Num <- BA.SIM/(pi/40000)
      Den <- sum(FT.SIM*(DBH1.SIM)^2)
      DBH1.SIM.COMP <- sqrt(DBH0^2 + (DBH1.SIM^2-DBH0^2)*(Num-Den)/(Den-sum(FT.SIM*DBH0^2)))
      #m <- NHA1.SIM/sum(FT.SIM)/sum(FT.SIM/NHA1.SIM) #does not affect. The same as tree_simulator then
      #m <- log(NHA1.SIM)/sum(log(FT.SIM))
      FT.COMP <- FT.SIM
      #print(m)
    } else {      #No compatibility. Just tree.simulator
      DBH1.SIM.COMP <- DBH1.SIM  # Unadjusted DBH1
      FT.COMP <- FT.SIM
    }

    sim.tree$tree.list$DBH <- DBH1.SIM.COMP  # Updated DBH with compatibility
    sim.tree$tree.list$FT <- FT.COMP         # Updated FT (mortality with compatibility)
    #print(head(sim.tree$tree.list))

    #for sp.table
    sim.comp <- input_module(ZONE=sim.tree$ZONE,
                             AD=sim.tree$AD, HD=sim.tree$HD, SI=sim.tree$SI,
                             AF=sim.tree$AF, ddiam=sim.tree$ddiam, comptype=sim.tree$comptype, thinning=FALSE,
                             type='tree',
                             tree.list=sim.tree$tree.list, area=sim.tree$area,
                             NHA_model=sim.tree$NHA_model, V_model=sim.tree$V_model, IADBH_model=sim.tree$IADBH_model,
                             Hest_method=1) #Should it be other?Hest_method

    sim.comp <- core_module(input=sim.comp)                   # Needed to produce VOL

    #sim.tree$comp <- FALSE
    #sim.tree$AD <- sim.tree$AD
    #sim.comp <- core_module(input=sim.tree)  # Needed to produce VOL and updated sp.table

    return(sim.comp=sim.comp)
}
