#' General Compatibility/Tree Simulator.
#'
#' \code{comp_simulator} General simulator for Tree simulations. It can run a tree simulator if core.tree$comp == 'None' or go
#' to the PY or PG pathways. It could be improved by avoiding running stand_simulator if core.tree$comp == 'None'.
#'
#' @param core.stand a stand list corrected from core_module
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' # Test-Example 1: Proportional Yield
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
#' sim.comp$sp.table
#' 
#' head(sim.tree$tree.list)
#' head(sim.stand$tree.list)
#' head(sim.comp$tree.list)
#' 
#' sim.tree$comptype='PY'
#' sim.comp<-comp_module(sim.tree=sim.tree, sim.stand=sim.stand) 
#' 
#' 
#' 
#' # Example 1: Proportional Yield
#' tree.list<- read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=33,tree.list=tree.list, comp = 'PY')
#' core.tree<-core_module(input=input$input)
#' sim.tree<-comp_simulator(core.tree=core.tree$input)
#' core_module(input = sim.tree)$sp.table
#'
#' # Example 2: Proportional Growth
#' tree.list<- read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=33,tree.list=tree.list, comp = 'PG')
#' core.tree<-core_module(input=input$input)
#' sim.tree<-comp_simulator(core.tree=core.tree$input)
#' core_module(input = sim.tree)$sp.table
#'
#' # Example 3: No compatibility
#' tree.list<- read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=33,tree.list=tree.list, comp = 'None')
#' core.tree<-core_module(input=input$input)
#' sim.tree<-comp_simulator(core.tree=core.tree$input)
#' core_module(input = sim.tree)$sp.table

comp_module <- function(sim.tree=NA, sim.stand=NA){

    #p1.SIM <- sim.tree$comp.list$prob.surv        # Survival probability  ????????????
    NHA1.SIM <- sim.stand$sp.table$N[5]   # From stand_simulation
    BA.SIM <- sim.stand$sp.table$BA[5]    # Simulated BA from stand_simulator
    DBH1.SIM <- sim.tree$tree.list$DBH    # Simulated new diameter from tree_simulator
    #DBH0 <- core.tree$tree.list$DBH                # Original DBH from tree_simulatior
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
    print(head(sim.tree$tree.list))
    
    sim.tree$comp <- FALSE
    sim.tree$AD <- sim.tree$AD
    sim.comp <- core_module(input=sim.tree)  # Needed to produce VOL and updated sp.table

    return(sim.comp=sim.comp)
}
