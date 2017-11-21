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

comp_simulator <- function(core.tree = NULL){

  ### Initializations of variables
  HT<-core.tree$tree.list$HT   #list heights
  #Ss<-core.tree$tree.list$SS   #list sociological status NA
  FTv<-rep(1,length(core.tree$tree.list$ID))    #expansion factor for volume?
  input.data1<-covariates(ID=core.tree$tree.list$ID,
                          Fa=core.tree$tree.list$FT,    #list of expansion factors
                          sp=core.tree$tree.list$SPECIES,
                          DBH=core.tree$tree.list$DBH,      #list dbh
                          ZONE=core.tree$zone)   #estimating stand and individual variables

  #---===
  # SIMULATION ----
  #---===

  # Initial variables. This are updated every year.
  Fap<-core.tree$tree.list$FT  #list of expansion factors
  QDp<-input.data1$QD    #list of quadratic diameter per tree
  DBHp<-input.data1$DBH
  BALcp<-input.data1$BALc
  SDIp<-input.data1$SDI
  Ap<-FTv*core.tree$AD
  SSp<-input.data1$Ss
  DAp<-FTv*core.tree$AD
  PSCALp<-input.data1$PScal

  AD  <- core.tree$AD   #to maintain constant
  AF  <- core.tree$AF

  #FROM INITIAL AGE TO FINAL AGE
  for (y in (AD):(AF-1)) {
    #y <- AD
    core.tree$AD <- y
    core.tree$AF <- y + 1

    #Simualtion
    sim.tree <- tree_simulator(core.tree = core.tree)  #tree simulator
    sim.stand <- stand_simulator(core.stand = core.tree) #stand simulator

    #Values needed for compatibility. Updated every year
    #p1.SIM <- sim.tree$comp.list$prob.surv   #Survival probability
    NHA1.SIM <- sim.stand$input$sim.stand$NHA[2]   #From stand_simulation
    BA.SIM <- sim.stand$input$sim.stand$BA[2]    #simulated BA from stand_simulator
    DBH1.SIM <- sim.tree$input$tree.list$DBH    #simulated new diameter from tree_simulator
    DBH0 <- core.tree$tree.list$DBH     #original DBH
    FT.SIM <- sim.tree$input$tree.list$FT   #From the tree_simulation

    if (core.tree$comp == 'PY'){      #Proportional compatibility
      DBH1.SIM.COMP <- sqrt(((DBH1.SIM)^2)*(BA.SIM/(pi/40000))/(sum(FT.SIM*(DBH1.SIM)^2)))  #adjusting diameter
      FT.COMP <- FT.SIM*(NHA1.SIM/sum(FT.SIM))   #Adjusting FT
    } else if (core.tree$comp == 'PG'){      #Individual compatibility
      #Adjusted DBH1
      Num<-(BA.SIM/(pi/40000))
      Den<-sum(sim.tree$input$tree.list$FT*(DBH1.SIM)^2)
      DBH1.SIM.COMP <- sqrt(DBH0^2 + (DBH1.SIM^2-DBH0^2)*(Num-Den)/(Den-sum(FT.SIM*DBH0^2)))
      #m <- NHA1.SIM/sum(FT.SIM)/sum(FT.SIM/NHA1.SIM) #does not affect. The same as tree_simulator then
      #m <- log(NHA1.SIM)/sum(log(FT.SIM))
      FT.COMP <- FT.SIM
      #print(m)
    } else if (core.tree$comp == 'None'){      #No compatibility. Just tree.simulator
      #Unadjusted DBH1
      DBH1.SIM.COMP <- DBH1.SIM
      FT.COMP <- FT.SIM
      #print(m)
    }

    sim.tree$input$tree.list$DBH <- DBH1.SIM.COMP  #Update diameter with compatibility results
    sim.tree$input$tree.list$FT <- FT.COMP  #Update
    sim.tree$input$AD <- AD  #Updating year. Just important for final year.

    core.tree <- core_module(input = sim.tree$input)  #needs to be here because core.tree is used in the simulation
  }

  return(sim.tree$input)
}
