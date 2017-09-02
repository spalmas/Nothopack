#' Simulates both individual and stand simulations and makes them compatible
#'
#' \code{comp_simulator} Simulates both individual and stand simulations and makes them compatible.
#'
#' @param core.stand a stand list corrected from core_module
#'
#' @author
#' S.Gezan, S.Palmas and P.Moreno
#'
#' @examples
#' plot<- read.csv(file= 'data/Plot_example.csv')
#' input<-input_module(type='tree',zone=2,AD=28,HD=15.5,area=500,AF=35,tree.list=plot, comp = 'PG')
#' core.tree<-core_module(input=input$input)
#' sim.tree<-comp_simulator(core.tree=core.tree$input)
#' core_module(input = sim.tree)
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
  for (y in AD:(AF-1)) {
    #y <- AD
    core.tree$AD <- y
    core.tree$AF <- y + 1

    #Simualtion
    sim.tree <- tree_simulator(core.tree = core.tree)  #tree simulator
    #core_module(input = sim.tree$input)
    sim.stand <- stand_simulator(core.stand = core.tree) #stand simulator

    #Values needed for compatibility. Updated every year
    p1.SIM <- sim.tree$comp.list$prob.surv   #Survival probability
    NHA1.SIM <- sim.stand$input$sim.stand$NHA[2]   #From stand_simulation
    AREA.HA <- sim.stand$input$area/10000    #plot area in hectares
    FT <- 1/AREA.HA
    BA.SIM <- sim.stand$input$sim.stand$BA[2]    #simulated BA from stand_simulator
    DBH1.SIM <- sim.tree$input$tree.list$DBH    #simulated new diameter from tree_simulator
    DBH0 <- core.tree$tree.list$DBH     #original DBH

    if (core.tree$comp == 'PYM'){   #Proportional yield mortality
      #Adjusted survival probability
      #p1.SIM.COMP <- p1.SIM*(AREA.HA*NHA1.SIM/sum(p1.SIM))

    } else if (core.tree$comp == 'PYD'){      #Proportional yield diameter
      #Adjusted DBH1
      DBH1.SIM.COMP <- sqrt((DBH1.SIM)^2*(AREA.HA*BA.SIM/(pi/40000))/(sum(p1.SIM*(DBH1.SIM)^2)))

    } else if (core.tree$comp == 'PG'){      #Proportional growth
      #Adjusted DBH1
      DBH1.SIM.COMP <- sqrt(DBH0^2 + ((AREA.HA*BA.SIM/(pi/40000) - sum(p1.SIM*DBH0^2))/(sum(p1.SIM)*(DBH1.SIM^2-DBH0^2)) )*(DBH1.SIM^2 - DBH0^2))

    } else if (core.tree$comp == 'DIS'){      #Dissagregation
      #p1.SIM.COMP <- function(p1, s, N1){ p1^m sum(p1) =s*N }

      #m <- log(NHA1.SIM/FT)/sum(log(p1.SIM))
      #FT*sum(p1.SIM)
      #sum(p1.SIM*FT)


    }

    sim.tree$input$tree.list$DBH <- DBH1.SIM.COMP  #Update diameter
    sim.tree$input$tree.list$FT <- FT  #change back expansion factor
    sim.tree$input$AD <- AD

    core.tree <- core_module(input = sim.tree$input)  #needs to be here because core.tree is used in the simulation
    #core_module(input = sim.tree$input)
    #print(core.tree$tree.list$DBH)   #To track results
  }

  return(sim.tree$input)
}
