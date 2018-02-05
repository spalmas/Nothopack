#Importing packages and reading all required files
source('startup.R')

shinyApp(
  ###---
  #  USER INTERFACE ----------------
  ###---
  ui <- tagList(
    navbarPage(
      theme = "darkly",  # <--- To use a theme, uncomment this
      "Nothofagus Simulator",

      ###---
      #  Simulator tab ----------------
      ###---
      tabPanel(title = "Simulator",
               fluidPage( titlePanel("Simulator"),
                          fluidRow(column(width = 3,
                                          wellPanel(div(class = "option-group",
                                                        radioButtons("sim_type", "Simulation type", choices = c("Stand", "Tree"), inline = TRUE),
                                                        selectInput(inputId = 'zone', label = 'Zone',choices = c(1,2,3,4), selected = 1, width = '25%'),
                                                        numericInput("AD", "Dominant Age", value = 20, width = '25%'),
                                                        numericInput("AF", "Final Age", value = 30, width = '25%'),
                                                        numericInput("HD", "Dominant height", value = 20, width = '25%'),
                                                        conditionalPanel("input.sim_type === 'Stand'",
                                                                         numericInput("BA.NA", "BA NA", value = 1.09, min = 0),
                                                                         div(style="display:inline-block",numericInput("NHA.NA", "NHA BA ", value = 60, min = 0, width = '40%')),
                                                                         div(style="display:inline-block",numericInput("BA.NO", "BA NO", value = 38.82, min = 0, width = '40%')),
                                                                         div(style="display:inline-block",numericInput("NHA.NO", "NHA NO", value = 780, min = 0, width = '40%')),
                                                                         div(style="display:inline-block",numericInput("BA.ND", "BA ND", value = 0.0, min = 0, width = '40%')),
                                                                         div(style="display:inline-block",numericInput("NHA.ND", "NHA ND", value = 0, min = 0, width = '40%')),
                                                                         div(style="display:inline-block",numericInput("BAC", "BAC", value = 0.31, min = 0, width = '40%')),
                                                                         div(style="display:inline-block",numericInput("NHAC", "NHAC", value = 80, min = 0, width = '40%'))),
                                                        conditionalPanel("input.sim_type === 'Tree'",
                                                                         fileInput("tree.list.", "Tree list", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                                         numericInput("area", "Plot area (m2)", value = 500, width = '40%'),
                                                                         selectInput(inputId = 'comp', label = 'Compatibility', choices = c('None','PG','PY'), selected = 'None', width = '50%')),
                                                        actionButton(inputId = "run", label = "Run!", class = "btn-primary")

                                          ))
                          ),
                          # This outputs the dynamic UI component
                          column(6,h4('Simulation results'), tableOutput(outputId = "tab_sim_results")
                          )
                          )
               )
      ),

      ###---
      #  Advanced Parameters tab ----------------
      ###---

      tabPanel(title = "Advanced",
               fluidPage(titlePanel("Advanced"),
                         fluidRow(
                           column(3, wellPanel(
                             selectInput(inputId = "NHA_model", label = "Mortality model",choices = c('Linear','Non-linear')),
                             numericInput(inputId = 'theta', label = 'Theta', value = 0.003),   #Limit range of value?
                             selectInput(inputId = "V_model", label = "Volume model",choices = c(1,2))
                           ))
                         )
               )
      ),

      ###---
      #  Contact tab ----------------
      ###---
      tabPanel(title = "Contact",
               fluidPage(titlePanel("Contact"),
                         sidebarPanel(verbatimTextOutput("txtout"),
                                      h3("Authors"),
                                      h5("Salvador Gezan, Paulo Moreno, Sebastian Palmas"),
                                      h3("Github repository"),
                                      h5( a("github.com/spalmas/Nothopack", href="https://github.com/spalmas/Nothopack"))))
      )
    )
  ),

  ###---
  #  SERVER ----------------
  ###---

  server <- function(input, output) {
    ###---
    # Reactive functions ----------------
    ###---

    # *Stand simulation ----
    tab_sim_results  <- eventReactive(eventExpr = input$run,{
      start_time <- Sys.time()   #Useful to estimate time that the simulation takes
      #From plot_example. needs the details of the plot_example
      if(input$sim_type == 'Stand'){
        stand_input<-input_module(type='stand',
                                  zone=input$zone, HD=input$HD, AD=input$AD, AF=input$AF,
                                  N=c(input$NHA.NA, input$NHA.NO, input$NHA.ND, input$NHAC),
                                  BA=c(input$BA.NA, input$BA.NO, input$BA.ND, input$BAC),
                                  V_model=input$V_model)
        core.stand<-core_module(input = stand_input)
        tab_sim_results <- stand_simulator(core.stand = core.stand)$input$sim.stand
      } else if(input$sim_type == 'Tree'){
        if(is.null(input$tree.list.)){
          tree.list. <- plot_example  #Use example if there is no input file
        } else {
          tree.list. <- read_csv(input$tree.list.$datapath)  #read csv data
        }
        #input.list<-input_module(type='tree',zone=1,AD=20,HD=15,area=500,AF=30,tree.list=plot_example, comp = 'None')
        tree_input<-input_module(type='tree',
                                 zone=input$zone, HD=input$HD, AD=input$AD, AF=input$AF,
                                 area=input$area, tree.list=tree.list., ddiam = TRUE,
                                 comp = input$comp)
        core.tree<-core_module(input=tree_input$input)      #Initial core_
        sim.tree<-comp_simulator(core.tree=core.tree$input)   #Running simulation
        tab_sim_results<-core_module(input=sim.tree)$sp.tabl
      }

      processing_time <- Sys.time() - start_time   #End time of simulation
      #Return table of results, or maybe list?
      return(tab_sim_results)
    })

    ###---
    #  Output definitions ----------------
    ###---
    #Table of stand_simulation results
    output$tab_sim_results <- renderTable({ tab_sim_results() })

    #output$input_type_text <- renderText({
    #  input$input_type
    #})


  }


)
