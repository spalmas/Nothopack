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
                          fluidRow(
                            column(1, wellPanel(selectInput(inputId = "type", label = "Simulation Type", choices = c("Tree", "Stand")))),
                            # This outputs the dynamic UI component
                            column(3, wellPanel(uiOutput("ui"))),
                            column(7,h4('Stand simulation results'),
                                   tableOutput(outputId = "tab_sim_stand_results"),
                                   h4('Tree simulation results'),
                                   tableOutput(outputId = "tab_sim_tree_results")
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
                             numericInput(inputId = 'theta', label = 'Theta', value = 0.003),
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
    tab_sim_stand_results  <- eventReactive(eventExpr = input$runstand,{
      start_time <- Sys.time()   #Useful to estimate time that the simulation takes
      #From plot_example. needs the details of the plot_example
      stand_input<-input_module(type='stand',
                                zone=input$zone,AD=input$AD,HD=input$HD,
                                N=c(input$NHA.NA, input$NHA.NO, input$NHA.ND, input$NHAC),
                                BA=c(input$BA.NA, input$BA.NO, input$BA.ND, input$BAC),
                                V_model=input$V_model, AF=input$AF)
      core.stand<-core_module(input = stand_input)
      tab_sim_results <- stand_simulator(core.stand = core.stand)$input$sim.stand
      processing_time <- Sys.time() - start_time   #End time of simulation
      #Return table of results, or maybe list?
      return(tab_sim_results)
    })

    # *Tree and Compatibility simulation ----
    tab_sim_tree_results <- eventReactive(eventExpr = input$runtree,{
      start_time <- Sys.time()   #Useful to estimate time that the simulation takes
      if(is.null(input$tree.list.)){
        tree.list. <- plot_example  #Use example if there is no input file
      } else {
        tree.list. <- read_csv(input$tree.list.$datapath)  #read csv data
      }
      #input.list<-input_module(type='tree',zone=1,AD=20,HD=15,area=500,AF=30,tree.list=plot_example, comp = 'None')
      input.list<-input_module(type='tree', zone=input$zone, area=input$area, HD=input$HD, AD=input$AD, AF=input$AF, tree.list=tree.list., comp = input$comp, ddiam = TRUE)
      core.tree<-core_module(input=input.list$input)      #Initial core_
      sim.tree<-comp_simulator(core.tree=core.tree$input)   #Running simulation
      tab_sim_tree_results<-core_module(input=sim.tree)$sp.table    #Obtaining results from simulation
      processing_time <- Sys.time() - start_time   #End time of simulation
      return(tab_sim_tree_results)
    })

    ###---
    #  Output definitions ----------------
    ###---

    #  *Input UI ----------------
    output$ui <- renderUI({
      if (is.null(input$type)) return()

      # Depending on input$input_type, we'll generate a different UI component and send it to the client.
      switch(input$type,
             "Tree" = tagList(fileInput("tree.list.", "Tree list", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                              selectInput(inputId = 'zone', label = 'Zone',choices = c(1,2,3,4), selected = 1, width = '30%'),
                              numericInput("area", "Plot area (m2)", value = 500, width = '40%'),
                              div(style="display:inline-block",numericInput("AD", "Dominant Age", value = 20, width = '30%')),
                              div(style="display:inline-block",numericInput("HD", "Dominant height", value = 20, width = '30%')),
                              selectInput(inputId = 'comp', label = 'Compatibility', choices = c('None','PG','PY'), selected = 'None', width = '50%'),
                              numericInput("AF", "Final Age", value = 30, width = '50%'),
                              actionButton(inputId = "runtree", label = "Run!", class = "btn-primary")
             ),
             "Stand" = tagList(selectInput(inputId = 'zone', label = 'Zone',choices = c(1,2,3,4), selected = 1, width = '30%'),
                               div(style="display:inline-block",numericInput("AD", "Dominant Age", value = 20, width = '30%')),
                               div(style="display:inline-block",numericInput("HD", "Dominant height", value = 20, width = '30%')),
                               div(style="display:inline-block",numericInput("BA.NA", "BA NA", value = 1.09, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("NHA.NA", "NHA BA ", value = 60, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("BA.NO", "BA NO", value = 38.82, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("NHA.NO", "NHA NO", value = 780, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("BA.ND", "BA ND", value = 0.0, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("NHA.ND", "NHA ND", value = 0, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("BAC", "BAC", value = 0.31, min = 0, width = '40%')),
                               div(style="display:inline-block",numericInput("NHAC", "NHAC", value = 80, min = 0, width = '40%')),
                               numericInput("AF", "Final Age", value = 30, width = '50%'),
                               actionButton(inputId = "runstand", label = "Run!", class = "btn-primary")
             )
      )
    })

    #Table of stand_simulation results
    output$tab_sim_stand_results <- renderTable({ tab_sim_stand_results() })

    #Table of tree_simulation results
    output$tab_sim_tree_results <- renderTable({ tab_sim_tree_results() })


    #output$input_type_text <- renderText({
    #  input$input_type
    #})


  }


)
