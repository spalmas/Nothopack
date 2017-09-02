require(shiny)
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
                            column(4, wellPanel(uiOutput("ui"))),
                            column(7,h4('Stand simulation results'),
                                   tableOutput(outputId = "tab_sim_results")
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
                             numericInput(inputId = 'theta', label = 'Theta', value = 0.0003),
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
    tab_sim_results  <- eventReactive(eventExpr = input$runstand,{
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

    # *tree simulation ----
    tab_sim_results2 <- eventReactive(eventExpr = input$runtree,{
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

    ###---
    #  Output definitions ----------------
    ###---
    output$ui <- renderUI({
      if (is.null(input$type))
        return()

      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$type,
             "Tree" = tagList(fileInput("treelist", "Tree list:"),
                              selectInput(inputId = 'zone', label = 'Zone',choices = c('Zone 1','Zone 2','Zone 3','Zone 4'), selected = '1', width = '30%'),
                              numericInput("area", "Plot area", value = 500, width = '40%'),
                              div(style="display:inline-block",numericInput("AD", "Dominant Age", value = 20, width = '30%')),
                              div(style="display:inline-block",numericInput("HD", "Dominant height", value = 20, width = '30%')),
                              div(style="display:inline-block",numericInput("SI", "Site index", value = 20, width = '30%')),
                              selectInput(inputId = 'comp', label = 'Compatibility', choices = c('None','PG','PYD'),selected = 'None', width = '50%'),
                              numericInput("AF", "Final Age", value = 30, width = '50%'),
                              actionButton(inputId = "runtree",label =  "Run!", class = "btn-primary")
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
                               actionButton(inputId = "runstand",label =  "Run!", class = "btn-primary")
             )
      )
    })

    #Table of stand_simulation results
    output$tab_sim_results <- renderTable({ tab_sim_results() })

    output$input_type_text <- renderText({
      input$input_type
    })


  }


)
