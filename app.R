###---
# IMPORTING PACKAGES, FUNCTIONS AND DATA ----
###---

source('startup.R')

###---
# APP DEFINITION --------------------------
###---

shinyApp(

  ###---
  #  *USER INTERFACE ----------------
  ###---

  ui <- tagList(
    navbarPage(
      theme = "darkly",  # <--- To use a theme, uncomment this
      "Nothofagus Simulator",

      tabPanel("Simulator",
               ###---
               # ** Main sidebar panel ----
               ###---
               sidebarPanel(width = 4,
                            h3("Input parameters"),
                            fileInput("treelist", "Tree list:"),
                            textInput(inputId = "AF", label = "Final Age of Simulation:", value = 70),
                            selectInput(inputId = 'ZONE', label = 'Growth Zone',choices = c(1,2,3,4),selected = '1'),
                            div(style="display:inline-block",textInput(inputId="AD", label="Dominant Age", value = 0.0)),
                            div(style="display:inline-block",textInput(inputId="HD", label="Dominant Height", value = 0.5)),
                            div(style="display:inline-block",textInput(inputId="SI", label="Site Index", value = 12)),
                            div(style="display:inline-block",textInput(inputId="BA", label="BA", value = 0.0)),
                            div(style="display:inline-block",textInput(inputId="NHA", label="NHA", value = 0.5)),
                            div(style="display:inline-block",textInput(inputId="QD", label="QD", value = 12)),
                            div(style="display:inline-block",textInput(inputId="area", label="Area (m^2)", value = 1000)),
                            actionButton(inputId = "run",label =  "Run!", class = "btn-primary")
               ),

               ###---
               # ** Results panel ----
               ###---
               mainPanel(width = 7,
                         tabsetPanel(
                           ###---
                           # *** Main results tab ----
                           ###---
                           tabPanel(title = "Results",
                                    h4("Table"),
                                    tableOutput("tab.sim.result"),
                                    h4("Results"),
                                    plotOutput("sim.results"),
                                    h5("Alfuna funcion")
                           ),
                           ###---
                           # *** Rauli tab ----
                           ###---
                           tabPanel("Rauli"),
                           ###---
                           # *** Roble tab ----
                           ###---
                           tabPanel("Roble"),
                           ###---
                           # *** Coigue tab ----
                           ###---
                           tabPanel("Coigue")
                         )
               )
      ),

      ###---
      #** ADVANVCED RESULTS TAB: ONLY INPUT----
      ###---
      tabPanel("Advanced",
               sidebarPanel(verbatimTextOutput("txtout"),
                            h3("Advanced Parameters"),
                            h5("This tab allows for more control of simulation parameters.
                               For more information on each parameter see Manual")
               ),
               mainPanel(radioButtons(inputId = "NHA_model", label = "Mortality model", choices = list("Linear model" = 1, "Non-linear model" = 2), selected = 1),
                         textInput(inputId="theta", label="theta", value = 0.0005),
                         radioButtons(inputId = "V_model", label = "Volume model", choices = list("With PNHAN" = 1, "Without PNHAN" = 2), selected = 1),
                         checkboxInput(inputId = 'ddiam', label = 'Construct diametric distribution', value = TRUE),
                         checkboxInput(inputId = 'comp', label = 'Make compatibility', value = TRUE)
               )
      ),

      ###---
      #          CONTACT TAB
      ###---
      tabPanel("Contact",
               sidebarPanel(verbatimTextOutput("txtout"),
                            h3("Contact"),
                            h5("Salvador Gezan, Paulo Moreno, Sebastian Palmas"),
                            h3("Github repository"),
                            h5( a("github.com/spalmas/Nothopack", href="https://github.com/spalmas/Nothopack"))
               )
      )
    )
  ),

  ###---
  #          SERVER FUNCTIONS
  ###---

  server <- function(input, output) {

    #If there is a tree list use that, if not use the plot.example data
    if(is.na(input$treelist)){
      stand_table<-inputmodule(type='tree',zone=1,AD=25,area = 1000,AF=40,tree.list=plot_roble)
      stand_table<-inputmodule(type='tree',zone=input$ZONE,AD=input$AD,area=input$area,AF=input$AF,tree.list=plot)
    } else {
      core.stand <- core_module(input = input$treelist)
    }

    #Run simulator and store results
    tab.sim.results  <- eventReactive(eventExpr = input$run,{
      stand_simulator(core.stand)
    })

    #Function for plotting tab.sim.results
    output$plot.sim.results <- renderPlot({
      report_plots(SIM = tab.sim.results)
    })

    #Output for length of simulation
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })

  }
)
