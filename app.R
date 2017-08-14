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
                                    tableOutput(outputId = "tab_sim_results")# ,
                                    #h4("Plot"),
                                    #plotOutput(outputId = "plot_sim_results")
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
                         textInput(inputId="theta", label="theta parameter", value = 0.0003),
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

    #Changing to core and
    #core.stand <- core_module(input = input$treelist)
    tab_sim_results  <- eventReactive(eventExpr = input$run,{

      #Starting time of simulation
      start_time <- Sys.time()   #Useful to estimate time that the simulation takes

      #Path of tree file
      infile <- input$treelist

      #Reading file or usign example
      if(!is.null(infile)){
        #Use the treelist or stand data
        #stand_input<-input_module(type='tree',zone=input$ZONE,AD=input$AD,area=input$area,AF=input$AF,tree.list=plot)
      } else {
        #From plot_example. needs the details of the plot_example
        stand_input<-input_module(N=stand_example$N,BA=stand_example$BA,
                                  zone=2,AD=28,HD=15.5,AF=35)
        #stand_input <- input_module(type='tree',zone=1,AD=20,area=500,AF=40,tree.list=plot_example)
      }

      #changing to core
      core_stand <- core_module(input = stand_input)

      #Run simulator and store results
      tab_sim_results <- stand_simulator(core_stand)$results

      #End time of simulation
      processing_time <- start_time - Sys.time()   #Useful to estimate time that the simulation takes


      return(tab_sim_results)

    })

    ###---
    # *Output functions ----
    ###---
    #Function for plotting tab.sim.results
    output$tab_sim_results <- renderTable({ tab_sim_results() })

    #Function for plotting tab.sim.results
    output$plot_sim_results <- renderPlot({
      report_plots(SIM = tab_sim_results())
    })


    #Output for length of simulation
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })


  }
)
