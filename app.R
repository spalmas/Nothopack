###---
# IMPORTING PACKAGES ----------------------
###---
library(shiny)     #For app
library(tidyverse) #for data management

###---
# IMPORTING SOURCE FILESAPP DEFINITION ----
###---

file.sources = list.files(path = 'R/', pattern="*.R")
sapply(paste0('R/',file.sources), source, .GlobalEnv)

plot.example <- read.csv('data/Plot_example.csv', sep = ',')

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
                            sliderInput(inputId = "sy", label = "Years of Simulation:", min = 1, max = 25, value = 20),
                            selectInput(inputId = 'ZONE', label = 'Growth Zone',choices = c(1,2,3,4),selected = '1'),
                            div(style="display:inline-block",textInput(inputId="AD", label="Dominant Age", value = 0.0)),
                            div(style="display:inline-block",textInput(inputId="HD", label="Dominant Height", value = 0.5)),
                            div(style="display:inline-block",textInput(inputId="SI", label="Site Index", value = 12)),
                            div(style="display:inline-block",textInput(inputId="BA", label="BA", value = 0.0)),
                            div(style="display:inline-block",textInput(inputId="NHA", label="NHA", value = 0.5)),
                            div(style="display:inline-block",textInput(inputId="QD", label="QD", value = 12)),
                            actionButton(inputId = "run",label =  "Run!", class = "btn-primary")
               ),


               mainPanel(width = 7,
                         tabsetPanel(
                           ###---
                           # ** Main results tab ----
                           ###---
                           tabPanel(title = "Results",
                                    h4("Table"),
                                    tableOutput(outputId = "Results Table"),
                                    h4("Text try 1 "),
                                    h1("Header 1"),
                                    h2("Header 2"),
                                    h3("Header 3"),
                                    h4("Header 4"),
                                    h5("Header 5")
                           ),
                           ###---
                           # ** RAULI RESULTS TAB: ONLY OUTPUT ----
                           ###---
                           tabPanel("Rauli"),
                           ###---
                           # ** ROBLE RESULTS TAB: ONLY OUTPUT ----
                           ###---
                           tabPanel("Roble"),
                           ###---
                           #          COIGUE RESULTS TAB: ONLY OUTPUT
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
               mainPanel(radioButtons(inputId = "NHAmodel", label = h4("Mortality model"), choices = list("Linear model" = 1, "Non-linear model" = 2), selected = 1),
                         textInput(inputId="theta", label="theta", value = 0.0005),
                         checkboxInput(inputId = 'ddiam', label = h4('Construct diametric distribution'), value = TRUE)
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
      core.stand <-  core_module(input = plot.example)
    } else {
        core.stand <- core_module(input = input$treelist)
    }

    stand.sim.results  <- eventReactive(input$run,{
      stand_simulator(core.stand)
    })

    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })

    output$table <- renderTable({
      head(cars, 4)
    })

  }
)
