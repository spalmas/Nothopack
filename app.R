#---------------IMPORTING PACKAGES----------------
library(grid)     #For app
library(gridExtra)     #For app
library(shiny)     #For app
library(tidyverse) #for data management

#---------------IMPORTING SOURCE FILES----------------

file.sources = list.files(path = 'R/', pattern="*.R")
sapply(paste0('R/',file.sources), source, .GlobalEnv)

plot.example <- read.csv('data/Plot_example.csv', sep = ',')

#---------------APP DEFINITION----------------
shinyApp(
  ui = tagList(
    navbarPage(
      theme = "darkly",  # <--- To use a theme, uncomment this
      "Nothofagus Simulator",
      tabPanel("Simulator",
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
                 checkboxInput(inputId = 'ddiam', label = 'Diametric Distribution', value = TRUE),
                 actionButton(inputId = "run",label =  "Run!", class = "btn-primary")
               ),
               mainPanel(width = 7,
                 tabsetPanel(
                   tabPanel("Results",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Text try 1 "),
                            verbatimTextOutput("txtout"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Rauli"),
                   tabPanel("Roble"),
                   tabPanel("Coigue")
                 )
               )
      ),
      tabPanel("Advanced",
               sidebarPanel(verbatimTextOutput("txtout"),
                            h3("Advanced Parameters"),
                            h5("This tab allows for more control of simulation parameters.
                               For more information on each parameter see XXX")
                            ),
               mainPanel(radioButtons("NHAmodel", label = h2("Mortality model"),
                                      choices = list("Linear model" = 1, "Non-linear model" = 2
                                                     ), selected = 1),
                         textInput(inputId="theta", label="theta", value = 0.0005)
               )
      ),
      tabPanel("Contact",
               sidebarPanel(verbatimTextOutput("txtout"),
                            h3("Contact"),
                            h5("Salvador Gezan, Paulo Moreno, Sebastian Palmas"),
                            h3("Github repository"),
                            h5("github.com/spalmas/Nothopack")
                            )
               )
    )
  ),

  server = function(input, output) {

    #core.stand <-  core_module(input = input$treelist)
    core.stand <-  core_module(input = plot.example)

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
