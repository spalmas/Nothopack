shinyApp(
  ui = tagList(
    navbarPage(
      theme = "darkly",  # <--- To use a theme, uncomment this
      "Nothofagus Simulator",
      tabPanel("Simulator",
               sidebarPanel(width = 4,
                 h3("Input parameters"),
                 fileInput("treelist", "Tree list:"),
                 sliderInput(inputId = "AF", label = "Years of Simulation:", min = 1, max = 25, value = 20),
                 selectInput(inputId = 'ZONE', label = 'Growth Zone',choices = c(1,2,3,4),selected = '1'),
                 div(style="display:inline-block",textInput(inputId="AD", label="Dominant Age", value = 0.0)),
                 div(style="display:inline-block",textInput(inputId="HD", label="Dominant Height", value = 0.5)),
                 div(style="display:inline-block",textInput(inputId="SI", label="Site Index", value = 12)),
                 div(style="display:inline-block",textInput(inputId="BA", label="BA", value = 0.0)),
                 div(style="display:inline-block",textInput(inputId="NHA", label="NHA", value = 0.5)),
                 div(style="display:inline-block",textInput(inputId="QD", label="QD", value = 12)),
                 checkboxInput(inputId = 'ddiam', label = 'Diametric Distribution', value = TRUE),
                 actionButton(inputId = "action2",label =  "Action button", class = "btn-primary")
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
                                      choices = list("Choice 1" = 1, "Choice 2" = 2
                                                     ),selected = 1))
      ),
      tabPanel("Contact")
    )
  ),

  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
  }
)
