shinyApp(
  ui = tagList(
    navbarPage(
      theme = "darkly",  # <--- To use a theme, uncomment this
      "Nothofagus Simulator",
      tabPanel("Simulator",
               sidebarPanel(
                 h3("Input parameters"),
                 fileInput("file", "File input:"),
                 textInput("txt", "Text input:", "general"),
                 sliderInput("slider", "Slider input:", 1, 100, 30),
                 actionButton(inputId = "action2",label =  "Action button", class = "btn-primary")
               ),
               mainPanel(
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
      tabPanel("Advanced"),
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
