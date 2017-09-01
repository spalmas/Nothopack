require(shiny)

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
                            column(4, wellPanel(uiOutput("ui")))
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
             "Stand" = tagList(selectInput(inputId = 'zone', label = 'Zone',choices = c('Zone 1','Zone 2','Zone 3','Zone 4'), selected = '1', width = '30%'),
                               numericInput("area", "Plot area", value = 500, width = '40%'),
                               div(style="display:inline-block",numericInput("AD", "Dominant Age", value = 20, width = '30%')),
                               div(style="display:inline-block",numericInput("HD", "Dominant height", value = 20, width = '30%')),
                               div(style="display:inline-block",numericInput("SI", "Site index", value = 20, width = '30%')),
                               numericInput("AF", "Final Age", value = 30, width = '50%'),
                               actionButton(inputId = "runstand",label =  "Run!", class = "btn-primary")
             )
      )
    })

    output$input_type_text <- renderText({
      input$input_type
    })


  }


)
