#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
#

library(ggplot2)
library(nleqslv)

# Define UI for application
ui <- fluidPage(
  titlePanel("Labor Discipline Model"),
  sidebarPanel(
    numericInput(
      inputId = "b",
      label = "Unemployment benefits (as an 'hourly' wage).",
      value = 15, 
      min = 0
    ),
    br(),
    numericInput(
      inputId = "d",
      label = "Duration of unemployment benefits (weeks).",
      value = 16, 
      min = 0
    ),
    br(),
    numericInput(
      inputId = "s",
      label = "Expected spell of unemployment (as a number of weeks).",
      value = 16, 
      min = 0
    ),
    br(),
    sliderInput(
      inputId = "m",
      label = "Intensity of worker monitoring by the employer (0 = least intense; 1 = most intense).",
      value = 0.5, 
      min = 0,
      max = 1
    ),
    br(),
    sliderInput(
      inputId = "u",
      label = "Unemployment (as a proportion).",
      value = 0.04, 
      min = 0,
      max = 0.25
    ),
    br(),
    sliderInput(
      inputId = "a",
      label = "Worker's preference for leisure compared to consumption (0 = lowest preference for leisure; 1 = highest).",
      value = 0.5, 
      min = 0,
      max = 1
    ),
    br(),
    actionButton("go", "Show the graph!"),
    br(),
  ),
  mainPanel(plotOutput(
    "plot", width = "450px", height = "450px"
  ), 
  tableOutput(
    "table"
  ))
)

# Define server logic
server <- function(input, output) {
  library(alabama)
  library(ggplot2)
  source("source/laborDiscipline.r")
  v <- reactiveValues(
    # Solutions
    eq0 = NULL, 
    eq1 = NULL,
    # Graph with user-defined changes
    g = NULL,
    # Table with user-defined changes
    r = NULL
  )
  
  observeEvent(input$go, {
    # Initial model parameters
    b0 = 15
    d0 = 16 
    s0 = 16 
    m0 = 0.5 
    u0 = 0.04 
    a0 = 0.5 
    # Adds the new parameters
    b1 = input$b 
    d1 = input$d 
    s1 = input$s
    m1 = input$m 
    u1 = input$u
    a1 = input$a
    # Solves the equilibriums
    eq0 = laborDiscipline(b0, d0, s0, m0, u0, a0)
    eq1 = laborDiscipline(b1, d1, s1, m1, u1, a1)
    # Graph 
    v$g = plot.laborDiscipline(eq0, eq1, color1 = 'red')
    # Initial results table 
    v$r = data.frame(
      "Initial Equilibrium" = c(b0, d0, s0, m0, u0, a0, eq0$wage, eq0$effort),
      "New Equilibrium" = c(b1, d1, s1, m1, u1, a1, eq1$wage, eq1$effort),
      row.names = c(
        "Unemployment Benefit",
        "Duration of Benefits",
        "Unemployment Duration",
        "Monitoring Intensity",
        "Unemployment Rate",
        "Leisure Preference",
        "Wage",
        "Effort"
      )
    )
  }
  )
  output$table <- renderTable({
    v$r
  },
  rownames = TRUE, colnames = TRUE)
  output$plot <- renderPlot({
    v$g
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
