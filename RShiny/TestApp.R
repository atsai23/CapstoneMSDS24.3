# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)

# Load data --------------------------------------------------------------------

load("sitedemo.rda")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("MS36", "MS39"),
                  selected = "MS36"),
      
      # Select variable for x-axis
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("Date"),
                  selected = "Date"),
      
      # Select variable for color
      #selectInput(inputId = "___",
                  #label = "____",
                  #choices = c(___),
                  #selected = "___")
      
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = sitedemo, aes_string(x = input$x, y = input$y
                                     )) +
      geom_point()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)