# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)

# Load data --------------------------------------------------------------------

df <- read.csv("mergetemp_test.csv")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  #load data
  df <- read.csv("mergetemp_test.csv")
  
  df <- mutate(df, Date = as.Date(Date, format= "%d.%m.%Y"))
  
  output$scatterplot <- renderPlot({
    ggplot(data = df, aes(x = Date, y = Temp)) + geom_point()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)