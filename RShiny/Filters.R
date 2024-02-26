# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

df <- read.csv("mergetemp_test.csv")
df <- mutate(df, Date = as.Date(Date, format= "%Y-%m-%d"))
site_names <- sort(unique(df$ward5))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    #Input(s)
    sidebarPanel(
      
      #Filter by site/group
      selectInput(
        inputID = "site",
        label = "Select Group/Site",
        choices = site_names,
        selected = 0,
        multiple = TRUE)
      
      )
    
    ),
    
  # Output: Show scatterplot
  mainPanel(
    plotOutput(outputId = "scatterplot")
    )
  )


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    sites_subset <- df %>% filter(ward5 %in% input$site) %>% select(Date:Temp)
    ggplot(data = sites_subset, aes(x = Date, y = Temp)) + geom_point()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)