# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

df <- read.csv("mergetemp_test.csv")

#Make sure date column is formated as date
df <- mutate(df, Date = as.Date(Date, format= "%Y-%m-%d"))

#Get site names
site_names <- sort(unique(df$ward5)) 
#hard coded- might want to have some flexibility for the name of the label column?


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Temperature Time Series by Site"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Dropdown for site selection
      selectInput('site',
        label = 'Choose a Site',
        choices = site_names,
        selected = site_names[0],
        multiple = TRUE
        )
      ),
    
    #Display graph
    mainPanel(plotOutput('scatterplot')
      )
    )
  )

# Define server ----------------------------------------------------------------
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    #Filter for just the selected sites
    sites_subset <- df %>% filter(ward5 %in% input$site) %>% select(Date:Temp)

    #Code for plots
    ggplot(data = sites_subset, aes(x = Date, y = Temp)) + 
      geom_point(aes(color = factor(ward5))) 
    #ideally which column has the labels would be from user input
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)