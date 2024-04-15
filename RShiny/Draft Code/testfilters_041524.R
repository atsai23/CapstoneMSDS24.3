# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(dygraphs)

# Load data --------------------------------------------------------------------
temptest <- readRDS('temptest.rds')

#Get info for only sites we have measurements for
measured_sites <- readRDS('measured_sites.rds')

site_names <- temptest$Site

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Temperature Time Series by Site"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Select Section
      selectInput(
        'Section',
        label = 'Section',
        choices = unique(measured_sites$SECTION),
        selected = unique(measured_sites$SECTION)[1],
        multiple = TRUE
      ),

      #Select Metrics
      selectInput(
        'Metric',
        label = 'Metric',
        choice = c('Mean', 'Min', 'Max'),
        selected = 'Mean',
        multiple = TRUE
      ),
      
      #Select a site
      selectInput(
        'site',
        label = 'Choose a Site',
        choices = site_names,
        selected = site_names[0:5],
        multiple = TRUE
      )
      
    ),
    
    #Display graph
    mainPanel(dygraphOutput("scatterplot"))
    
  )
)

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  updateSource <- reactive({
    return(input)
  })
  
  #Make sites reactive to section
  newSites <- reactive({
    measured_sites %>% 
      filter(SECTION == updateSource()$Section) %>% select(Temp_Alias)
  })
  
  observeEvent(input$Section, {
    updateSelectInput(session, "site", choices = newSites())
  })
  
  #Filter for selected site
  selected_data <- reactive({
    temptest %>% select('Date', all_of(updateSource()$site))
  })

  #Generate plot
  output$scatterplot <- renderDygraph({
    dygraph(selected_data()) %>% dyRangeSelector()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)