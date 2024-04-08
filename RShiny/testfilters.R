# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(dygraphs)

# Load data --------------------------------------------------------------------

#Temperature Data
#temp <- read.csv('data/daily-avg-tmp.csv')

#Drop NaNs
#temp <- temp[!(is.na(temp$Temp)), ]

#Make sure date column is formatted as date
#temp <- mutate(temp, Date = as.Date(Date, format = "%Y-%m-%d"))

temptest <- readRDS('temptest.rds')

#Get site info
sites <- read_xlsx('data/Temperature Site Names.xlsx')

#Get info for only sites we have measurements for
measured_sites <- sites %>% filter(Temp_Alias %in% temp$Site)

#Get list of site names
site_names <- sort(unique(measured_sites$Temp_Alias))
#hard coded- might want to have some flexibility for the name of the label column?

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Temperature Time Series by Site"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Select Section
      selectInput(
        'Section',
        label = 'Section',
        choices = sites$SECTION,
        selected = sites$SECTION[1]
      ),

      #Select a site
      selectInput(
        'site',
        label = 'Choose a Site',
        choices = site_names,
        #selected = site_names[0:5],
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
  sitesdropdown <- reactive({
    sites %>% filter(SECTION == updateSource()$Section) %>% select(Temp_Alias)
  })
  
  observeEvent(input$Section, {
    updateSelectInput(session, "site", choices = sitesdropdown())
  })
  

  #Display plot
  output$scatterplot <- renderDygraph({
    dygraph(temptest) %>% dyRangeSelector()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)