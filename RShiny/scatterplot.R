# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

#Temperature Data
temp <- read.csv('data/daily-avg-tmp.csv')

#Drop NaNs
temp <- temp[!(is.na(temp$Temp)), ]

#Make sure date column is formated as date
temp <- mutate(temp, Date = as.Date(Date, format = "%Y-%m-%d"))

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
      #Dropdown for site selection
      selectInput(
        inputId = 'site',
        label = 'Choose a Site',
        choices = site_names,
        selected = site_names,
        multiple = TRUE
      ),
      
      sliderInput(
        'range',
        label = 'River KM',
        min = 0,
        max = max(measured_sites$RKM),
        value = c(0, 25)
      )
    ),
    
    #Display graph
    mainPanel(plotOutput('scatterplot'))
  )
)

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  selected_sites <- reactive({
    measured_sites[input$range[1]:input$range[2], "Temp_Alias"]
  })
  
  
  #Filter River Kilometer
  selected_data <- reactive({
    temp %>% filter(Site %in% selected_sites()) %>% select(Date:Temp)
    
  })
  
  output$scatterplot <- renderPlot({
    #Code for plots
    ggplot(data = selected_data(), aes(x = Date, y = Temp)) +
      geom_point(aes(color = factor(Site))) +
      labs(color = 'Site')
    #ideally which column has the labels would be from user input
  })

  
}


# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)