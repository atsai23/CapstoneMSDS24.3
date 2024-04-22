# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)
library(readxl)
library(dygraphs)

# Load data --------------------------------------------------------------------
temp <- readRDS('data/temptest.rds')

#Get info for only sites we have measurements for
measured_sites <- readRDS('measured_sites.rds')

site_names <- temp$Site

temp <- readRDS('temp.rds')

#temp_long <- temp_long[!(is.na(temp_long$Temp)), ]

# Define UI --------------------------------------------------------------------

#Code to display plots
cards <- list(
  card(
    full_screen = TRUE,
    card_header("All Sites"),
    dygraphOutput("comboplot")
  ),
  card(
    full_screen = TRUE,
    card_header("Aggregated Statistics"),
    dygraphOutput("statplot")
  )
)

#Code for sidebar
filters <- sidebar(
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
  
)

ui <- page_sidebar(
  title = "Temperature",
  sidebar = filters,
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Minimum Temperature",
      value = 5
      #showcase = bsicons::bs_icon("align-bottom")
    ),
    value_box(
      title = "Maximum Temperature",
      value = 3
      #showcase = bsicons::bs_icon("align-center")
    ),
    value_box(
      title = "Average Temperature",
      value = 1
      #showcase = bsicons::bs_icon("handbag")
    )
  ),
  
  cards[[1]], cards[[2]]
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
    temp %>% select('Date', all_of(updateSource()$site))
  })

  stats <- reactive({
    selected_data() %>% rowwise(Date) %>% 
      summarize(mean = rowMeans(pick(where(is.numeric)), na.rm = TRUE), 
              min = min(pick(where(is.numeric)), na.rm = TRUE),
              max = max(pick(where(is.numeric)), na.rm = TRUE))})
  
  #Generate plot
  output$comboplot <- renderDygraph({
    dygraph(selected_data()) %>% dyRangeSelector()
  })
  
  output$statplot <- renderDygraph({
    dygraph(stats()) %>% dyRangeSelector()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)