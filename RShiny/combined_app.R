# Load packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)

# Load data --------------------------------------------------------------------

#Temperature Data
temp <- read.csv('data/daily-avg-tmp.csv')

#Drop NaNs
temp <- temp[!(is.na(temp$Temp)),]

#Make sure date column is formated as date
temp <- mutate(temp, Date = as.Date(Date, format = "%Y-%m-%d"))

#Get site info
sites <- read_xlsx('data/Temperature Site Names.xlsx')

#Get info for only sites we have measurements for
measured_sites <- sites %>% filter(Temp_Alias %in% temp$Site)

#Get list of site names
site_names <- sort(unique(temp$Site))
#hard coded- might want to have some flexibility for the name of the label column?


# load shapefile
elwha_st <- st_read('geo/elwha_streams.shp')
daily_avg_temps <- read.csv('data/daily-avg-tmp.csv')
site.names <-
  read_excel("data/Temperature Site Names.xlsx", "Master")

unique(site.names$Temp_Alias)

# drop Z/M coords
elwha_shp <- st_zm(elwha_st)

# convert to dataframe
elwha_df <- st_as_sf(elwha_shp)

# load in drainage basin shapefile
drainage_network <- st_read("geo/WBD_Elwha.shp") %>%
  st_transform('+proj=longlat +datum=WGS84')


# plot elwha
elwha_map <- ggplot() +
  geom_sf(data = elwha_df) +
  labs(title = "Elwha River")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$script(
      # adjust size of map based on window size
      '
      $(window).on("resize", function() {
        var map = $("#map);
        map.height($(window).height() - map.offset().top - 20);
      });
      '
    )
  ),
  
  titlePanel("Temperature Time Series by Site"),
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "Site",
                   label = "Select Site",
                   choices = unique(site.names$Temp_Alias),
                   selected = "ES01"
                 )
               ),
               mainPanel(
                 # display the map
                 leafletOutput("map", height = "650px"),
                 # point plot
                 plotOutput("ggplot", height = "350px", width =
                              '600px')
               )
             )),
    #Tab for scatterplot
    tabPanel("Plot", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 #Dropdown for site selection
                 selectInput(
                   'site',
                   label = 'Choose a Site',
                   choices = site_names,
                   selected = site_names[0:5],
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
               #Display the plot
               mainPanel(plotOutput('scatterplot'))
             ))
  )
)

sec_palette <-
  colorRampPalette(brewer.pal(length(unique(site.names$SECTION)), "Set1"))

# Define server ----------------------------------------------------------------
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    #Filter for just the selected sites
    sites_subset <-
      temp %>% filter(Site %in% input$site) %>%
      select(Date:Temp)
    
    #Code for plots
    ggplot(data = sites_subset, aes(x = Date, y = Temp)) +
      geom_point(aes(color = factor(Site))) +
      labs(color = 'Site')
    #ideally which column has the labels would be from user input
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      #Basemap
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = -123.5596,
              lat = 48.03,
              zoom = 10) %>%
      # add site markers, show site names when clicked
      addPolygons(data = drainage_network,
                  weight = 5,
                  col = 'blue') %>%
      addMarkers(
        data = site.names,
        ~ LONG,
        ~ LAT,
        popup =  ~ Temp_Alias,
        options = markerOptions(riseOnHover = TRUE)
      )
  })
  
}
# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)