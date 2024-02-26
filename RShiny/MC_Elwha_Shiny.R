library(ggplot2)
library(tidyverse)
library(dplyr)
library(maps)
library(sf)
library(readxl)
#install.packages("leaflet")
install.packages("leaflet.markercluster")
#install.packages("shiny")
#install.packages("shinythemes")
library(leaflet)
library(shiny)
library(RColorBrewer)

# load shapefile
elwha_st <- st_read('geo/elwha_streams.shp')
daily_avg_temps <- read.csv('data/daily-avg-tmp.csv')
site.names <- read_excel("data/Temperature Site Names.xlsx", "Master")

unique(site.names$Temp_Alias)

# drop Z/M coords
elwha_shp <- st_zm(elwha_st)

# convert to dataframe
elwha_df <- st_as_sf(elwha_shp)

# plot elwha
elwha_map <- ggplot() +
  geom_sf(data=elwha_df) +
  labs(title="Elwha River")

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
  titlePanel("Demo R Shiny Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Site", label="Select Site", choices=unique(site.names$Temp_Alias),
                  selected="ES01")
    ),
    mainPanel(
      # display the map
      leafletOutput("map", height="650px"),
      # point plot
      plotOutput("ggplot", height="350px", width='600px')
    )
  )
)

sec_palette <- colorRampPalette(brewer.pal(length(unique(site.names$SECTION)), "Set1"))

server <- function(input, output) {
  # render plot
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = -123.5596, lat = 48.03, zoom = 10) %>%
      # add site markers, show site names when clicked
      addMarkers(data=site.names, ~LONG, ~LAT, popup=~Temp_Alias,
                 options=markerOptions(riseOnHover = TRUE))#, color=~colorFactor(SECTION, palette = "Dark2"))# %>% 
      #addMarkersCluster()
  })
  #output$ggplot <- renderPlot({
    # filter based on selected site
   # filtered <- site.names[site.names$Temp_Alias == input$Site, ]
    
    # plot elwha and site points
    #ggplot() +
     # geom_sf(data=elwha_df) +
     # geom_point(data=filtered, aes(x=LONG, y=LAT, color='red', size=3)) +
     # labs(title='Elwha River Sites') +
     # theme(legend.position='none')
  #})
   
}

shinyApp(ui = ui, server = server)
