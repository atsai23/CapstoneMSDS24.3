# Load packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib)
library(dygraphs)

# Load data --------------------------------------------------------------------

#Temperature Data
temp_1 <- read.csv('data/daily-avg-tmp.csv')

temp_1$Date <- as.Date(temp_1$Date)

temp <- readRDS('temp.rds')

sites <- readRDS('measured_sites.rds')

site_names <- temp$Site

#Get info for only sites we have measurements for
#measured_sites <- sites %>% filter(Temp_Alias %in% temp$Site)

#Get list of site names
#site_names <- sort(unique(temp$Site))
#hard coded- might want to have some flexibility for the name of the label column?

# load shapefile
elwha_st <- st_read('geo/elwha_streams.shp')
#daily_avg_temps <- read.csv('data/daily-avg-tmp.csv')

# drop Z/M coords
elwha_shp <- st_zm(elwha_st)

# convert to dataframe
elwha_df <- st_as_sf(elwha_shp)

# load in drainage basin shapefile
drainage_network <- st_read("geo/WBD_Elwha.shp") %>%
  st_transform('+proj=longlat +datum=WGS84')


#UI Elements -------------------------------------------------------------------
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


#Code for map sidebar
map_filters <- sidebar(
  #Select Section
  selectInput(
    'Section',
    label = 'Section',
    choices = unique(sites$SECTION),
    selected = unique(sites$SECTION)[1],
    multiple = TRUE
  )
)

#Code for plots sidebar
plot_filters <- sidebar(
  #Select Section
  selectInput(
    'Section',
    label = 'Section',
    choices = unique(sites$SECTION),
    selected = unique(sites$SECTION)[1],
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

merged_data <- merge(temp_1, sites, by.y = "Temp_Alias", by.x = "Site")

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
  tabsetPanel(tabPanel("Map", 
                       fluid = TRUE,
                       page_sidebar(
                         title = "Map",
                         sidebar = map_filters,
                         # display the map
                         leafletOutput("map", height = "650px"),
                         # point plot
                         plotOutput("ggplot", height = "350px", width = '600px')
                         ) #End page_sidebar
                       ), #End tab panel
              #Tab for Plots
              tabPanel("Plot",
                       fluid = TRUE,
                       page_sidebar(
                         title = "Temperature",
                         sidebar = plot_filters,
                         #Min/max/avg cards
                         layout_columns(
                           fill = FALSE,
                           value_box(title = "Minimum Temperature", value = 5),
                           value_box(title = "Maximum Temperature", value = 3),
                           value_box(title = "Average Temperature", value = 1)
                           ),
                         #plots
                         cards[[1]],
                         cards[[2]]
                         )
                       )
        )
)#End fluid page
      
      # Define server ----------------------------------------------------------------
      server <- function(input, output, session) {
        updateSource <- reactive({
          return(input)
        })
        
        # filter leaflet map
        leaflet_marks <- reactive({
          sites %>% 
            filter(SECTION == updateSource()$Section)
        })
        
        ## leaflet map
        output$map <- renderLeaflet({
          leaflet() %>%
            #Basemap
            addProviderTiles(providers$OpenStreetMap) %>%
            setView(lng = -123.5596,
                    lat = 48.03,
                    zoom = 10) %>%
            # add site markers, show site names when clicked
            addPolygons(data = drainage_network,
                        weight = 3,
                        col = 'lightblue') %>%
            clearMarkers() %>% 
            addCircleMarkers(
              data = leaflet_marks(),
              ~ LONG,
              ~ LAT,
              weight = 1,
              opacity = 10,
              popup =  ~ Temp_Alias,
              options = markerOptions(riseOnHover = TRUE)
            )
        })
        
        ggplot_data <- reactive({
          # get lat and long from marker click
          lat <- input$map_marker_click$lat
          lng <- input$map_marker_click$lng
          
          # filter merged dataset
          filtered <- merged_data[merged_data$LAT %in% lat & merged_data$LONG %in% lng,]
          print(filtered)
        })
        
        output$ggplot <- renderPlot({
          #Code for plots
          ggplot(data = ggplot_data(), aes(x = Date, y = Temp)) +
            geom_line()
          #ideally which column has the labels would be from user input
        })
        
        #Make sites reactive to section
        newSites <- reactive({
          sites %>%
            filter(SECTION == updateSource()$Section) %>% select(Temp_Alias)
        })
        
        observeEvent(input$Section, {
          updateSelectInput(session, "site", choices = newSites())
        })
        
        #Filter for selected site
        selected_data <- reactive({
          temp_1 %>% select('Date', all_of(updateSource()$site))
        })
        
        stats <- reactive({
          selected_data() %>% rowwise(Date) %>%
            summarize(
              mean = rowMeans(pick(where(is.numeric)), na.rm = TRUE),
              min = min(pick(where(is.numeric)), na.rm = TRUE),
              max = max(pick(where(is.numeric)), na.rm = TRUE)
            )
        })
        
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