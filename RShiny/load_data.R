#Function to load data
#Make sure you are in a directory with data stored in /data and geo stored in /geo
load_data <- function(){
  df <- read.csv("mergetemp_test.csv")
  
  #Make sure date column is formated as date
  df <- mutate(df, Date = as.Date(Date, format= "%Y-%m-%d"))
  
  #Get site names
  site_names <- sort(unique(df$ward5)) 
  #hard coded- might want to have some flexibility for the name of the label column?
  
  # load shapefile
  elwha_st <- st_read('geo/elwha_streams.shp')
  daily_avg_temps <- read.csv('data/daily-avg-tmp.csv')
  site.names <- read_excel("data/Temperature Site Names.xlsx", "Master")
  
  unique(site.names$Temp_Alias)
  
  # drop Z/M coords
  elwha_shp <- st_zm(elwha_st)
  
  # convert to dataframe
  elwha_df <- st_as_sf(elwha_shp)
  
  # load in drainage basin shapefile
  drainage_network <- st_read("geo/WBD_Elwha.shp") %>% 
    st_transform('+proj=longlat +datum=WGS84')
}

