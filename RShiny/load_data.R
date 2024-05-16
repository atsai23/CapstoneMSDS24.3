library(tidyverse)
library(readxl)

#Function to load data
#Make sure you are in a directory with data stored in /data

#Temperature Data
temp <- read.csv('data/daily-avg-tmp.csv')
temp_interp <- read.csv('data/daily-avg-tmp-interp.csv')

#Drop NaNs
temp <- temp[!(is.na(temp$Temp)), ]

#Truncate site alias- assuming that MS16 = MS16_5 for example
temp <- mutate(temp, Site = str_split_i(Site, "_", 1))
temp_interp <- mutate(temp_interp, Site = str_split_i(Site, "_", 1))

#Make sure date column is formatted as date
temp <- mutate(temp, Date = as.Date(Date, format = "%Y-%m-%d"))
temp_interp <- mutate(temp_interp, Date = as.Date(Date, format = "%Y-%m-%d"))

#Pivot wider
temp_wide <- temp %>% pivot_wider(values_from = Temp, names_from = Site)

# Put interpolated temps in another column
temp_interp$interp_temps <- ifelse(temp_interp$interpolation_status,
                                      temp_interp$Temp, NA)

temp_interp$Temp <- ifelse(temp_interp$interpolation_status, NA, temp_interp$Temp)

saveRDS(temp_wide, 'temp.rds')
saveRDS(temp_interp, 'temp_interp.rds')

#Get site info
siteNames <- read_xlsx('data/Temperature Site Names.xlsx')

#Get info for only sites we have measurements for
measured_sites <- siteNames %>% filter(Temp_Alias %in% temp$Site)

saveRDS(measured_sites, 'measured_sites.rds')

#Get list of site names
uniqueSites <- sort(unique(measured_sites$Temp_Alias))
#hard coded- might want to have some flexibility for the name of the label column?
