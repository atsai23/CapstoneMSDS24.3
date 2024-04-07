#Function to load data
#Make sure you are in a directory with data stored in /data

#Temperature Data
temp <- read.csv('data/daily-avg-tmp.csv')

#Drop NaNs
temp <- temp[!(is.na(temp$Temp)), ]

#Make sure date column is formated as date
temp <- mutate(temp, Date = as.Date(Date, format = "%Y-%m-%d"))

#Pivot wider
temp <- temp %>% pivot_wider(values_from = Temp, names_from = Site)

saveRDS(temp, file = 'temptest.rds')

