# Examine Elwha stream temperature data

library(readxl)
library(data.table)
library(sf)
library(dplyr)

# EMPIRICAL TEMPERATURES ####
# Read Elwha temperature database (years 2000 - 2023)
tdl <- fread("data/elwha.cleaned.2000-2023.csv")
str(tdl); summary(tdl)

# Get site attribute information
site.names <- read_xlsx("data/Temperature Site Names.xlsx", "Master")
all.sites <- sort(unique(site.names$Temp_Alias))
sites <- unique(tdl$Site)
setdiff(all.sites, sites) # sites with no data - either logger was lost or there were daily but not hourly data

# Quick look at what years there are data for each site (see also the Fullerton 2018 report)
par(mfrow = c(4,4), mar = c(4,5,2,0))
for(s in sites){
  plot(tdl$DateTime[tdl$Site == s], tdl$Temp[tdl$Site == s], type = 'l', las = 1, ylab = "Temperature (C)", xlab = "", main = s)
}



## MODELED TEMPERATURES #####
# Read in predicted (modeled) stream temperatures; these are daily for each reach
# Note that there are spatial gaps where the old reservoirs were
# Elwha HUC (hydrologic unit code) is 17110020, combined with Dungeness
tpreds <- read.csv("data/st_preds/st_pred_171100/st_pred_1711002001.csv")
for(t in c(2:5,7)){
  tdat <- read.csv(paste0("data/st_preds/st_pred_171100/st_pred_171100200", t, ".csv")); tpreds <- rbind.data.frame(tpreds, tdat)
}
tpreds$tim.date <- as.POSIXlt(tpreds$tim.date)
str(tpreds); summary(tpreds)

# Load shapefile of Elwha River watershed streams
streams <- st_read("data/shapefiles/elwha_streams.shp")

# Custom legend function
addLegendToSFPlot <- function(value_range = c(0, 1), num_cats = 2, palette = c("blue", "red"), adjX = 0, adjY = 0, ...){
  #modified from: https://stackoverflow.com/questions/52975447/reorganize-sf-multi-plot-and-add-a-legend
  
  # Get the axis limits and calculate size
  axisLimits <- par()$usr
  xLength <- axisLimits[2] - axisLimits[1]
  yLength <- axisLimits[4] - axisLimits[3]
  adjX <- adjX * xLength
  adjY <- adjY * yLength
  
  # Define values cuts and make pretty labels
  values <- seq(value_range[1], value_range[2], length.out = num_cats)
  labels <- paste(" ", sprintf("%02d", round(seq(value_range[1], value_range[2], length.out = num_cats))))
  labels2 <- paste(" ", sprintf("%02d", seq(value_range[1], value_range[2], by = 5)))
  if(length(labels) > num_cats) labels <- labels2
  
  # Define the colour palette
  colourPalette <- leaflet::colorNumeric(palette, range(values))
  
  # Add the legend
  plotrix::color.legend(xl = axisLimits[1] + adjX - 0.025 * xLength, xr = axisLimits[1] + adjX,
                        yb = axisLimits[3] + adjY, yt = axisLimits[3] + 0.4 * yLength + adjY,
                        legend = labels, rect.col = colourPalette(values), 
                        gradient = "y", align = 'rb', cex = 0.8, ...)
} 

# Limit data to a single day for mapping
the.date <- as.POSIXlt("2015-08-01")
dat <- tpreds[tpreds$tim.date == the.date,]
streams.sub <- left_join(streams, dat, by = "COMID")
streams.sub <- st_zm(streams.sub)
streams.dat <- as.data.frame(streams.sub)

# Set color range
color_range <- range(streams.dat$prd.stream_temp[streams.dat$prd.stream_temp > 0], na.rm = T)
color_range[1] <- floor(color_range[1] - 0.05 * color_range[1]); color_range[2] <- ceiling(color_range[2] + 0.05 * color_range[2])
if(color_range[1] == 0) color_range[1] <- 1
col_by <- round((color_range[2] - color_range[1] + 1) / 13)
colscheme <- viridis::plasma(length(seq(color_range[1], color_range[2], by = col_by)) - 1) #violet to pink to yellow

# Plot predicted stream temperatures
png(paste0("map.png"), width = 5, height = 8, units = "in", res = 300)
par(oma = rep(0,4), mar = rep(1,4))
plot(st_geometry(streams.sub), col = "gray70", key.pos = NULL, reset = F, cex = 0.8, main = the.date)
plot(streams.sub[,"prd.stream_temp"], breaks = seq(color_range[1], color_range[2], by = col_by), pal = colscheme, add = T)
addLegendToSFPlot(value_range = color_range, num_cats = length(colscheme), palette = colscheme, adjX = 0.05, adjY = 0.05)
dev.off()
