# Functions for Willamette exceedance Rshiny app


# Set watershed-specific variables
fncSetWatershed <- function(subwat){
  
  if(subwat == "north-santiam") {
    subwat_dams <- c("Detroit", "BIG CLIFF")
    noaa_nm <- "North Santiam"
    rchs <- sort(LCM_Reaches[grep(noaa_nm, LCM_Reaches)])
    rchs <- c(rchs, "San2Falls", "Falls2Mouth")
    thedata <- nsan_data
    
  }
  if(subwat == "south-santiam") {
    subwat_dams <- c("Green Peter", "Foster")
    noaa_nm <- "South Santiam"
    rchs <- sort(LCM_Reaches[grep(noaa_nm, LCM_Reaches)])
    rchs <- c(rchs, "San2Falls", "Falls2Mouth")
    thedata <- ssan_data
    
  }
  if(subwat == "mckenzie") {
    subwat_dams <- c("Cougar", "Blue River Dam")
    noaa_nm <- "McKenzie"
    rchs <- sort(LCM_Reaches[grep(noaa_nm, LCM_Reaches)])
    rchs <- c(rchs, "Mck2San", "San2Falls", "Falls2Mouth")
    thedata <- mckz_data
    
  }
  if(subwat == "middle-fork") {
    subwat_dams <- c("Hills Creek", "DEXTER")
    noaa_nm <- "Middle Fork"
    rchs <- sort(LCM_Reaches[grep(noaa_nm, LCM_Reaches)])
    rchs <- c(rchs, "Mfk2Mck", "Mck2San", "San2Falls", "Falls2Mouth")
    thedata <- mfrk_data
    
  }
  # if(subwat == "upper-willamette"){
  #   subwat_dams <- NA
  #   noaa_nm <- "UW mainstem"
  #   rchs <- sort(LCM_Reaches[grep(noaa_nm, LCM_Reaches)])
  #   rchs <- c(rchs, "Mfk2Mck", "Mck2San")
  # }
  # if(subwat == "middle-swillamette"){
  #   subwat_dams <- NA
  #   noaa_nm <- "MW mainstem"
  #   rchs <- sort(LCM_Reaches[grep(noaa_nm, LCM_Reaches)])
  #   rchs <- c(rchs, "San2Falls", "Falls2Mouth")
  # }
  
  if(length(grep("Migration", rchs)) > 0) rchs <- rchs[-grep("Migration", rchs)] #remove Dam reaches
  
  return(list(noaa_nm, rchs, subwat_dams, thedata))
}

# Set life-stage/species specific variables
fncSetLifestage <- function(species = "Chinook", life.stage, thedata, rchs, noaa_nm, subwat = subwat){
  
  if(life.stage == "enroute") {
    ls.rchs <- unique(LCM_Migration$LCM_Reach[LCM_Migration$LCM_Reach %in% rchs])
    idx.enroute <- LCM_Migration$COMID[LCM_Migration$LCM_Reach %in% ls.rchs]
    if(!is.null(thedata)) thedata <- thedata[thedata$COMID %in% idx.enroute & thedata$Life.stage == "enroute",]
    th <- 20
    thetitle <-  paste0(species, " adult migration 15 Apr - 31 Jul, >", th, "C")
  }
  if(life.stage == "prespawn") {
    ls.rchs <- sort(unique(LCM_Spawning$LCM_Reach[LCM_Spawning$LCM_Reach %in% rchs]))
    idx.prespawn <- LCM_Spawning$COMID[LCM_Spawning$LCM_Reach %in% ls.rchs]
    if(!is.null(thedata)) thedata <- thedata[thedata$COMID %in% idx.prespawn & thedata$Life.stage == "prespawn",]
    th <- 16
    thetitle <-  paste0(species, " prespawn holding 1 Jul - 15 Sep, >", th, "C")
  }
  if(life.stage == "incubat") {
    ls.rchs <- sort(unique(LCM_Spawning$LCM_Reach[LCM_Spawning$LCM_Reach %in% rchs]))
    idx.incubat <- LCM_Spawning$COMID[LCM_Spawning$LCM_Reach %in% ls.rchs]
    if(!is.null(thedata)) thedata <- thedata[thedata$COMID %in% idx.incubat & thedata$Life.stage == "incubat",]
    th <- 13
    thetitle <- paste0(species, " egg incubation 15 Sep - 31 Jan, >", th, "C")
  }
  if(life.stage == "outmigr") {
    ls.rchs <- unique(LCM_All$LCM_Reach[LCM_All$LCM_Reach %in% rchs])
    ls.rchs <- sort(ls.rchs[grep(noaa_nm, ls.rchs)])
    if(subwat == "north-santiam") {ls.rchs <- c(ls.rchs, "San2Falls", "Falls2Mouth")}
    if(subwat == "south-santiam") {ls.rchs <- c(ls.rchs, "San2Falls", "Falls2Mouth")}
    if(subwat == "mckenzie") {ls.rchs <- c(ls.rchs, "Mck2San", "San2Falls", "Falls2Mouth")}
    if(subwat == "middle-fork") {ls.rchs <- c(ls.rchs, "Mfk2Mck", "Mck2San", "San2Falls", "Falls2Mouth")}
    idx.outmigr <- LCM_All$COMID[LCM_All$LCM_Reach %in% ls.rchs]
    if(!is.null(thedata)) thedata <- thedata[thedata$COMID %in% idx.outmigr & thedata$Life.stage == "outmigr",]
    th <- 18
    thetitle <-  paste0(species, " smolt migration 1 Feb - 1 Jun, >", th, "C")
  }
  
  return(list(ls.rchs, thetitle, thedata, th))
}

# COLORS 
fncColors4Quantiles <- function(){
  #Min/max
  c1.0<- rgb(226,226,226,150,NULL,255) #hex #e2e2e2, light gray
  c1.1<- rgb(252,217,156,150,NULL,255) #hex #fcd99c96 orange
  c1.2<- rgb(247,246,187,150,NULL,255) #hex #f7f6bb96 yellow
  c1.3<- rgb(176,191,252,150,NULL,255) #hex #b0bffc96 blue
  c1.4<- rgb(209,167,207,150,NULL,255) #hex #d1a7cf purple
  c1.5<- rgb(196,237,197,150,NULL,255) #hex #e2e2e2 green
  c1.6<- rgb(171,201,205,150,NULL,255) #hex #abc9cd aqua
  
  #Q1/Q3
  c2.0<- rgb(142,142,142,200,NULL,255) #hex #8e8e8e, gray
  c2.1<- rgb(234,173,68,200,NULL,255) #hex #eaad44c8 orange
  c2.2<- rgb(239,237,95,200,NULL,255) #hex #efed5fc8 yellow
  c2.3<- rgb(128,153,252,200,NULL,255) #hex #809ffcc8 blue
  c2.4<- rgb(137,101,136,200,NULL,255) #hex #896588 purple
  c2.5<- rgb(81,198,83,200,NULL,255) #hex #8e8e8e green
  c2.6<- rgb(74,146,155,200,NULL,255) #hex #4a929b aqua
  
  #Median
  c3.0<- rgb(5,5,5,255,NULL,255) #black
  c3.1<- rgb(244,155,2,255,NULL,255) #hex #f49b02ff orange
  c3.2<- rgb(206,193,8,255,NULL,255) # hex #cec108ff yellow
  c3.3<- rgb(3,38,178,255,NULL,255) #hex #0326b2ff blue
  c3.4<- rgb(97,18,104,255,NULL,255) #hex #611268 purple
  c3.5<- rgb(1,137,3,255,NULL,255) #green
  c3.6<- rgb(66,110,130,255,NULL,255) #hex #426e82 aqua
  
  c1 <- c(c1.0, c2.0, c3.0)
  c2 <- c(c1.1, c2.1, c3.1)
  c3 <- c(c1.2, c2.2, c3.2)
  c4 <- c(c1.3, c2.3, c3.3)
  c5 <- c(c1.4, c2.4, c3.4)
  c6 <- c(c1.5, c2.5, c3.5)
  c7 <- c(c1.6, c2.6, c3.6)

  return(list(c1, c2, c3, c4, c5, c6, c7))
}

# Wrap long legend
fncWrapLegend <- function(legend_text, legwidth, col, cx = 1){
  
  #plot(1:5, 1:5, type = 'n', axes = F, xlab = "", ylab = "")
  
  # Split the legend text into multiple lines
  legend_text_lines <- strwrap(legend_text, width = legwidth)  # Adjust the width as needed
  
  # Calculate the number of lines in the legend
  num_lines <- length(legend_text_lines)
  
  # Calculate the coordinates for the legend
  legend_x <- 1950  # Adjust the x-coordinate
  legend_y <- 1.25 # Adjust the y-coordinate
  line_height <- 0.1  # Adjust the height between lines
  
  # Add each line of the legend
  legend(legend_x, legend_y, 
         legend = legend_text_lines[1], cex = cx, lwd = 2, col = col, bty = "n")
  for (i in 2:num_lines) {
    legend(legend_x, legend_y - (i - 1) * line_height, 
           legend = legend_text_lines[i], cex = cx, lwd = 2, col = NA, bg = NA, bty = "n")
  }
}

# Plot map legend function for use with sf plots
#library(sf); library(leaflet); library(plotrix)
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

