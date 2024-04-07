# Shiny App to disaply thermal metrics for Willamette River basin salmon populations
# AH Fullerton, last updated 11/19/2023

library(shiny)
library(dplyr)
library(sf)
library(viridis)
library(wesanderson)
library(leaflet)
library(plotrix)
library(dygraphs)
library(xts)

if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")

#source("R/functions4Exceedance.R")
#source("R/load_data.R")


# Define UI for application 
ui <- fluidPage(

    navbarPage(
      "Willamette Thermal Metrics",
      tabPanel("Exceedance time series",
      # Sidebar panel for user selections 
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "spp",
                       label = "Species:",
                       choices = c("Chinook salmon" = "Chinook", "Steelhead (not yet functional)" = "steelhead"),
                       selected = "Chinook"),
          selectInput(inputId = "population",
                      label = "Population:",
                      choices = c("North Santiam" = "north-santiam", "South Santiam" = "south-santiam", "McKenzie" = "mckenzie", "Middle Fork" = "middle-fork"),
                      selected = "north-santiam"),
          selectInput(inputId = "popreach",
                      label = "Reach:",
                      choices = c("A" = "A", "B" = "B", "C" = "C", "D" = "D", "E" = "E", "F" = "F", "G" = "G",
                                  "Middle Fork to McKenzie" = "Mfk2Mck", "McKenzie to Santiam" = "Mck2San", "Santiam to Falls" = "San2Falls", "Falls to mouth" = "Falls2Mouth"), 
                      selected = "A"),            
          selectInput(inputId = "lifestage",
                      label = "Life stage:",
                      choices = c("Enroute" = "enroute", "Prespawn" = "prespawn", "Incubation" = "incubat", "Outmigration" = "outmigr"), 
                      selected = "prespawn"),
          radioButtons(inputId = "threshold",
                       label = "Temperature threshold:",
                       choices = c("State standard" = 1, "Plus 2 C" = 2),
                       selected = 1),
          hr(style = "border-top: 1px solid #000000;"),
          radioButtons(inputId = "colors",
                       label = "Plot palette:",
                       choices = c("Black" = 1, "Orange" = 2, "Yellow" = 3, "Blue" = 4, "Purple" = 5, "Green" = 6, "Aqua" = 7),
                       selected = 7)
        ), #end sidebarPanel
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("ExcPlot", width = "600px"),
          "Proportion of each LCM reach where stream temperature exceeds thresholds designated for the species, life stage, and population over time. 
           For each salmon population (e.g., 'North Santiam'), there are multiple 'reaches' used in life cycle models (LCM), denoted with capital 
           letters (e.g. 'Reach A'). 
           The plot  displays the median (line), 25th and 75th percentiles (dark shading of the same color) and minimum and maximum (lighter shading 
           of the same color) across smaller stream segments within each LCM reach (smaller segments are those in the National Hydrography Dataset 
           version 2, delineated by unique COMIDs).",
          hr(style = "border-top: 1px solid #000000;"),
          downloadButton('download1', "Download plot data"),
          downloadButton('download2', "Download raw data"),
          downloadButton("savePlot", "Save plot", icon = icon("download"))
        ) #end mainPanel
      ) #end sidebarLayout
    ), #end tabPanel
    
    tabPanel("Mapped metrics",
          sidebarLayout(
            sidebarPanel(
              radioButtons(inputId = "spp2",
                           label = "Species:",
                           choices = c("Chinook salmon" = "Chinook", "Steelhead (not yet functional)" = "steelhead"),
                           selected = "Chinook"),
              selectInput(inputId = "population2",
                          label = "Population:",
                          choices = c("North Santiam" = "north-santiam", "South Santiam" = "south-santiam", "McKenzie" = "mckenzie", "Middle Fork" = "middle-fork"),
                          selected = "north-santiam"),
              selectInput(inputId = "metric",
                          label = "Thermal metric:",
                          choices = c("Minimum weekly minimum" = "IWIT", "Average weekly average" = "AWAT", 
                                      "Maximum weekly average" = "MWAT", "Average weekly maximum" = "AWMT", 
                                      "Maximum weekly maximum" = "MWMT", "First week hot (Julian Day)" = "FirstWeekHotJD", 
                                      "Days exceeding incubation threshold" = "IncDaysAbv", 
                                      "Average weekly average during upstream migration" = "enroute_AWAT", 
                                      "Average weekly maximum during upstream migration" = "enroute_AWMT", 
                                      "Cumulative thermal exposure during upstream migration" = "enroute_CumExp", 
                                      "Date when upstream migration thermal exceedance reached (Julian Day)" = "enroute_DateExcJD",
                                      "Average weekly average during prespawn holding" = "enroute_AWAT", 
                                      "Average weekly maximum during prespawn holding" = "enroute_AWMT", 
                                      "Cumulative thermal exposure during prespawn holding" = "enroute_CumExp", 
                                      "Date when prespawn holding thermal exceedance reached (Julian Day)" = "enroute_DateExcJD",
                                      "January mean" = "January", "February mean" = "February", "March mean" = "March",
                                      "April mean" = "April", "May mean" = "May", "June mean" = "June", "July mean" = "July",
                                      "August mean" = "August", "September mean" = "September", "October mean" = "October",
                                      "November mean" = "November", "December mean" = "December"),
                          selected = "AWAT"),
              #textInput(inputId = "year", label = "Year:", value = "2000"),
              selectInput(inputId = "year", label = "Year:",
                          choices = seq(1950,2099),
                          selected = 2020),
              hr(style = "border-top: 1px solid #000000;"),
              downloadButton('download3', "Download all data")
             ), #end sidebarPanel
            mainPanel(
              plotOutput("ThrmMaps", width = "600px"),
              "Map of the selected thermal metric summarized for the species and population
               over a particular calendar year.",
              hr(style = "border-top: 1px solid #000000;"),
              downloadButton("savePlot2", "Save plot", icon = icon("download"))
            )# end mainPanel
          ) #end sidebarLayout
    ), #end tabPanel
    
    tabPanel("Reference",
             sidebarLayout(
               sidebarPanel(
                 "Map displays Willamette reaches used in life cycle models. Reach names correspond to those used for choosing how to 
                 summarize data in other tabs. Map created by Tyler Nodine.",
                 hr(style = "border-top: 1px solid #000000;"),
                 "Underlying data used to compute thermal metrics in this app were predicted by ",
                 tags$a(href="https://doi.org/10.1371/journal.pwat.0000119", "Siegel et al. (2023)"),
                 "and modified to better match empirical gage data below dams 
                 (", tags$a(href="https://docs.google.com/document/d/1za735kfIDPaxAzSTNVcXQwPzbJZrMnyl/edit?usp=sharing&ouid=110566173461919570663&rtpof=true&sd=true", "Fullerton and Bond 2024"),").",
                 hr(style = "border-top: 1px solid #000000;")
               ), #end sidebarPanel
               mainPanel(
                 img(src = "map.png", align = "center")
               )# end mainPanel
      ) #end sidebarLayout           
    ) #end tabPanel
  ) #end navbarPage
) #end fluidPage

# Define server logic 
server <- function(input, output) {

    updateSource <- reactive({
      return(input)
    })
    
    updatePlotExc <- reactive({

      species <- updateSource()$spp
      subwat <- updateSource()$population
      life.stage <- updateSource()$lifestage
      reach.sel <- updateSource()$popreach
      t.thresh <- updateSource()$threshold
      clr <- as.numeric(updateSource()$colors)
      
      # Set population/reaches
      sw <- fncSetWatershed(subwat = subwat)
      
      # Get life-stage specific info
      sls <- fncSetLifestage(species, life.stage = life.stage, thedata = sw[[4]], rchs = sw[[2]], noaa_nm = sw[[1]], subwat = subwat)
      ls.rchs <- sls[[1]]; thetitle <- sls[[2]]; thedata2 <- sls[[3]]; th <- sls[[4]]
      ls.rchs <- intersect(ls.rchs, unique(thedata2$LCM_Reach))
      thresh <- ifelse(t.thresh == 1, th, (th + 2))
      thedata2 <- thedata2[thedata2$Thresh == thresh,]
      
      # Summarize 
      rch_data <-
        thedata2 %>%
        group_by(LCM_Reach, year) %>%
        summarise(pExc = quantile(pExc, c(0, 0.25, 0.5, 0.75, 1)), prob = c(0, 0.25, 0.5, 0.75, 1))
      rch_data <- rch_data[rch_data$LCM_Reach %in% ls.rchs,]
      
      the.reach <- ifelse(nchar(reach.sel) == 1, ls.rchs[grep(paste0(" ", reach.sel), ls.rchs)], reach.sel)
      if(!the.reach %in% ls.rchs){
        plot(1:5, 1:5, type = 'n', axes = F, ylab = "", xlab = ""); legend("center", legend = "The selected reach has no data for this population and life stage", bty = 'n')
      } else {
        mycolors <- fncColors4Quantiles()
        # 1 -black, 2 -orange, 3 -yellow, 4 -blue, 5 -purple, 6 -green, 7 -aqua
        col2use <- mycolors[[clr]]
        td <- rch_data[rch_data$LCM_Reach == the.reach,]
        the.years <- sort(unique(td$year))
        plot(the.years, td$pExc[td$prob == 0.5], type = 'n', ylab = "Proportion of LCM reach exceeding temperature threshold", xlab = "Year", main = the.reach, las = 1, ylim = c(0, 1.2))
        polygon(c(the.years, rev(the.years)), c(td$pExc[td$prob == 0], rev(td$pExc[td$prob == 1])), border = NA, col = col2use[1])
        polygon(c(the.years, rev(the.years)), c(td$pExc[td$prob == 0.25], rev(td$pExc[td$prob == 0.75])), border = NA, col = col2use[2])
        lines(the.years, td$pExc[td$prob == 0.5], lwd = 2, col = col2use[3])
        abline(v = c(1950, 2000, 2050, 2100), lty = 3)
        abline(h = c(0.2, 0.4, 0.6, 0.8, 1), lty = 3)
        lw <- ifelse(species == "Chinook", 25, 28) 
        fncWrapLegend(legend_text = thetitle, legwidth = lw, col = col2use[3], cx = 1)
      }
      recordPlot()
    })
    
    output$ExcPlot <- renderPlot({
      updatePlotExc()
    })
    
    updateThrmMaps <- reactive({
    
      # update user input
      species <- updateSource()$spp2
      subwat <- updateSource()$population2
      met <- updateSource()$metric
      yy <- as.numeric(updateSource()$year)
      
      # select correct dataset and load
      if(subwat == "north-santiam") {subwat_dams <- c("Detroit", "BIG CLIFF"); noaa_nm <- "North Santiam"; subnm <- "nsan"}
      if(subwat == "south-santiam") {subwat_dams <- c("Green Peter", "Foster"); noaa_nm <- "South Santiam"; subnm <- "ssan"}
      if(subwat == "mckenzie") {subwat_dams <- c("Cougar", "Blue River Dam"); noaa_nm <- "McKenzie"; subnm <- "mckz"}
      if(subwat == "middle-fork") {subwat_dams <- c("Hills Creek", "DEXTER"); noaa_nm <- "Middle Fork"; subnm <- "mfrk"}
      
      streams <- get(paste0(subnm, "_streams"))
      subwat_huc <- get(paste0(subnm, "_huc"))
      temp.metrics <- get(paste0(subnm, "_metrics"))
      temp.metrics$FirstWeekHotJD <- as.POSIXlt(temp.metrics$FirstWeek)$yday
      temp.metrics$enroute_DateExcJD <- as.POSIXlt(temp.metrics$enroute_DateExc)$yday
      temp.metrics$prespawn_DateExcJD <- as.POSIXlt(temp.metrics$prespawn_DateExc)$yday
      
      # Merge NHD lines and predictions based on COMID
      streams$sortby <- as.numeric(rownames(streams))
      dat <- temp.metrics[temp.metrics$year == yy, ]
      dat <- dat[,c("COMID", met)] #data.table format
      dd <- merge(streams, dat, by = "COMID", all.x = T, sort = F)
      dd <- dd[order(dd$sortby),] #need to do this because the COMIDs with no predictions get shunted to the bottom and messes up sorting
      dd[is.na(dd[,met]),met] <- 0
      streams <- dd
      
      # Plot maps
      par(las = 1, cex = 0.9)
      
      # Color schemes
      color_range <- range(temp.metrics[,met][temp.metrics[,met] > 0], na.rm = T)
      color_range[1] <- floor(color_range[1] - 0.05 * color_range[1]); color_range[2] <- ceiling(color_range[2] + 0.05 * color_range[2])
      if(color_range[1] == 0) color_range[1] <- 1
      col_by <- round((color_range[2] - color_range[1] + 1) / 10)
      
      if(length(grep("Days", met)) > 0){
        colscheme <- viridis::viridis(length(seq(color_range[1], color_range[2], by = col_by)) - 1) #violet to teal to yellow
      }else if(length(grep("CumExp", met)) > 0){
        colscheme <- wesanderson::wes_palette("Zissou1", length(seq(color_range[1], color_range[2], by = col_by)) - 1, type = "continuous") #blue to red
        colscheme <- colscheme[1:length(colscheme)]
      }else if(length(grep("FirstWeek", met)) > 0){
        colscheme <- rev(viridis::mako(length(seq(color_range[1], color_range[2], by = col_by)) - 1)) #seagreen to blue to black (early to late)
      }else if(length(grep("DateExc", met)) > 0){
        colscheme <- rev(viridis::mako(length(seq(color_range[1], color_range[2], by = col_by)) - 1)) #seagreen to blue to black (early to late)
      } else {
        colscheme <- viridis::plasma(length(seq(color_range[1], color_range[2], by = col_by)) - 1) #violet to pink to yellow
      }
      
      # Plot basin and streams colored by metric
      plot(sf::st_geometry(subwat_huc), border = NA, col = "gray97", main = paste0(subwat, ": ", met, ", ", yy), axes = T)
      plot(sf::st_geometry(streams), col = "gray60", key.pos = NULL, reset = F, add = T)
      plot(streams[,met], breaks = seq(color_range[1], color_range[2], by = col_by), pal = colscheme, add = T)
      
      # legend
      addLegendToSFPlot(value_range = color_range, num_cats = length(colscheme), palette = colscheme, adjX = 0.05, adjY = 0.05)
      
      # add dam location(s) for reference
      plot(sf::st_geometry(dams[dams$DAM_NAME == subwat_dams[1],]), pch = 24, bg = "black", cex = 0.8, add = TRUE)
      plot(sf::st_geometry(dams[dams$DAM_NAME == subwat_dams[2],]), pch = 25, bg = "black", cex = 0.8, add = TRUE)
      legend("topleft", legend = c(paste(stringr::str_to_title(subwat_dams[1]), "dam"), paste(stringr::str_to_title(subwat_dams[2]), "dam")), pch = c(24, 25), pt.bg = "black", cex = 0.8, bty ='n')
      
      recordPlot()
    })
    
    output$ThrmMaps <- renderPlot({
      updateThrmMaps()
    })
    
    updateData <- reactive({
      species <- updateSource()$spp
      subwat <- updateSource()$population
      life.stage <- updateSource()$lifestage
      reach.sel <- updateSource()$popreach
      t.thresh <- updateSource()$threshold
      clr <- as.numeric(updateSource()$colors)
      
      # Set population/reaches
      sw <- fncSetWatershed(subwat = subwat)
      
      # Get life-stage specific info
      sls <- fncSetLifestage(species, life.stage = life.stage, thedata = sw[[4]], rchs = sw[[2]], noaa_nm = sw[[1]], subwat = subwat)
      ls.rchs <- sls[[1]]; thetitle <- sls[[2]]; thedata2 <- sls[[3]]; th <- sls[[4]]
      ls.rchs <- intersect(ls.rchs, unique(thedata2$LCM_Reach))
      thresh <- ifelse(t.thresh == 1, th, (th + 2))
      thedata2 <- thedata2[thedata2$Thresh == thresh,]
       
      # Summarize 
      rch_data <-
        thedata2 %>%
        group_by(LCM_Reach, year) %>%
        summarise(pExc = quantile(pExc, c(0, 0.25, 0.5, 0.75, 1)), prob = c(0, 0.25, 0.5, 0.75, 1))
      rch_data <- rch_data[rch_data$LCM_Reach %in% ls.rchs,]
      
      the.reach <- ifelse(nchar(reach.sel) == 1, ls.rchs[grep(paste0(" ", reach.sel), ls.rchs)], reach.sel)
      if(the.reach %in% ls.rchs){
        td <- rch_data[rch_data$LCM_Reach == the.reach,]
        rawdat <- thedata2[thedata2$LCM_Reach == the.reach,]
      } else {
        td <- out <- NULL
      }
      
      return(list(td, rawdat))
    })
    
    updateData2 <- reactive({
      
      # update user input
      species <- updateSource()$spp2
      subwat <- updateSource()$population2
      met <- updateSource()$metric
      yy <- as.numeric(updateSource()$year)
      
      # select correct dataset and load
      if(subwat == "north-santiam") {subnm <- "nsan"}
      if(subwat == "south-santiam") {subnm <- "ssan"}
      if(subwat == "mckenzie") {subnm <- "mckz"}
      if(subwat == "middle-fork") {subnm <- "mfrk"}
      
      return(get(paste0(subnm, "_metrics")))

    })
    
    output$download1 <- downloadHandler(
      filename = function(){paste0(updateSource()$population, "_", updateSource()$popreach, "_", 
                            updateSource()$spp, "_", updateSource()$lifestage, "_exceedance_agg.csv")}, 
      content = function(fname){
        write.csv(updateData()[[1]], fname, row.names = FALSE)
      }
    )      
    output$download2 <- downloadHandler(
      filename = function(){paste0(updateSource()$population, "_", updateSource()$popreach, "_", 
                            updateSource()$spp, "_", updateSource()$lifestage, "_exceedance_raw.csv")}, 
      content = function(fname){
        write.csv(updateData()[[2]], fname, row.names = FALSE)
      }
    )  
    output$savePlot <- downloadHandler(
      filename = function(){paste0(updateSource()$population, "_", updateSource()$popreach, "_", 
                            updateSource()$spp, "_", updateSource()$lifestage, ".png")}, 
      content = function(file){
        png(file, width = 6, height = 4, res = 150, units = "in")
        replayPlot(updatePlotExc())
        dev.off()
       }
    )  
    output$download3 <- downloadHandler(
      filename = function(){paste0(updateSource()$population2, "_", updateSource()$spp, "_metrics.csv")}, 
      content = function(fname){
        write.csv(updateData2(), fname, row.names = FALSE)
      }
    )      
    output$savePlot2 <- downloadHandler(
      filename = function(){paste0(updateSource()$population2, "_", updateSource()$spp, "_", 
                            updateSource()$metric, "_", updateSource()$year, ".png")}, 
      content = function(file){
        png(file, width = 6, height = 4, res = 150, units = "in")
        replayPlot(updateThrmMaps())
        dev.off()
      }
    )  

}

# Run the application 
shinyApp(ui = ui, server = server)
