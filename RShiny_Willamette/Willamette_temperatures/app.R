# Shiny App to disaply temperature time series for Willamette River basin salmon populations
# AH Fullerton, last updated 3/20/24

library(shiny)
library(dplyr)
library(dygraphs)
library(xts)


# Define UI for application 
ui <- fluidPage(

    navbarPage(
      "Willamette Stream Temperatures",
      tabPanel("Annual time series",
               # Sidebar panel for user selections 
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "population1",label = "Population:",
                               choices = c("North Santiam", "South Santiam", "McKenzie", "Middle Fork", "Mainstem"),
                               selected = "North Santiam"),
                   selectInput(inputId = "cscen1", label = "Climate scenario:",
                               choices = c("Retrospective","CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", 
                                           "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC", "ST_med"),
                               selected = "Retrospective"),
                   selectInput(inputId = "year1", label = "Year:",
                               choices = seq(1950,2099),
                               selected = 2020),
                   checkboxInput(inputId = "means_only", label = "Means only",
                                value = FALSE),
                   downloadButton('download1', "Download plot data")

                 ), #end sidebarPanel
                 # Show a plot of the generated distribution
                 mainPanel(
                   dygraphOutput("dyPlot1", width = "700px"),
                   hr(), 
                   "Annual stream temperature time series in the Willamette River basin predicted by Siegel et al. (2023),
                   and modified to better match empirical gage data below dams (Fullerton and Bond 2024). 
                   For each salmon population (e.g., 'North Santiam'), there are multiple 'reaches' used in life cycle models (LCM), denoted with capital 
                   letters (e.g. 'Reach A') and unique colors. The plot  displays the mean (line), minimum and maximum (shading of the same color) across 
                   smaller stream segments within each LCM reach (smaller segments are those in the National Hydrography Dataset version 2,
                   delineated by unique COMIDs). The LCM populations and reaches are shown below the plot in a map produced by Tyler Nodine for reference.
                   Default plot values are 7-day rolled means; this can be changed in the text box near the plot's origin.
                   Population, climate scenario, and year can be changed in the sidebar to the left.
                   The roller bar at the bottom allows visualization of different temporal periods. Mousing over the plot identifies specific data.",
                   hr(), 
                   img(src = "map.png", align = "center"),
                   hr(),
                   "Siegel, JE, AH Fullerton, AM FitzGerald, D Holzer, CE Jordan. 2023. ", 
                   tags$a(href="https://doi.org/10.1371/journal.pwat.0000119", "Daily stream temperature predictions 
                   for free-flowing streams in the Pacific Northwest, USA"),
                   ". PLoS Water2(8): e0000119. https://doi.org/10.1371/ journal.pwat.0000119.",
                   hr(),
                   "Fullerton, AH, M Bond. 2024. ", tags$a(href="https://docs.google.com/document/d/1za735kfIDPaxAzSTNVcXQwPzbJZrMnyl/edit?usp=sharing&ouid=110566173461919570663&rtpof=true&sd=true", 
                   "Willamette River stream temperature modeling"),". Internal methods report, March 7, 2024.",
                   hr()
                  ) #end mainPanel
               ), #end sidebarLayout
      ), #end tabPanel

      tabPanel("Entire time series",
               # Sidebar panel for user selections 
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "population2",label = "Population:",
                               choices = c("North Santiam", "South Santiam", "McKenzie", "Middle Fork", "Mainstem"),
                               selected = "North Santiam"),
                   selectInput(inputId = "cscen2", label = "Climate scenario:",
                               choices = c("Retrospective","CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", 
                                           "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC", "ST_med"),
                               selected = "Retrospective"),
                   hr(),
                   downloadButton('download2', "Download plot data")
                   
                 ), #end sidebarPanel
                 # Show a plot of the generated distribution
                 mainPanel(
                   dygraphOutput("dyPlot2", width = "700px"),
                   hr(), 
                   "Stream temperature across the entire time series in the Willamette River basin predicted by ",
                   tags$a(href="https://doi.org/10.1371/journal.pwat.0000119", "Siegel et al. (2023)"),
                   "and modified to better match empirical gage data below dams (",
                   tags$a(href="https://docs.google.com/document/d/1za735kfIDPaxAzSTNVcXQwPzbJZrMnyl/edit?usp=sharing&ouid=110566173461919570663&rtpof=true&sd=true", "Fullerton and Bond 2024"),"). 
                   For each salmon population (e.g., 'North Santiam'), there are multiple 'reaches' used in life cycle models (LCM), denoted with capital 
                   letters (e.g. 'Reach A') and unique colors. The plot  displays the mean (line), minimum and maximum (shading of the same color) across 
                   smaller stream segments within each LCM reach (smaller segments are those in the National Hydrography Dataset version 2,
                   delineated by unique COMIDs).
                   Default plot values are 7-day rolled means; this can be changed in the text box near the plot's origin.
                   Population and climate scenario can be changed in the sidebar to the left. 
                   The roller bar at the bottom allows visualization of different temporal periods. Mousing over the plot
                   identifies specific data."
                 ) #end mainPanel
               ), #end sidebarLayout
      ), #end tabPanel
      
    ) #end navbarPage
) #end fluidPage

# Define server logic 
server <- function(input, output, session) {

    updateSource <- reactive({
      return(input)
    })
    
# ANNUAL TIME SERIES TAB ----
    
    # Make drop-down choice of year dependent upon user input of cscen
    yrdropdown1 = reactive({
      if(updateSource()$cscen1 == "Retrospective") {
        seq(1990, 2021)
      } else {
        seq(1950, 2099)
      }
    })
    
    observeEvent(input$cscen1, {
      updateSelectInput(session, "year1", choices = yrdropdown1())
    })
    
    updateData1 <- reactive({
      pop1 <- updateSource()$population1
      yy1 <- updateSource()$year1
      cs1 <- updateSource()$cscen1
      pop1b <- gsub(" ", "", pop1)
      cs1b <- gsub("-", "_", cs1)
      if(cs1 == "Retrospective"){ 
        data.list <- get(paste0(pop1b, ".data.list"))
      } else {
        data.list <- get(paste0(pop1b, ".", cs1b, ".data.list"))
      }
      xdt1 <- xts::as.xts(as.data.frame(data.list[[paste0("dat.", pop1, ".", yy1)]]))
      df1 <- cbind.data.frame("Date" = index(xdt1), xdt1); row.names(df1) <- NULL
      return(df1)
    })
      
    output$dyPlot1 <- renderDygraph({
      pop1 <- updateSource()$population1
      yy1 <- updateSource()$year1
      cs1 <- updateSource()$cscen1
      mo <- updateSource()$means_only
      pop1b <- gsub(" ", "", pop1)
      cs1b <- gsub("-", "_", cs1)
      if(cs1 == "Retrospective"){ 
        data.list <- get(paste0(pop1b, ".data.list"))
      } else {
        data.list <- get(paste0(pop1b, ".", cs1b, ".data.list"))
      }
      xdt1 <- xts::as.xts(as.data.frame(data.list[[paste0("dat.", pop1, ".", yy1)]]))
      reaches1 <- gsub("Mean.", "", colnames(xdt1)); reaches1 <- gsub("Min.", "", reaches1); reaches1 <- gsub("Max.", "", reaches1); reaches1 <- unique(reaches1)
      for(r in 1:length(reaches1)){
        v <- c(paste0("Min.", reaches1[r]), paste0("Mean.", reaches1[r]), paste0("Max.", reaches1[r]))
        assign(paste0("v",r), v)
        if(pop1 != "Mainstem"){
          l <- paste0(pop1, substr(reaches1[r], nchar(reaches1[r]) - 1, nchar(reaches1[r])))
        } else {
          l <- gsub("Mainstem", "MS", reaches1[r])
        }
        assign(paste0("l",r), l)
      }
      rm(v,l)
      if(mo == T){
        xdt1 <- xdt1[,grep("Mean", colnames(xdt1))]
        for(r in 1:length(reaches1)){
          v <- c(paste0("Mean.", reaches1[r]))
          assign(paste0("v",r), v)
        }
      }
      
      dygraph(xdt1) %>%
        dySeries(.,v1, label = l1) %>%
        {if(length(reaches1) > 1) dySeries(.,v2, label = l2) else .} %>%
        {if(length(reaches1) > 2) dySeries(.,v3, label = l3) else .} %>%
        {if(length(reaches1) > 3) dySeries(.,v4, label = l4) else .} %>%
        {if(length(reaches1) > 4) dySeries(.,v5, label = l5) else .} %>%
        {if(length(reaches1) > 5) dySeries(.,v6, label = l6) else .} %>%
        {if(length(reaches1) > 6) dySeries(.,v7, label = l7) else .} %>%
        dyRoller(showRoller = T, rollPeriod = 7) %>%
        dyRangeSelector() %>%
        dyLegend(width = 160, labelsSeparateLines = T) %>%
        dyAxis("y", label = "Stream temperature (C)")
    })
    
    output$download1 <- downloadHandler(
      filename = function(){paste0(updateSource()$population1, "_", updateSource()$cscen1, "_", 
                                   updateSource()$year1, ".csv")}, 
      content = function(fname){
        write.csv(updateData1(), fname, row.names = FALSE)
      }
    )
    
# WHOLE TIME SERIES TAB ----
    updateData2 <- reactive({
      pop2 <- updateSource()$population2
      cs2 <- updateSource()$cscen2
      pop2b <- gsub(" ", "", pop2)
      cs2b <- gsub("-", "_", cs2)
      if(cs2 == "Retrospective"){ 
        xdt2 <- xts::as.xts(as.data.frame(retro.means))
      } else {
        xdt2 <- get(paste0(cs2b, ".means"))
        xdt2 <- xts::as.xts(as.data.frame(xdt2))
      }
      reaches2 <- unique(colnames(xdt2))
      reaches2 <- reaches2[grep(pop2, reaches2)]
      if(length(reaches2[grep("Migration", reaches2)]) > 0) reaches2 <- reaches2[-grep("Migration", reaches2)]
      xdt2 <- xdt2[,colnames(xdt2) %in% reaches2]
      
      df2 <- cbind.data.frame("Date" = index(xdt2), xdt2); row.names(df2) <- NULL
      return(df2)
    })
    
    output$dyPlot2 <- renderDygraph({
      pop2 <- updateSource()$population2
      cs2 <- updateSource()$cscen2
      pop2b <- gsub(" ", "", pop2)
      cs2b <- gsub("-", "_", cs2)
      if(cs2 == "Retrospective"){ 
        xdt2 <- xts::as.xts(as.data.frame(retro.means))
      } else {
        xdt2 <- get(paste0(cs2b, ".means"))
        xdt2 <- xts::as.xts(as.data.frame(xdt2))
      }
      reaches2 <- unique(colnames(xdt2))
      reaches2 <- reaches2[grep(pop2, reaches2)]
      if(length(reaches2[grep("Migration", reaches2)]) > 0) reaches2 <- reaches2[-grep("Migration", reaches2)]
      xdt2 <- xdt2[,colnames(xdt2) %in% reaches2]

      dygraph(xdt2) %>%
        dyRoller(showRoller = T, rollPeriod = 7) %>%
        dyRangeSelector() %>%
        dyLegend(width = 210, labelsSeparateLines = T) %>%
        dyAxis("y", label = "Stream temperature (C)", valueRange = c(0,30))
    })
    

    output$download2 <- downloadHandler(
      filename = function(){paste0(updateSource()$population2, "_", updateSource()$cscen2, ".csv")}, 
      content = function(fname){
        write.csv(updateData2(), fname, row.names = FALSE)
      }
    )
    
    output$dyPlot2 <- renderDygraph({
      pop2 <- updateSource()$population2
      cs2 <- updateSource()$cscen2
      pop2b <- gsub(" ", "", pop2)
      cs2b <- gsub("-", "_", cs2)
      if(cs2 == "Retrospective"){ 
        xdt2 <- xts::as.xts(as.data.frame(retro.means))
      } else {
        xdt2 <- get(paste0(cs2b, ".means"))
        xdt2 <- xts::as.xts(as.data.frame(xdt2))
      }
      reaches2 <- unique(colnames(xdt2))
      reaches2 <- reaches2[grep(pop2, reaches2)]
      if(length(reaches2[grep("Migration", reaches2)]) > 0) reaches2 <- reaches2[-grep("Migration", reaches2)]
      xdt2 <- xdt2[,colnames(xdt2) %in% reaches2]

      dygraph(xdt2) %>%
        dyRoller(showRoller = T, rollPeriod = 7) %>%
        dyRangeSelector() %>%
        dyLegend(width = 210, labelsSeparateLines = T) %>%
        dyAxis("y", label = "Stream temperature (C)", valueRange = c(0,30))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
#runGadget(ui, server, viewer = dialogViewer("Willamette temperatures", width = 1200, height = 2000))
#runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
#runGadget(ui, server, viewer = paneViewer(minHeight = 500))
