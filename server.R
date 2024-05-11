# DATAVIS FINAL SERVER
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(sp)
library(shinyjs)
library(forcats)
library(rgeos)
library(readr)
library(rgdal)
library(networkD3)

shinyServer(function(input, output, session) {
  # react values---------------------------------------------------------------------------------
  # caseFiles <- reactiveVal (totalFrame)
  # missingDF <- reactiveVal(missingreacDF)
  # dataBase <- reactiveVal(dataBase)
  # requestList <- reactiveVal(requestList)
  activeButton <- reactiveVal(NULL)
  # startDate <- reactiveVal(NULL)
  # endDate <- reactiveVal(NULL)
  
  # dataset ---------------------------------------------------------------------------------------------
  
  # determines if the user has uploaded any data into the system 
  dataFileUpload <- reactive({
    caseFiles()%>%
      select(-c("psolcolors", "nodcolors"))
  }) # dataFileUpload
  
  # determines if the user has uploaded any data into the system 
  dataFileUpdate <- reactive({
    # conducts validation and checking of data frames
    sortedData <- dataValidation(caseFiles()) # removes any entries that are not accepted by Pro Staff/ are not denoted in case form
    sortedData <- splitData(sortedData) # splitting data into entries with coordinates and those with only relative coordinates
    coordDF <- data.frame(sortedData[1])#dataframe with coordinates
    rLocationDF <- data.frame(sortedData[2]) # dataframe relative positions
    coordDF <- prepCoordFrame(coordDF)# reformating dataframe columns to contain Lat & Long and be same as coordDF
    dataBase = read_csv('RLocationLatLongs.csv', col_names = TRUE) #getting dataframe with saved Lat and Longs gained from click map
    rLocationDF <- PopulateLocationData(dataBase, rLocationDF) 
    missingreacDF <- FindMissingLocationData(rLocationDF) # separate entries that do not still have a Lat and Long
    dataBase <- AddLocationsToDataList(dataBase, requestList()) 
    # creating dataframe for all entries with Lat and Long
    totalFrame <- MergeFrames(rLocationDF, coordDF)
    missingreacDF <- missingreacDF[, c(1,2,3,8, 9, 4, 5, 6, 7)]
    missingreacDF <- colorCoordiante(missingreacDF)
    missingDF(missingreacDF)
    dataBase(dataBase)
    caseFiles(totalFrame)
    return(caseFiles())
  })
  
  # filters the dots on the map based on what the user has selected and updates map
  dataFilter <- reactive({
    df <- caseFiles()
    if (nrow(df)>0) {
      df %>%
        filter(nod %in% input$caseDistress)
      
    } else {df}
  })
  
  # showing imported csv file before any data wrangle was done. 
  output$dataset <- renderDataTable({
    req(dataFileUpload())  # Ensure dataFile is not NULL
    dataFileUpload()  # Get the dataframe
  },options = list(pageLength = 10))
  
  output$ColumnInfo <- renderUI({
    HTML(paste("case: case number<br/>",
               "date: MM/DD/YYYY<br/>",
               "rlocation: relative location<br/>",
               "nod: nature of distress<br/>",
               "psol: primary solution<br/>",
               "ssol: secondary solution<br/>",
               "ict: initial call time<br/>"))
  })
  
  # Analysis---------------------------------------------------------------------------------------------
  setActiveButton <- function(btn) {
    if (btn == "assistancerendered") {
      activeButton("assistancerendered")
      shinyjs::addClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::removeClass(selector = "#general", class = "active-button")
      shinyjs::addClass(selector = "#general", class = "reserve-button")
      shinyjs::addClass(selector = "#page4", class = "reserve-button")
    } else if (btn == "general") {
      activeButton("general")
      shinyjs::addClass(selector = "#general", class = "active-button")
      shinyjs::removeClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::addClass(selector = "#assistancerendered", class = "reserve-button")
      shinyjs::addClass(selector = "#page4", class = "reserve-button")
    } else if (btn == "stats") {
      shinyjs::addClass(selector = "#stats", class = "active-button")
      shinyjs::addClass(selector = "#assistancerendered", class = "reserve-button")
      shinyjs::addClass(selector = "#general", class = "reserve-button")
      shinyjs::addClass(selector = "#page4", class = "reserve-button")
    } else if (btn == "page4") {
      shinyjs::addClass(selector = "#page4", class = "active-button")
      shinyjs::addClass(selector = "#assistancerendered", class = "reserve-button")
      shinyjs::addClass(selector = "#general", class = "reserve-button")
      shinyjs::addClass(selector = "#stats", class = "reserve-button")
    }
  }
  
  # Observe button clicks and update the plotToShow value
  observeEvent(input$assistancerendered, {
    setActiveButton("assistancerendered")
  })
  
  observeEvent(input$general, {
    setActiveButton("general")
  })
  
  output$sangraph <- renderPlotly({
    links <- sankeynet(caseFiles())
    nodes <- data.frame(name = c(as.character(links$nod), as.character(links$psol)) %>% unique())
    links$IDnod <- match(links$nod, nodes$name) - 1
    links$IDsol <- match(links$psol, nodes$name) - 1
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(
        label = nodes$name,
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = links$IDnod,
        target = links$IDsol,
        value = links$value
      )
    )
  })
  # # Render the overtime plot
  output$overTime <- renderPlotly({
    # Calculate the count of cases per day
    caseCount <- casePerDay(caseFiles())
    
    # Plot the count of cases per day over time
    plot <- ggplot(caseCount, aes(x = date, y = total)) +
      geom_col()+ theme_minimal()
    
    ggplotly(plot)
    
  })
  observeEvent(c(input$assistancerendered, input$general), {
    output$plot <- renderUI({
      if (!is.null(activeButton())) {
        if (activeButton() == "assistancerendered") {
          plotlyOutput("sangraph", width = "100%",  height = 650)
        } else if (activeButton() == "general") {
          plotlyOutput("overTime", width = "100%", height = 650)
        }
      } else {
        HTML("This page will allow you to look at some Case Stats for the year 2021-2022")
      }
    })
  })
  
  # EC-SAR Cases---------------------------------------------------------------------------------------------
  
  # output$selectDates <- renderPrint({input$dates})
  # 
  # # code to create the interactive map
  output$ECSARCases <- renderLeaflet({
    # When there is no loaded csv file, it shows a blank map
    if (is.null(dataFilter())) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -82.6,
                lat = 27.7,
                zoom = 9.5)
    } else {
      leaflet() %>%
        addCircleMarkers(
          data = dataFilter(),
          lat = ~ as.numeric(Latitude),
          lng = ~ as.numeric(Longitude),
          color = ~ nodcolors,
          fillOpacity = 1.0,
          radius = 2,
          
          # When the user clicks on a dot, it will give all the information about the case.
          # This information is the same that can be seen in dataTable tab.
          popup = paste(
            'Case Number: ', dataFilter()$case,'<br/>',
            'Date: ', dataFilter()$date,'<br/>',
            'Relative Location: ', dataFilter()$rlocation,'<br/>',
            'Nature of Distress: ', dataFilter()$nod,'<br/>',
            'Primary Solution: ', dataFilter()$psol,'<br/>',
            'Secondary Solution: ', dataFilter()$ssol,'<br/>',
            'Inital Call Time: ', dataFilter()$ict)
        ) %>%
        addTiles() %>%
        addScaleBar() %>%
        setView(lng = -82.6,
                lat = 27.7,
                zoom = 9.5)
    }
  })
  
  
  
  # GPS ---------------------------------------------------------------------------------------------
  
  # reactive values to allow for user input and changes of data
  # output$genLocPicker <- renderUI({
  #   if (!is.null(missingDF())) {
  #     pickerInput(
  #       inputId = 'genLoc',
  #       label = "Select Entry",
  #       choices = sort(unique(missingDF()$rlocation)),
  #       options = list(`actions-box` = TRUE),
  #       multiple = FALSE,
  #       selected = unique(missingDF()$rlocation)
  #     )
  #   }
  # })
  # 
  # rv_location <- reactiveValues(id = NULL, lat=NULL, lng=NULL)
  # 
  # output$uiCoord <- renderText({
  #   location_info <- reactiveValuesToList(rv_location)
  #   
  #   if (!all(is.null(unlist(location_info)))) {
  #     HTML(paste('latitude :', location_info$lat),'<br/>', 
  #          paste('longitude:', location_info$lng))
  #   } else {
  #     "Click on the map to see geological information"  # Adjusted this line to use single string indexing
  #   }
  # })
  # 
  # 
  # output$clickMap <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -82.6, lat = 27.7, zoom = 9.5)
  # })
  # 
  # # When any click happens, identify clicks on map and log new location info.
  # observeEvent(input$clickMap_click,{
  #   clickMap_info <- input$clickMap_click
  #   
  #   # Sets the Lat and long to the 3rd decimal place.
  #     
  #     rv_location$lat <- round(clickMap_info$lat,3)
  #     rv_location$lng <- round(clickMap_info$lng,3)
  #     
  #   })
  # 
  # # Add logging statements
  # observeEvent(input$saveButton, {
  #   print("Save button clicked")
  #   
  #   # Check if all required inputs are available
  #   req(input$genLoc[1])
  #   req(isolate(rv_location$lat))
  #   req(isolate(rv_location$lng))
  #   
  #   # Remove the row from the missingDF because we have already found its GPS position
  #   tempMissingDF <- filter(missingDF(), rlocation != input$genLoc[1])
  #   
  #   # Add the GPS position of the relative position to the dataBase
  #   newData <- data.frame(
  #     RLocation = isolate(input$genLoc[1]),
  #     Latitude = isolate(rv_location$lat), 
  #     Longitude = isolate(rv_location$lng)
  #   )
  #   dataBaseUpdated <- rbind(newData, dataBase())
  #   
  #   # Update the missingDF
  #   write.csv(tempMissingDF, file = 'missingDF.csv', row.names = FALSE)
  #   write.csv(dataBaseUpdated, 'RLocationLatLongs.csv', row.names = FALSE)
  #   missingDF(tempMissingDF)
  #   
  #   # Update the dataframe used in the ECCase plot
  #   updatedData <- dataFileUpdate()
  #   
  #   # Update the picker input choices
  #   updatePickerInput(session, 
  #                     'genLoc', 
  #                     choices = if (nrow(tempMissingDF) > 0)
  #                       sort(unique(tempMissingDF$rlocation)) else NULL
  #   )
  #   
  #   # Return the updated dataBase
  #   return(dataBaseUpdated)
  # })
}) # shinyServer
