# EC-SAR CASES SERVER
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
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
  filtered_data_gen <- reactiveVal(NULL)
  caseCount <- reactiveVal(NULL)
  activeButton <- reactiveVal(NULL)
  
  # dataset ---------------------------------------------------------------------------------------------
  
  # determines if the user has uploaded any data into the system 
  dataFileUpload <- reactive({
    df <- caseFiles()%>%
      select("case", "date", "rlocation", "Latitude", "Longitude", "nod", "psol", "ssol", "ict")
    colnames(df) <- c("Case ID", "Date", "Relative Location", "Latitude", "Longitude", "Nature of Distress", 
                      "Primary Solution", "Secondary Solution", "Intial Call Time")
    return(df)
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
        mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
        filter(nod %in% input$caseDistress)%>%
        filter(date >= input$dates[1])%>%
        filter(date <= input$dates[2])
    } else {df}
    
  })
  
  dataFilterSol <- reactive({
    df <- caseFiles()
    if (nrow(df)>0) {
      df %>%
        mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
        filter(psol %in% input$caseSolution)%>%
        filter(date >= input$dates[1])%>%
        filter(date <= input$dates[2])
    } else {df}
  })
  
  # showing imported csv file before any data wrangle was done. 
  output$dataset <- renderDataTable({
    req(dataFileUpload())  # Ensure dataFile is not NULL
    dataFileUpload()  # Get the dataframe
  },options = list(pageLength = 10))
  
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
  
  observeEvent(input$assistancerendered, {
    setActiveButton("assistancerendered")
  })
  
  observeEvent(c(input$general, input$datefilter), {
    setActiveButton("general")
    
    selected_dates <- input$datefilter
    if (!is.null(selected_dates)) {
      
      # Filter data based on selected dates
      reduced_gen <- caseFiles() %>%
        filter(dateMonth >= selected_dates[1] & dateMonth <= selected_dates[2])
      filtered_data_gen(reduced_gen)
      count <- casePerDay(reduced_gen)
      caseCount(count)
    } else {
      filtered_data_gen(caseFiles())
      count <- casePerDay(caseFiles())
      caseCount(count)
    }
    # Render the overtime plot
    output$overTime <- renderPlotly({
      # Plot the count of cases per day over time
      plot <- ggplot(caseCount(), aes(x = date, y = total)) +
        geom_col() +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme_minimal()
      
      ggplotly(plot)
    })
    
    output$topNOD <- renderTable({
      box1(filtered_data_gen())
      
    })
    output$topPSOL <- renderTable({
      box2(filtered_data_gen())
    })
    output$topTIME <- renderTable({
      box3(filtered_data_gen())
    })
    output$topDAY <- renderTable({
      box4(filtered_data_gen())
    })
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
  
  observeEvent(c(input$assistancerendered, input$general), {
    output$plot <- renderUI({
      if (!is.null(activeButton())) {
        if (activeButton() == "assistancerendered") {
          fluidRow(
            column(width = 11, plotlyOutput("sangraph", width = "100%",  height = 650))
          )
        } else if (activeButton() == "general") {
          fluidRow(
            tags$div(class = "GeneralPage",
                     fluidRow(
                       # Use offset to center the content
                       column(offset = 1, width = 10,
                              sliderTextInput(width = "100%", 
                                              inputId = "datefilter",
                                              label = "Choose a range:", 
                                              force_edges = TRUE, grid = TRUE,
                                              choices = month_labels,
                                              selected = month_labels[c(1, 13)] # Default selection (e.g., July 2021 and August 2022)
                              ))
                     ),
                     fluidRow(
                       column(offset = 1, width = 10,
                              plotlyOutput("overTime", width = "100%", height = 650)
                       )),
                     fluidRow(
                       column(offset = 2, width = 2, tableOutput("topNOD")),
                       column(width = 2, tableOutput("topPSOL")),
                       column(width = 2, tableOutput("topTIME")),
                       column(width = 2, tableOutput("topDAY"))
                     )
            ) # fluidRow
          )
        }
      } else {
        HTML("This page will allow you to look at some Case Stats for the year 2021-2022")
      }
    })
  })
  
  
  # EC-SAR Cases---------------------------------------------------------------------------------------------
  
  # code to create the interactive map
  output$ECSARCasesDistress <- renderLeaflet({
    # When there is no loaded csv file, it shows a blank map
    if (is.null(dataFilter())) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -82.6, lat = 27.7, zoom = 9.5)
    } else {
      if(input$reasonFill == "Primary Solution"){
        leaflet() %>%
          addCircleMarkers(
            data = dataFilter(),
            lat = ~ as.numeric(Latitude), lng = ~ as.numeric(Longitude),
            color = ~ psolcolors, fillOpacity = 1.0, radius = 2,
            
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
          setView(lng = -82.6, lat = 27.7, zoom = 9.5)
      } else {
        leaflet() %>%
          addCircleMarkers(
            data = dataFilter(),
            lat = ~ as.numeric(Latitude), lng = ~ as.numeric(Longitude),
            color = ~ nodcolors, fillOpacity = 1.0, radius = 2,
            
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
          setView(lng = -82.6, lat = 27.7, zoom = 9.5)
      }
    }
  })
  
  
  # code to create the interactive map
  output$ECSARCasesSoltion <- renderLeaflet({
    # When there is no loaded csv file, it shows a blank map
    if (is.null(dataFilterSol())) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -82.6, lat = 27.7, zoom = 9.5)
    } else {
      if(input$reasonFill == "Nature of Distress"){
        leaflet() %>%
          addCircleMarkers(
            data = dataFilterSol(),
            lat = ~ as.numeric(Latitude),  lng = ~ as.numeric(Longitude),
            color = ~ nodcolors, fillOpacity = 1.0, radius = 2,
            
            # When the user clicks on a dot, it will give all the information about the case.
            # This information is the same that can be seen in dataTable tab.
            popup = paste(
              'Case Number: ', dataFilterSol()$case,'<br/>',
              'Date: ', dataFilterSol()$date,'<br/>',
              'Relative Location: ', dataFilterSol()$rlocation,'<br/>',
              'Nature of Distress: ', dataFilterSol()$nod,'<br/>',
              'Primary Solution: ', dataFilterSol()$psol,'<br/>',
              'Secondary Solution: ', dataFilterSol()$ssol,'<br/>',
              'Inital Call Time: ', dataFilterSol()$ict)
          ) %>%
          addTiles() %>%
          addScaleBar() %>%
          setView(lng = -82.6, lat = 27.7, zoom = 9.5)
      } else {
        leaflet() %>%
          addCircleMarkers(
            data = dataFilterSol(),
            lat = ~ as.numeric(Latitude), lng = ~ as.numeric(Longitude),
            color = ~ psolcolors, fillOpacity = 1.0, radius = 2,
            
            # When the user clicks on a dot, it will give all the information about the case.
            # This information is the same that can be seen in dataTable tab.
            popup = paste(
              'Case Number: ', dataFilterSol()$case,'<br/>',
              'Date: ', dataFilterSol()$date,'<br/>',
              'Relative Location: ', dataFilterSol()$rlocation,'<br/>',
              'Nature of Distress: ', dataFilterSol()$nod,'<br/>',
              'Primary Solution: ', dataFilterSol()$psol,'<br/>',
              'Secondary Solution: ', dataFilterSol()$ssol,'<br/>',
              'Inital Call Time: ', dataFilterSol()$ict)
          ) %>%
          addTiles() %>%
          addScaleBar() %>%
          setView(lng = -82.6, lat = 27.7, zoom = 9.5)
      }
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
