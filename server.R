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

shinyServer(function(input, output, session) {
  caseFiles <- reactiveVal (NULL)
  missingDF <- reactiveVal(NULL)
  dataBase <- reactiveVal(NULL)
  requestList <- reactiveVal(NULL)
  
  ########################################### dataset ############################################
  
  # determines if the user has uploaded any data into the system 
  dataFileUpload <- reactive({
    if (is.null(input$caseFile)) {
      return(NULL)
    } else {
      # reads in file from the input and creates heading since initial input doesn't have headers
      fileSpec <- input$caseFile
      caseFolderReactive <- read.csv(fileSpec$datapath, header = FALSE, sep =  ',')
      colnames(caseFolderReactive) =  c("case", "date", "rlocation", "nod", "psol", "ssol", "ict")
      
      caseFolderReactive <- caseFolderReactive%>%
        mutate(psol = case_when(
          psol == "No Longer Needs Assistance " ~ "Charlied",
          psol == "Agency Assist (Case Accepted) " ~ "Charlied",
          # Add more cases as needed
          TRUE ~ psol  # Keep other values unchanged
        ))
      caseFiles(caseFolderReactive)
      return(caseFiles())  # Return the dataframe
    }
  }) # dataFileUpload
  
  # determines if the user has uploaded any data into the system 
  dataFileUpdate <- reactive({
    if (!is.null(input$caseFile)) {
    # Filters the csv file and creates a merged file that contains the updated coordinates for relative locations that 
    # can be mapped out. 
      # reads in file from the input and creates heading since initial input doesn't have headers
      fileSpec <- req(input$caseFile)
      caseFolderReactive <- read.csv(fileSpec$datapath, header = FALSE, sep =  ',')
      colnames(caseFolderReactive) = c("case", "date", "rlocation", "nod", "psol", "ssol", "ict")
      
      # conducts validation and checking of data frames
      sortedData <- dataValidation(caseFolderReactive) # removes any entries that are not accepted by Pro Staff/ are not denoted in case form
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
    }
      # return(list(totalFrame, missingDF))
  })
  
  # filters the dots on the map based on what the user has selected and updates map
  dataFilter <- reactive({
    df <- dataFileUpdate()
    if (nrow(df) > 0) {
      df %>%
        filter(nod %in% input$caseDistress)

    } else {df}
  })
  
  # showing imported csv file before any data wrangle was done. 
  output$dataset <- renderDataTable({
    req(dataFileUpload())  # Ensure dataFile is not NULL
    dataFileUpload()  # Get the dataframe
  })
  
  output$ColumnInfo <- renderUI({
    if (!is.null(input$caseFile)) {
      HTML(paste("case: case number<br/>",
                 "date: MM/DD/YYYY<br/>",
                 "rlocation: relative location<br/>",
                 "nod: nature of distress<br/>",
                 "psol: primary solution<br/>",
                 "ssol: secondary solution<br/>",
                 "ict: initial call time<br/>"))
    } else {
      NULL  # Return NULL if no CSV file is loaded
    }
  })
  
  # ########################################### Analysis ############################################
  output$overTime <- renderPlot({
    if (!is.null(caseFiles())) {

      # Calculate the count of cases per day
      caseCount <- casePerDay(caseFiles())

      # Plot the count of cases per day over time
      ggplot(caseCount, aes(x = date, y = total)) +
        geom_line() +
        geom_point() +
        theme_minimal()

        
    } else {
      text(0.5, 0.5, "Upload a case file to see the plot", cex = 1.2)
    }
  })
  
  output$box1 <- renderTable({
    req(caseFiles())
    Top5nod <- caseFiles() %>%
      group_by(nod) %>%
      summarise(total = n()) %>%
      arrange(desc(total)) %>%
      rename("NATURE OF DISTRESS" = nod, "TOTAL" = total)%>%
      head(5)
    
    Top5nod
  })
  
  output$box2 <- renderTable({
    req(caseFiles())
    Top5psol <- caseFiles() %>%
      group_by(psol) %>%
      summarise(total = n()) %>%
      arrange(desc(total)) %>%
      rename("PRIMARY SOLUTION" = psol, "TOTAL" = total)%>%
      head(5)
    
    
    Top5psol
  })
  
  output$box3 <- renderTable({
    req(caseFiles())
    
    timeframe <- caseFiles() %>%
      filter(!is.na(ict))%>%
      mutate(ict = substr(ict, start = 1, stop = 2))%>%
      mutate(ict = case_when(
        ict == "00" ~ "0000-00:59", ict == "01" ~ "0100-01:59", ict == "02" ~ "0200-02:59", ict == "03" ~ "0300-03:59",
        ict == "04" ~ "0400-04:59", ict == "05" ~ "0500-05:59", ict == "06" ~ "0600-06:59", ict == "07" ~ "0700-07:59",
        ict == "08" ~ "0800-08:59", ict == "09" ~ "0900-09:59", ict == "10" ~ "1000-10:59", ict == "11" ~ "1100-11:59",
        ict == "12" ~ "1200-12:59", ict == "13" ~ "1300-13:59", ict == "14" ~ "1400-14:59", ict == "15" ~ "1500-15:59",
        ict == "16" ~ "1600-16:59", ict == "17" ~ "1700-17:59", ict == "18" ~ "1800-18:59", ict == "19" ~ "1900-19:59",
        ict == "20" ~ "2000-20:59", ict == "21" ~ "2100-21:59", ict == "22" ~ "2200-22:59", ict == "23" ~ "2300-23:59",
        TRUE ~ "Unknown"
      )) %>%
      group_by(ict)%>%
      summarise(total = n())%>%
      arrange(desc(total))%>%
      rename("INITIAL CALL TIME" = ict, "TOTAL" = total)%>%
      head(5)
    
    timeframe
  })
  
  output$box4 <- renderTable({
    req(caseFiles())
    
    caseCount <- caseFiles() %>%
      mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
      group_by(date) %>%
      summarise(total = n())
    
    mergeweek <- left_join(date_weekday_df, caseCount, by = c("Date" = "date"))
    
    weekdaydist <- mergeweek%>%
      group_by(Weekday)%>%
      summarise(total = sum(total, na.rm = TRUE))%>%
      arrange(desc(total))%>%
      rename("WEEKDAY" = Weekday, "TOTAL" = total)
      
    weekdaydist
  })
  
  ################################ ECSAR Cases ########################################
  
  # code to create the interactive map
  output$ECSARCases <- renderLeaflet({
    # When there is no loaded csv file, it shows a blank map
    if (is.null(dataFileUpdate())) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -82.6,
                lat = 27.7,
                zoom = 9.5)
    }
    
    # Shows map of cases within the totalFrame
    else {
      leaflet() %>%
        addCircleMarkers(
          data = dataFilter(),
          lat = ~ (as.numeric(Latitude)),
          lng = ~ (as.numeric(Longitude)),
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
            'Inital Call Time: ', dataFilter()$ict)) %>%
        addTiles() %>%
        addScaleBar() %>%
        setView(lng = -82.6,
                lat = 27.7,
                zoom = 9.5)
    }
    
  })
  
  ################################ Setting GPS ########################################
  
  # reactive values to allow for user input and changes of data
  output$genLocPicker <- renderUI({
    if (!is.null(missingDF())) {
      pickerInput(
        inputId = 'genLoc',
        label = "Select Entry",
        choices = sort(unique(missingDF()$rlocation)),
        options = list(`actions-box` = TRUE),
        multiple = FALSE,
        selected = unique(missingDF()$rlocation)
      )
    }
  })
  
  rv_location <- reactiveValues(id = NULL, lat=NULL, lng=NULL)

  output$uiCoord <- renderText({
    location_info <- reactiveValuesToList(rv_location)
    
    if (!all(is.null(unlist(location_info)))) {
      HTML(paste('latitude :', location_info$lat),'<br/>', 
           paste('longitude:', location_info$lng))
    } else {
      "Click on the map to see geological information"  # Adjusted this line to use single string indexing
    }
  })
  
  
  output$clickMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -82.6, lat = 27.7, zoom = 9.5)
  })
  
  # When any click happens, identify clicks on map and log new location info.
  observeEvent(input$clickMap_click,{
    clickMap_info <- input$clickMap_click
    
    # Sets the Lat and long to the 3rd decimal place.
      
      rv_location$lat <- round(clickMap_info$lat,3)
      rv_location$lng <- round(clickMap_info$lng,3)
      
    })
  
  # Add logging statements
  observeEvent(input$saveButton, {
    # Check if all required inputs are available
    req(input$genLoc[1])
    req(isolate(rv_location$lat))
    req(isolate(rv_location$lng))
    
    # Remove the row from the missingDF because we have already found its GPS position
    tempMissingDF <- filter(missingDF(), rlocation != input$genLoc[1])
    
    # Add the GPS position of the relative position to the dataBase
    newData <- data.frame(
      RLocation = isolate(input$genLoc[1]),
      Latitude = isolate(rv_location$lat), 
      Longitude = isolate(rv_location$lng)
    )
    dataBaseUpdated <- rbind(newData, dataBase())
    
    # Update the missingDF
    write.csv(tempMissingDF, file = 'missingDF.csv', row.names = FALSE)
    write.csv(dataBaseUpdated, 'RLocationLatLongs.csv', row.names = FALSE)
    missingDF(tempMissingDF)
    
    # Update the dataframe used in the ECCase plot
    updatedData <- dataFileUpdate()
    
    # Update the picker input choices
    updatePickerInput(session, 
                      'genLoc', 
                      choices = if (nrow(tempMissingDF) > 0)
                        sort(unique(tempMissingDF$rlocation)) else NULL
    )
    
    # Return the updated dataBase
    return(dataBaseUpdated)
  })
  
  # Testing map to draw polygons
  output$polygonMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -82.6, lat = 27.7, zoom = 9.5) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) 
  })
  
  observeEvent(input$map_draw_new_feature, {
    drawnFeatures <- input$map_draw_new_feature
    drawnFeatureType <- drawnFeatures$type
    if (drawnFeatureType == "polygon") {
      drawnPolygon <- leaflet:::getGeoJSON(drawnFeatures)
      polygonCoords <- leaflet:::getLatLngs(drawnPolygon)
    }
  })
}) # shinyServer
