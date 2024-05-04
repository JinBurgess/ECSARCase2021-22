library(leaflet)
library(shiny)
library(shinyWidgets)
library(sp)
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
                zoom = 9.4)
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
    print("Save button clicked")
    
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
}) # shinyServer