# YuHan Burgess
# Computer Science Capstone 2023

# This GUI enables the user to click on a map to get GPS coordinates in the form of decimal degrees.
# When the user saves the coordinates, it updates the the dataframe and enables more points to be sown on the caseMap. 

library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)
library(sp)
library(rgeos)
library(readr)
library(rgdal) # geospatial
library(ggplot2)

source("MainClickMap.R")

# UI interface
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    
    # allows the input of one relative location based off the dataframe
    pickerInput(
      inputId = 'genLoc',
      label = "Select Entry",
      choices = if (nrow(missingDF) > 0)
        sort(unique(missingDF$RLocation)) else NULL,
      options = list(`actions-box` = TRUE),
      multiple = FALSE,
      selected = if (nrow(missingDF) > 0)
        unique(missingDF$RLocation) else NULL
    ),
    
    # Is an input that is not changed from the user, but it was a method used and works to get the lat and long from 
    # a user click 

    
    # text of the lat and long of mouse click
    textOutput('ui_coord'),
    
    # suppose to save the relative location name, lat and long to the dataBase of RLocationLatLon
    actionButton("saveButton", label = "Save Coordinates"),
    
    # saves all clicks in session to the RLocationLatLong.csv
    actionButton("saveChanges", label = "Finalize Changes")
  ),
  
  #  Shows the map itself
  mainPanel(
    leafletOutput("map_pos", height = 650))
))


server = function(input, output, session) {
  # reactive values to allow for user input and changes of data
  reactMissingDF <- reactiveVal(missingDF)
  
  # related to reactive values of getting the lat and long of mouse clicks
  rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL)
  rv_text <- reactiveValues(click='Click on the map to see geological information.')
  
  # Updates the csv file that is being called when user has saved a GPS coordinate.
  # The list of the genLoc no longer has that entry as an option because it is now saved in a different data frame.
  
  # creating the base map that the user can click on 
  output$map_pos <- renderLeaflet({
    leaflet(missingDF) %>%
      addTiles() %>%
      setView(lng = -82.6, lat = 27.7, zoom = 9.5)
  })
  
  # creates the text of the lat and long that the user will see on the side bar 
  output$ui_coord <- renderText({
    location_info <- reactiveValuesToList(rv_location)
    
    if (!all(is.null(unlist(location_info)))) {
      HTML(paste('latitude :', location_info$lat),'<br/>', 
           paste('longitude:', location_info$lng))
    } else {
      "Click on the map to see geological information"  # Adjusted this line to use single string indexing
    }
  })
  
  # When any click happens, identify clicks on map and log new location info.
  observeEvent(input$map_pos_click,{
    map_pos_click_info <- input$map_pos_click
      rv_location$lat <- round(map_pos_click_info$lat,3)
      rv_location$lng <- round(map_pos_click_info$lng,3)
    })
  
  # when the saveButton is clicked, the variables of RLocation, lat and long will be appended to the dataBase which will 
  # be used to in the merge portion of the coordinate database and the database with only relative positions.
  dataBase <- reactiveVal(dataBase)
  
  observeEvent(input$saveButton, {
    
    # Removes the row from the missingDF because we have already found it's GPS position.
    tempMissingDF <- filter(reactMissingDF(), RLocation != input$genLoc[1])
    
    # Removes relative location from possible entries to be entered into the system. 
    reactMissingDF(tempMissingDF)
    updatePickerInput(session, 
                      'genLoc',
                      choices = if (nrow(tempMissingDF) > 0)
                        sort(unique(tempMissingDF$RLocation)) else NULL
                      
    )
    
    # Adds the GPS position of the relative position to the dataBase. 
    t = rbind(data.frame(RLocation = isolate(input$genLoc[1]),
                         Latitude = isolate(rv_location$lat), 
                         Longitude = isolate(rv_location$lng))
              ,dataBase())
    
    # updating the missingDF
    write.csv(tempMissingDF, file = 'C:/Users/User/Documents/CS Capstone/missingDF.csv', row.names = F)
    return(dataBase(t))
    
  })
  # saving changes fully to dataBase
  observeEvent(input$saveChanges, {
    write.csv(dataBase(),'C:/Users/User/Documents/CS Capstone/RLocationLatLongs.csv', row.names = F)
  })
}

shinyApp(ui = ui, server = server)
