# YuHan Burgess
# Computer Science Capstone 2023

# This GUI enables the user to upload a csv file and see where the cases are plotted within the Tampa Bay Area.
# Users are able to see characters of a given case. 

library(leaflet)
library(shiny)
library(shinyWidgets)
library(shinybusy)
library(sp)
library(rgeos)
library(readr)
library(rgdal)


source("Main.R")

# redundancy in for initial deployment of R shiny
totalFrame <- data.frame(NULL)

# creation on the user interface
ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle"),
  titlePanel("ECSAR Cases"),
  
  # Place where user can input csv file and change parameters
  sidebarLayout(
    sidebarPanel(
      
      # inputting csv file
      fileInput('caseFile', 'CaseFolder File',
                multiple = FALSE,
                accept = '.csv',
                buttonLabel = 'Browse',
                placeholder = 'No filed selected'),
      
      # User will be able to select cases based on their nature of distress
      # Created a list of possible nature of distress in the csv file. Using this method enables the map to be shown 
      # without initially having a database loaded into the system. 
      pickerInput(
        inputId = "caseDistress",
        label = "Select Nature of Distress",
        choices = c('Aground', 'Bridge Jumper', 'Capsized', 'Collision/Allision', 'Disabled', 'Fire', 'Medical', 
                    'PIW', 'No Distress (Good Intent / Hoax)', 'Outside SOPs', 'Overdue', 
                    'Taking on Water', 'Unknown Circumstance'),
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = NULL 
      ),
      width = 4
    ),
    
    # this area is where the map is shown as well as the csv file that is loaded in 
    mainPanel(tabsetPanel(
      id = "mainTabs",
      type = "tabs",
      tabPanel("caseFile",
               value = 'tableCaseFolder',
               tableOutput('outputCaseFolderTable')),
      tabPanel("CaseMap",
               value = "tabMap",
               leafletOutput('ECSARCases'))
      
    ),width = 8))
)


server <- function(input,output,session){
  
  # determines if the user has uploaded any data into the system 
  dataFile <- reactive({
    if (is.null(input$caseFile)) {
      return()
    }
    
    # Filters the csv file and creates a merged file that contains the updated coordinates for relative locations that 
    # can be mapped out. 
    else{
      
      # reads in file from the input and creates heading since initial input doesn't have headers
      fileSpec <- req(input$caseFile)
      caseFolderReactive <- read.csv(fileSpec$datapath, header = FALSE, sep =  ',')
      colnames(caseFolderReactive) = c("Case", "Date", "Relative Location", "Nature of Distress", 
                                       "Primary Solution", "Secondary Solution", "Initial Call Time")
      
      # conducts validation and checking of data frames
      sortedData <- dataValidation(caseFolderReactive) # removes any entries that are not accepted bay Pro Staff/ are not denoted in case form
      sortedData <- splitData(sortedData) # splitting data into entries with coordinates and those with only relative coordinates
      coordDF <- data.frame(sortedData[1])#dataframe with coordinates
      rLocationDF <- data.frame(sortedData[2]) # dataframe relative positions
      relLocationDF <- relLocValidation(rLocationDF) # normalizing measurements to ft and setting directions for some entries
      rLocationDF <-PrepGenFrame(relLocationDF) # reformating dataframe columns to contain Lat & Long and be same as dataBase
      coordDF <- prepCoordFrame(coordDF)# reformating dataframe columns to contain Lat & Long and be same as coordDF
      dataBase = read_csv('RLocationLatLongs.csv', col_names = TRUE) #getting dataframe with saved Lat and Longs gained from click map
      rLocationDF <- PopulateLocationData(dataBase, rLocationDF) 
      missingDF <- FindMissingLocationData(rLocationDF) # separate entries that do not still have a Lat and Long
      dataBase <- AddLocationsToDataList(dataBase, requestList) 
      
      # setting Lat and Long for entries with matching relative position and keeping those entries that contain Lat and Long
      updateDF <- PopulateLocationData(dataBase, 
                                       filter(rLocationDF, 
                                              (!is.na(rLocationDF$Latitude) & !is.na(rLocationDF$Longitude))))
      
      # creating dataframe for all entries with Lat and Long
      totalFrame <- MergeFrames(updateDF, coordDF)
      
      return(totalFrame)
    }
  })
  
  # filters the dots on the map based on what the user has selected and updates map
  dataFilter <- reactive({
    if (nrow(dataFile()) > 0) {
      dataFile() %>%
        filter(
          sfasf %in% input$caseDistress)
      
    } else {
      dataFile()
    }
  })
  
  
  # showing imported csv file before any data wrangle was done. 
  output$outputCaseFolderTable <- renderTable({
    if (is.null(input$caseFile)) {
      return()
    } else{
      fileSpec <- req(input$caseFile)
      caseFolderReactive <- read.csv(fileSpec$datapath, header = FALSE, sep =  ',')
      colnames(caseFolderReactive) = c("Case", "Date", "Relative Location", "Nature of Distress", 
                                       "Primary Solution", "Secondary Solution", "Initial Call Time")
      return(caseFolderReactive)
    }
  })
  
  
  # code to create the interactive map
  output$ECSARCases <- renderLeaflet({
    # When there is no loaded csv file, it shows a blank map
    if (is.null(dataFile())) {
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
          color = ~ Colors,
          fillOpacity = 1.0,
          radius = 2,
          
          # When the user clicks on a dot, it will give all the information about the case. 
          # This information is the same that can be seen in dataTable tab. 
          popup = paste(
            'Case Number: ', dataFilter()$Case,'<br/>',
            'Date: ', dataFilter()$Date,'<br/>',
            'Relative Location: ', dataFilter()$RLocation,'<br/>',
            'Nature of Distress: ', dataFilter()$sfasf,'<br/>',
            'Primary Solution: ', dataFilter()$PSolution,'<br/>',
            'Secondary Solution: ', dataFilter()$SSolution,'<br/>',
            'Inital Call Time: ', dataFilter()$ICTime)) %>%
        
        # This is legend for the primary solution of each case. 
        addLegend(position = 'bottomright',
                  colors = c('grey','blue','purple','red','sienna','black','cyan',
                             'slateblue','yellow','white','grey','deeppink',
                             'goldenrod','steelblue','violet','tomato','midnightblue'),
                  labels = c('Agency Assist (Case Accepted)', 'Dewatering','Escort','Firefighting',
                             'Freed from Aground','Fuel/Oil Transfer', 'In Water Rescue', 'Information/Advice',
                             'Jump Start', 'Medical', 'No Longer Needs Assistance', 'Other', 'Parbuckling', 
                             'Repaired','Search', 'Tow', 'Transport'),
                  opacity = .70)%>%
        addTiles() %>%
        addScaleBar() %>%
        setView(lng = -82.6,
                lat = 27.7,
                zoom = 9.4)
    }
    
  })
}

# Rendering of app
shinyApp(ui = ui, server = server)