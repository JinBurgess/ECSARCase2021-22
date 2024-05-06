library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(sp)
library(rgeos)
library(readr)
library(rgdal)
library(ggplot2)
library(plotly)

shinyUI(
  navbarPage(
    theme = bslib::bs_theme(bootswatch = "superhero"), # end theme
    "EC-SAR Cases",
    tabPanel("Overview", htmlOutput("overview.content")), # tabPanel Overview
    tabPanel("Dataset",
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   fileInput('caseFile', 'Upload Case Folder', multiple = FALSE, accept = '.csv',
                             buttonLabel = 'Browse', placeholder = '2021_22.csv'),
                   uiOutput("ColumnInfo")
                 ), #sidebarPanel
                 
                 mainPanel(
                   dataTableOutput("dataset")
                 ) # mainPanel regress
               ) # sidebarPanel
             ) # fluidPage
    ), # tabPanel Dataset
    tabPanel("Analysis",
               fluidRow(
                 box(width = 12, status = "info", title = "Cases by Day", solidHeader = TRUE, collapsible = TRUE,
                     plotOutput("overTime"))
               ), # 
               fluidRow(
                 box(width=3, title = "Top 5 Nature of Distress", solidHeader = TRUE, collapsible = TRUE, tableOutput("box1")),
                 box(width=3, title = "Top 5 Method of Assisting", solidHeader = TRUE, collapsible = TRUE, tableOutput("box2")),
                 box(width=3, title = "Top 5 High Traffic Times", solidHeader = TRUE, collapsible = TRUE, tableOutput("box3")),
                 box(width=3, title = "Case Frequency by Weekday ", solidHeader = TRUE, collapsible = TRUE, tableOutput("box4"))
               ) # fluidRow
    ),
    tabPanel("Plotting Cases", 
             sidebarLayout(
               sidebarPanel(
                 pickerInput(
                   inputId = "caseDistress", label = "Select Nature of Distress",
                   choices = c('Aground', 'Bridge Jumper', 'Capsized', 'Collision/Allision', 'Disabled', 'Fire', 'Medical',
                               'PIW', 'No Distress (Good Intent / Hoax)', 'Outside SOPs', 'Overdue',
                               'Taking on Water', 'Unknown Circumstance'),
                   options = list(`actions-box` = TRUE), multiple = TRUE, selected = NULL)
                 ),
               mainPanel(
                 tabsetPanel(
                   id = "mainTabs", type = "tabs",
                   tabPanel("CaseMap", value = "tabMap", leafletOutput('ECSARCases',height = 650))
                   ))
               )), # tabPanel Plotting Cases
    tabPanel("Setting GPS Position",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("genLocPicker"),
                 
                 # text of the lat and long of mouse click
                 textOutput('uiCoord'),

                 textOutput('uiLat'),
                 textOutput('uiLong'),
                 # suppose to save the relative location name, lat and long to the dataBase of RLocationLatLon
                 actionButton("saveButton", label = "Finalize Coordinates")
               ),

               #  Shows the map itself
               mainPanel(
                 tabsetPanel(
                   id = "selectTab", type = "tabs",
                   tabPanel("Select Certain Points", value = "tabMap", leafletOutput("clickMap", height = 650)),
                   tabPanel("Create Area", value = "tabMap", leafletOutput("polygonMap", height = 650))
                 ))
             ))
  )
)