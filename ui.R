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
    theme = bslib::bs_theme(bootswatch = "superhero"),
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
    ),
    tabPanel("Analysis",
             fluidRow(
                 box(width = 12, status = "info", title = "Correlation Matrix", solidHeader = TRUE, collapsible = TRUE,
                     plotOutput("overTime"))
               )
    ),
    tabPanel("Plotting Cases", 
                      sidebarLayout(
                        sidebarPanel(
                          # prettyRadioButtons("caseDistress", "Select Nature of Distress:", 
                          #                    choices=list("All Cases"="all", 
                          #                                 "Select Subset"="subset")
                          # ),
                          # conditionalPanel("input.caseDistress =='subset'", 
                          #                  selectInput("subsetCases", "Choose Case Type: ", choices=c('Aground', 'Bridge Jumper', 'Capsized', 'Collision/Allision', 'Disabled', 'Fire', 'Medical', 
                          #                                                                                           'PIW', 'No Distress (Good Intent / Hoax)', 'Outside SOPs', 'Overdue',
                          #                                                                                           'Taking on Water', 'Unknown Circumstance'), multiple=TRUE)
                          # )
                          pickerInput(
                            inputId = "caseDistress",
                            label = "Select Nature of Distress",
                            choices = c('Aground', 'Bridge Jumper', 'Capsized', 'Collision/Allision', 'Disabled', 'Fire', 'Medical',
                                        'PIW', 'No Distress (Good Intent / Hoax)', 'Outside SOPs', 'Overdue',
                                        'Taking on Water', 'Unknown Circumstance'),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE,
                            selected = NULL
                          )
                          ),
                        mainPanel(tabsetPanel(
                          id = "mainTabs",
                          type = "tabs",
                          tabPanel("CaseMap",
                                   value = "tabMap",
                                   leafletOutput('ECSARCases',height = 650))
                          
                        ))
                      )), # tabPanel Plotting Cases
             tabPanel("Setting GPS Position",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("genLocPicker"),

               #   # text of the lat and long of mouse click
                 textOutput('uiCoord'),

                 # suppose to save the relative location name, lat and long to the dataBase of RLocationLatLon
                 actionButton("saveButton", label = "Save Coordinates")
               ),

               #  Shows the map itself
               mainPanel(
                 leafletOutput("clickMap", height = 650))
             ))
  )
)