# EC-SAR CASES UI
library(shiny)
library(shinyWidgets)
library(leaflet.extras)
library(shinydashboard)
library(leaflet)
library(sp)
library(rgeos)
library(readr)
# library(GDAL)
library(ggplot2)
library(plotly)
library(networkD3)

shinyUI(
  navbarPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    "EC-SAR Cases",
    tabPanel("Overview", 
             column(width = 10, offset = 1, htmlOutput("overview.content"))),# tabPanel Overview
    
    # Dataset -----------------------------------------------------------------------------------------------
    tabPanel("Dataset",
             fluidRow(
               column(offset = 1, width = 10,
                      div(
                        style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
                        dropdownButton(
                          div(style ="width: 300px;",
                              tags$p("Case Number: an 8 digit number where first 4 digits refers to the year, the next 2 is the month, and the last 2 is the case within the month."),
                              tags$p("ex. 20210711 : this case happened on July 2021, and it is the 11th case of the month."),
                              tags$p("Charlied: Refers to a case that was accepted by the team, but no assistance was rendered. When the Primary solution of a case is
                            'No Longer Needs Assistance` or 'Case Accepted (Agency Assist)`, then it is given this code."),
                              tags$p("Inital Call Time: written in military time and refers to the first time the case is called in.")
                          ),
                          circle = TRUE, status = "default",
                          icon = icon("circle-info"),
                          tooltip = tooltipOptions(title = "Get More Info")
                        )
                      ),
                      dataTableOutput("dataset")
               )
             )
    ),
    # Analysis -----------------------------------------------------------------------------------------------
    tabPanel("Analysis",
             shinyjs::useShinyjs(),
             fluidRow(
               tags$head(
                 tags$style(HTML("\
                 .active-button {color: #b23a48 !important; background-color: #b23a48  !important; border-color: #212d40 !important; }
                                 .reserve-button {color: #212d40  !important; border-color:  #212d40 !important; }"))
               ),
               # b23a48 redwood
               # 212d40 prussian blue, adcce7 non phot blue
               # 80ded9 tiffany blue
               # dff3e4 hondeydew
               # 2e1760 federal blue
               
               column(width = 3,
                      actionButton("assistancerendered", label = "Case Progression", icon("life-ring"), 
                                   color = "warning", style = "bordered", width = "100%", class = "reserve-button")
               ),
               column(width = 3,
                      actionButton("general", label = "Case Distribution", icon("chart-simple"), 
                                   color = "warning", style = "bordered", width = "100%", class = "reserve-button")
               ),
               column(width = 3,
                      actionButton("stats", label = "Response Readiness", icon("ship"), 
                                   color = "warning", style = "bordered", width = "100%", class = "reserve-button")
               ),
               column(width = 3,
                      actionButton("infopage", label = "Information", icon("circle-info"), 
                                   color = "warning", style = "bordered",  width = "100%", class = "reserve-button")
               )),
             
             fluidRow(uiOutput("plot", width = "100%", height = 650))
    ),
    # Plotting -----------------------------------------------------------------------------------------------
    tabPanel("Plotting Cases", 
             sidebarLayout(
               sidebarPanel(width = 3,
                            
                            style = 'height: 715px;',
                            dateRangeInput("dates", label = "Date range", start = ete1.startDate, end = ete2.endDate),
                            hr(),
                            fluidRow(column(3, verbatimTextOutput("dateSelector"))
                            ),
                            radioGroupButtons(inputId = "leadtoPicker", label = "Plot based on",
                                              choices = c("Nature of Distress", "Primary Solution"),
                                              justified = TRUE
                            ),
                            conditionalPanel(
                              condition = "input.leadtoPicker == 'Nature of Distress'",
                              pickerInput(
                                inputId = "caseDistress",
                                label = "Select Nature of Distress",
                                choices = nodselection,
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE,
                                selected = NULL
                              ),
                            ), # conditional Panel
                            
                            conditionalPanel(
                              condition = "input.leadtoPicker == 'Primary Solution'",
                              pickerInput(
                                inputId = "caseSolution",
                                label = "Select Primary Solution",
                                choices = psolselection,
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE,
                                selected = NULL
                              ), 
                            ), 
                            prettyRadioButtons(
                              inputId = "reasonFill",
                              label = "Color Observations by:", 
                              choices = c("Nature of Distress", "Primary Solution"),
                              inline = TRUE, 
                              status = "danger",
                              fill = TRUE
                            ),
                            conditionalPanel(
                              condition = "input.reasonFill == 'Nature of Distress'",
                              div(
                                style = "height: 300px; overflow-y: auto;",
                                htmlOutput("nodLegend")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.reasonFill == 'Primary Solution'",
                              div(
                                style = "height: 300px; overflow-y: auto;",
                                htmlOutput("psolLegend")
                              )
                            )),
               mainPanel(width = 9,
                         div(style = "height: 715px; border: 2px solid black; padding: 5px;",
                             conditionalPanel(condition = "input.leadtoPicker == 'Nature of Distress'",
                                              leafletOutput('ECSARCasesDistress', height = 700)),
                             conditionalPanel(condition = "input.leadtoPicker == 'Primary Solution'",
                                              leafletOutput('ECSARCasesSoltion', height = 700)))
                         
                         
               )
             )), # tabPanel Plotting Cases
    # Set GPS -----------------------------------------------------------------------------------------------
    tabPanel("Setting GPS Position",
             sidebarLayout(
               sidebarPanel(width = 3, 
                            uiOutput("genLocPicker"),
                            textOutput('uiLat'),  textOutput('uiLong'),
                            actionButton("savePoint", label = "Finalize Coordinates")
                            
               ),
               mainPanel(width = 9,
                         div(style = "height: 715; border: 2px solid black; padding: 5px;",
                             # div(style = "border: 2px solid black; padding: 5px;",
                             leafletOutput("clickMap", height = 700))
                         
               )
             )
    )
  )
)

