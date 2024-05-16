# EC-SAR CASES SERVER
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
library(sp)
library(shinyjs)
library(forcats)
library(rgeos)
library(readr)
# library(rgdal)
library(networkD3)

shinyServer(function(input, output, session) {
  # react values---------------------------------------------------------------------------------
  # caseFiles <- reactiveVal (totalFrame)
  # missingDF <- reactiveVal(missingreacDF)
  filtered_data_gen <- reactiveVal(NULL)
  caseCount <- reactiveVal(NULL)
  activeButton <- reactiveVal(NULL)
  Lat <- reactiveVal(NULL)
  Lng <- reactiveVal(NULL)
  
  # overview ---------------------------------------------------------------------------------------------
  output$overview.content <- renderUI({
    team_overview
  })
  # dataset ---------------------------------------------------------------------------------------------
  
  # determines if the user has uploaded any data into the system 
  dataFileUpload <- reactive({
    df <- caseFiles()%>%
      select("case", "date", "rlocation", "Latitude", "Longitude", "nod", "psol", "ssol", "ict")
    colnames(df) <- c("Case ID", "Date", "Relative Location", "Latitude", "Longitude", "Nature of Distress", 
                      "Primary Solution", "Secondary Solution", "Intial Call Time")
    return(df)
  }) # dataFileUpload
  
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
      shinyjs::removeClass(selector = "#stats", class = "active-button")
      shinyjs::removeClass(selector = "#general", class = "active-button")
      shinyjs::removeClass(selector = "#infopage", class = "active-button")
      
      shinyjs::addClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::addClass(selector = "#general", class = "reserve-button")
      shinyjs::addClass(selector = "#infopage", class = "reserve-button")
    } else if (btn == "general") {
      activeButton("general")
      shinyjs::removeClass(selector = "#stats", class = "active-button")
      shinyjs::removeClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::removeClass(selector = "#infopage", class = "active-button")
      
      shinyjs::addClass(selector = "#general", class = "active-button")
      shinyjs::removeClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::addClass(selector = "#assistancerendered", class = "reserve-button")
      shinyjs::addClass(selector = "#infopage", class = "reserve-button")
    } else if (btn == "stats") {
      activeButton("stats")
      shinyjs::removeClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::removeClass(selector = "#general", class = "active-button")
      shinyjs::removeClass(selector = "#infopage", class = "active-button")
      
      shinyjs::addClass(selector = "#stats", class = "active-button")
      shinyjs::addClass(selector = "#assistancerendered", class = "reserve-button")
      shinyjs::addClass(selector = "#general", class = "reserve-button")
      shinyjs::addClass(selector = "#infopage", class = "reserve-button")
    } else if (btn == "infopage") {
      activeButton("infopage")
      shinyjs::removeClass(selector = "#assistancerendered", class = "active-button")
      shinyjs::removeClass(selector = "#general", class = "active-button")
      shinyjs::removeClass(selector = "#stats", class = "active-button")
      
      shinyjs::addClass(selector = "#infopage", class = "active-button")
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
  
  observeEvent(input$stats, {
    setActiveButton("stats")
  })
  
  observeEvent(input$infopage, {
    setActiveButton("infopage")
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
    )%>%
      layout(title = "Case Progression from Nature of Distress to Solution")
  })
  
  observeEvent(c(input$ecolecrews,input$summercrew, input$statcaseRange), {
    
    selected_dates <- input$statcaseRange
    if (!is.null(input$ecolecrews) && input$ecolecrews %in% c("A1", "A2", "B1", "B2", "C1", "C2")){
      reduced <- crewdf(caseFiles(), input$ecolecrews, input$summercrew, selected_dates[1], selected_dates[2])
      
      CaseType <- caseType(reduced)
      CaseCharlied <- caseCharlied(reduced)
      
      active_sum <- sum(CaseCharlied$total[CaseCharlied$psol_lmp == "Active"], na.rm = TRUE)
      charlie_sum <- sum(CaseCharlied$total, na.rm = TRUE)
      
      
      output$donutCaseType <- renderPlotly({
        plot_ly(data = CaseType, type = "pie", labels = ~nod, values = ~total,
                textinfo = "label+percent", hole = 0.4,
                marker = list(colors = ~nodcolors)) %>%
          layout(title = "Types of Cases",
                 showlegend = FALSE)  # Remove the legend
      })
      
      output$donutCaseCharlied <- renderPlotly({
        plot_ly(data = CaseCharlied, type = "pie", labels = ~psol_lmp, values = ~total,
                textinfo = "label+percent", hole = 0.4,
                marker = list(colors = ~psolcolors)) %>%
          layout(title = "Odds of Rendering Assistance",
                 showlegend = FALSE)  # Remove the legend
      })
      
      output$propStats <- renderText({
        HTML(sprintf("<div style='text-align: center;'><span style='font-size: 20px;'>%s : %s</span>", nrow(reduced), totalObservations))
      })
      
      output$propStats2 <- renderText({
        HTML(sprintf("<div style='text-align: center;'><span style='font-size: 20px;'>%s : %s</span>", active_sum, charlie_sum))
      })
      # paste("For", input$ecolecrews, "you have the possibility of being on ", nrow(reduced),"cased out of" , totalObservations)})
    } else{
      output$propStats <- renderText({
        "Select Crew"})
    }
  })
  
  output$infopage.content <- renderUI({
    analysis_review
  })
  observeEvent(c(input$assistancerendered, input$general, input$stats, input$infopage), {
    output$plot <- renderUI({
      if (!is.null(activeButton())) {
        if (activeButton() == "assistancerendered") {
          fluidRow(
            tags$head(
              tags$style(
                HTML(".offset-box { margin-left: 50px; margin-top: 50px;}")
              )
            ),
            div(class = "offset-box", column(width = 11, plotlyOutput("sangraph", width = "100%",  height = 650)))
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
                                              selected = month_labels[c(1, 13)]
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
            ) # tags$div closing
          ) # fluidRow closing
        } else if (activeButton() == "stats"){
          fluidRow( 
            box(width = 2, 
                pickerInput(inputId = 'ecolecrews',
                            label = "Select General Crew",
                            choices = c("A1", "A2", "B1", "B2", "C1", "C2"),
                            options = list(`actions-box` = TRUE),
                            multiple = FALSE, 
                            selected = "A1")),
            box(width = 2,
                pickerInput(inputId = "summercrew",
                            label = "Select Summer Crew?", 
                            choices = c("port", "starboard", 'none'),
                            options = list(`actions-box` = TRUE),
                            multiple = FALSE, 
                            selected = "none")
            ),
            box(width = 7, 
                sliderTextInput(width = "100%", 
                                inputId = "statcaseRange",
                                label = "Choose Date Range:", 
                                force_edges = TRUE, grid = TRUE,
                                choices = alldates,
                                selected = alldates[c(1, 377)]
                )
            ),
            fluidRow(width =12,
                     column(width = 6, title = tags$div("Possible Cases During Your Rotation", style = "text-align: center;"), 
                            plotlyOutput('donutCaseType', height = 575)),
                     column(width = 5, title = tags$div("Possible Case Response", style = "text-align: center;"), 
                            plotlyOutput('donutCaseCharlied', height = 575)),
            ),
            fluidRow(width =12,
                     box(width = 6, title = tags$div("Minimum Possible Cases in Year", style = "text-align: center;"), htmlOutput('propStats')),
                     box(width = 5, title = tags$div("Probability of Assisting", style = "text-align: center;"), htmlOutput('propStats2'))
                     
            )
          ) # fluidRow closing
        } else if (activeButton() == "infopage"){
          fluidRow(width = 12, 
                   column(width = 10, offset = 1, htmlOutput("infopage.content")))
        }
      }
    })
  })
  
  
  
  # EC-SAR Cases---------------------------------------------------------------------------------------------
  output$nodLegend <- renderText({
    nod_legend
  })
  
  output$psolLegend <- renderText({
    psol_legend
  })
  # code to create the interactive map
  output$ECSARCasesDistress <- renderLeaflet({
    
    # When there is no loaded csv file, it shows a blank map
    if (is.null(dataFilter())) {
      leaflet() %>%
        addTiles() %>%
        addScaleBar()%>%
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
        addScaleBar()%>%
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
  
  output$uiLat <- renderText({
    if(is.null(missingDF())){
      "No cases have a cooresponding GPS position and can be seen in Plotting Case."  # Adjusted this line to use single string indexing
      
    }
    if (!is.null(rv_location$lat)) {
      HTML(paste('latitude :', rv_location$lat))
    } else {
      "Click on the map to get GPS Coordinates"  # Adjusted this line to use single string indexing
    }
  })
  
  output$uiLong <- renderText({
    if (!is.null(rv_location$lng)) {
      HTML(paste('Longitude :', rv_location$lng))
    }
  })
  
  output$clickMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addScaleBar()%>%
      setView(lng = -82.6, lat = 27.7, zoom = 9.5)
  })
  
  # When any click happens, identify clicks on map and log new location info.
  observeEvent(input$clickMap_click,{
    clickMap_info <- input$clickMap_click
    
    # Sets the Lat and long to the 3rd decimal place.
    
    rv_location$lat <- round(clickMap_info$lat,3)
    rv_location$lng <- round(clickMap_info$lng,3)
    
    leafletProxy("clickMap") %>%
      clearMarkers() %>%
      addMarkers(lng = isolate(rv_location$lng), lat = isolate(rv_location$lat), 
                 options = markerOptions(draggable = FALSE))
    
  })
  
  outputOptions(output, 'clickMap', suspendWhenHidden = FALSE)
  
  observe({
    input$clickMap_marker_dragend
    pos <- input$clickMap_marker_dragend
    if (!is.null(pos)) {
      rv_location$lat <- round(pos$lat, 3)
      rv_location$lng <- round(pos$lng, 3)
      leafletProxy("clickMap") %>%
        updateMarkers(lng = rv_location$lng, lat = rv_location$lat,  
                      options = markerOptions(draggable = FALSE))
    }
  })
  
  observe({
    if(is.null(missingDF())){
      disable("savePoint")
    } else{
      enable("savePoint")
      
      # Add logging statements
      observeEvent(input$savePoint, {
        
        # Check if all required inputs are available
        req(input$genLoc[1])
        req(isolate(rv_location$lat))
        req(isolate(rv_location$lng))
        
        # Remove the row from the missingDF because we have already found its GPS position
        tempMissingDF <- filter(missingDF(), rlocation != input$genLoc[1])
        newObservation <- filter(missingDF(), rlocation == input$genLoc[1])%>%
          mutate(Latitude = isolate(rv_location$lat),
                 Longitude = isolate(rv_location$lng))
        
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
        updatedDF <- rbind(caseFiles(), newObservation)%>%
          arrange(date)
        caseFiles(updatedDF)
        
        
        return(dataBaseUpdated)
      })
      
    }
  })
})