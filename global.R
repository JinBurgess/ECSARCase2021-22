# EC-SAR CASES GLOBAL
library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(networkD3)

caseFiles <- reactiveVal (NULL)
missingDF <- reactiveVal(NULL)
dataBase <- reactiveVal(NULL)


df <- read_csv("2021_22.csv", col_names = FALSE)
colnames(df) <- c("case", "date", "rlocation", "nod", "psol", "ssol", "ict")

nodselection <-df %>%
  filter(!is.na(nod)) %>%
  distinct(nod)

psolselection <- df %>%
  filter(!is.na(psol)) %>%
  distinct(psol)



assign_rotation <- function(df, dateDF, startDate, startingcrew, crew) {
  
  # Calculate number of days between the start of the crew's duty to the offest of the start of the 2021-2022 case year
  days_to_start <- as.numeric(dateDF[1] - startDate)
  
  # Adjust the start index based on the number of days to start
  startingcrew <- startingcrew - (days_to_start %% length(etecrews))
  
  # Add crews column starting from the calculated index
  df$Crew <- crew[(startingcrew:(startingcrew + nrow(df) - 1)) %% length(crew) + 1]
  return(df)
}


crewdf <- function(df, ecolecrew, etecrew, startdate, enddate){
  df <- df%>%
    filter(date>= as.Date(startdate) & date <= as.Date(enddate))
  
  if(etecrew == 'none'){
    df <- df%>%
      filter(Crew == ecolecrew)
  }else{
    df <- df%>%
      filter(Crew == ecolecrew | Crew == etecrew)
  }
  
  return(df)
}

caseType <- function(df){
  df<- df%>%
    group_by(nod, nodcolors)%>%
    summarise(total = n())
  
  return(df)  
}

caseCharlied <- function(df){
  df<- df%>%
    mutate(psol_lmp = ifelse(psol != "Charlied", "Active", psol)) %>%
    filter(!is.na(psol_lmp)) %>%
    group_by(psol_lmp, psolcolors) %>%
    summarise(total = n())%>%
    mutate(psolcolors = ifelse(psol_lmp == "Active", "lightblue", psolcolors)) 
  
  return(df)  
}

df <- df %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

ete1.startDate<- min(df$date)
ete1.endDate <- as.Date("2021-08-15")
ete2.startDate<- as.Date("2022-05-20")
ete2.endDate <- max(df$date)
months <- seq(ete1.startDate, ete2.endDate, by = "month")
month_labels <- format(months, "%Y-%m")

alldates <- seq(ete1.startDate, ete2.endDate, by = "day")

ecolecrews <- c("A1","B1","C1", "A2","B2", "C2")
etecrews <- c("port", "starboard")

weekdays <- weekdays(alldates)
date_weekday_df <- data.frame(date = alldates, Weekday = weekdays)

ecoleRotation <- date_weekday_df%>%
  filter(date >= ete1.endDate & date <= ete2.startDate)

eteRotation1 <- date_weekday_df%>%
  filter(date < ete1.endDate) 

eteRotation2 <- date_weekday_df%>%
  filter(date > ete2.startDate)

start_index_ecole <- match("C2", ecolecrews)
start_index_ete <- match("starboard", etecrews)

eteRotation1 <- assign_rotation(eteRotation1, alldates, ete1.startDate, start_index_ete, etecrews)
eteRotation2 <- assign_rotation(eteRotation2, alldates, ete2.startDate,start_index_ete, etecrews)
ecoleRotation <- assign_rotation(ecoleRotation, alldates, ete1.endDate, start_index_ecole, ecolecrews)
fullrotation <- rbind(eteRotation1, ecoleRotation, eteRotation2)

# Plotting ---------------------------------------------------------------------------------------------
casePerDay <-function(df){
  df <- df %>%
    # mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
    group_by(date) %>%
    summarise(total = n())
  
  return(df)
}

box1 <- function(df){
  if (nrow(df)>0){
    Top5nod <- df %>%
      group_by(nod) %>%
      summarise(total = n()) %>%
      arrange(desc(total)) %>%
      rename("NATURE OF DISTRESS" = nod, "TOTAL" = total)
    
    if (nrow(df)>5){
      Top5nod <- head(Top5nod, 5)
    }
    return(Top5nod)
  }
}

box2 <- function(df){
  if (nrow(df)>0){
    Top5psol <- df%>%
      group_by(psol) %>%
      summarise(total = n()) %>%
      arrange(desc(total)) %>%
      rename("PRIMARY SOLUTION" = psol, "TOTAL" = total)
    if (nrow(df)>5){
      Top5psol <- head(Top5psol, 5)
    }
    return(Top5psol)
  }
}


box3 <- function(df){
  if (nrow(df)>0){
    Top5Times <- df%>%
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
      rename("INITIAL CALL TIME" = ict, "TOTAL" = total)
    
    if (nrow(Top5Times) > 5) {
      Top5Times <- head(Top5Times,5)
    }
    
    return(Top5Times)
  }
}

box4 <- function(df){
  caseCount <- df%>%
    group_by(date) %>%
    summarise(total = n())
  
  mergeweek <- left_join(date_weekday_df, caseCount, by =  "date")
  
  weekdaydist <- mergeweek%>%
    group_by(Weekday)%>%
    summarise(total = sum(total, na.rm = TRUE))%>%
    arrange(desc(total))%>%
    rename("WEEKDAY" = Weekday, "TOTAL" = total)
  
  return(weekdaydist)
}

sankeynet <- function(df) {
  san <- df %>%
    select(nod, psol)%>%
    filter(!is.na(nod) & !is.na(psol))%>%
    group_by(nod, psol) %>%
    summarise(value = n()) %>%
    mutate(psol = case_when(
      psol == "Medical" ~ "Medical Assistance",
      TRUE ~ as.character(psol)
    ))
  
  san2 <- df%>%
    select(psol, ssol)%>%
    filter(!is.na(ssol))%>%
    group_by(psol, ssol)%>%
    summarise(value = n()) %>%
    mutate(ssol = case_when(
      ssol == "Medical" ~ "Medical Assistance",
      ssol == "Other" ~ "Other Part 2",
      ssol == "Tow" ~ "Second Tow",
      TRUE ~ as.character(ssol)
    ))%>%
    rename(nod = psol, psol = ssol)
  
  links <- as.data.frame(rbind(san, san2))
  
  # write.csv(links, file = 'sankey.csv', row.names = FALSE)
  
  return(links)
}

# vdata munging --------------------------------------------------

dataValidation <- function(df) {
  
  # Creating data base with only cases that are accepted based on case numbering format 
  dff <- df %>%
    subset(psol != "Declined by SC") %>%
    filter(!is.na(nod)) %>%
    filter(str_detect(case, "20")) %>%
    filter(nchar(case, allowNA = FALSE) == 8) %>%
    mutate(psol = case_when(
      psol == 'Agency Assist (Case Accepted)' ~ "Charlied",
      psol == 'No Longer Needs Assistance' ~ "Charlied",
      TRUE ~ psol
    ))
  
  # Finding cases that start with the numbering format but doesn't meet the 8 character length. 
  # Hard code fix to find actual cases that do not meed the criteria of case numbering format
  entryCheck <- filter(df, nchar(case, allowNA = FALSE) != 8)%>%
    filter(str_detect(case, "20"),)%>%
    filter(!is.na(ict))
  
  # Add the cases that may have been sifted out back into the case dataframe
  combined_df <- rbind(dff,entryCheck)
  return(combined_df)
}

splitData <- function(df) { 
  coordFrame <- df %>%
    filter(substr(rlocation, 1, 2) == "27")
  
  generalFrame <- df %>%
    filter(substr(rlocation, 1, 2) != "27")
  return(list(coordFrame, generalFrame))
}

prepCoordFrame <- function(coordFrame){
  #make lat and long rows
  coordFrame = add_column(coordFrame, Latitude = NA, Longitude = NA, .after = "rlocation")
  
  #27 49.49 N -82 49.49 W --> 27.4949 82.4949
  #iterate row by row
  for (row in 1:nrow(coordFrame)) {
    #if it is already in decimal then leave it, but otherwise it is fine
    splitRow = str_split(coordFrame[row, 3], " ")
    
    if (length(splitRow[[1]]) == 2){#we are already properly formatted
      coordFrame[row,4] = splitRow[[1]][1]#lat
      coordFrame[row,5] = splitRow[[1]][2]#long
    }else{#need to convert how it is formatted
      #deal with lat first
      multipliedMinutes = as.double(splitRow[[1]][2]) / 60 #do math ad make char again
      coordFrame[row,4] = as.character(as.double(splitRow[[1]][1]) + multipliedMinutes)#add multiplied minutes to 27 and put it into a string
      
      #deal with long
      multipliedMinutes = as.double(splitRow[[1]][5]) / 60#do math ad make char again
      degrees = sub(".", "", splitRow[[1]][4])
      degrees = as.double(paste("-", degrees, sep = ""))
      #subtract instead of add because we are dealing with a negative
      coordFrame[row,5] = as.character(degrees-multipliedMinutes)#put it all back together and in long
    }
  }
  
  return(coordFrame)
}

PopulateLocationData = function(dataList, genFrame){
  df <- genFrame %>%
    mutate(rlocation = tolower(rlocation))%>%
    left_join(dataList, by = c("rlocation" = "RLocation"))
  
  df <- df[, c(1,2,3,8,9,4,5,6,7)]
  return(df)
}

FindMissingLocationData = function(df){
  returnFrame <- df %>%
    filter(is.na(Latitude))
  
  return(returnFrame)
}

AddLocationsToDataList = function(dataList, requestList){
  dataList$Latitude = as.character(dataList$Latitude)
  dataList$Longitude = as.character(dataList$Longitude)
  
  #iterate through requests
  new_data <- data.frame(
    RLocation = sapply(requestList, "[[", 1),
    Latitude = sapply(requestList, "[[", 2),
    Longitude = sapply(requestList, "[[", 3)
  )
  
  dataList <- bind_rows(dataList, new_data)
  
  #rewrite dataList with new data
  write.csv(dataList, "RLocationLatLongs.csv",row.names = FALSE)
  
  #return new dataList for use
  return(dataList)
}

abbreviate <- function(df) {
  # Lowercase everything
  df[, 5] <- tolower(df[, 5])
  
  # Define abbreviation rules
  abbreviation_rules <- list(
    "marker" = "mkr",
    "tierra verde" = "tv",
    "treasure island" = "ti",
    "pinellas north south" = "PNS",
    "pinellas east west" = "PEW",
    "maximo bridge" = "1st span",
    "dick misener" = "1st span",
    "boat ramp" = "BR",
    "yacht club" = "yc",
    "pass-a-grille" = "PAG",
    "coast guard" = "cg",
    "tv hi and dry" = "mkr",
    "search and rescue" = "SAR",
    "intracoastal waterway" = "ICW",
    "island" = "isl"
  )
  
  # Split location into words
  words <- strsplit(df[, 5], " ")
  
  # Replace words with abbreviations based on rules
  words <- lapply(words, function(x) {
    abbrev <- abbreviation_rules[x]
    ifelse(is.na(abbrev), x, abbrev)
  })
  
  # Collapse words back into strings
  abbreviated_locations <- sapply(words, paste, collapse = " ")
  
  # Update generalFrame with abbreviated locations
  df[, 5] <- abbreviated_locations
  
  return(df)
}

colorCoordiante <- function(df) {
  df["psolcolors"] <-  NA
  df["nodcolors"] <-  NA
  #assign colors based on the PSolution
  df <- df %>%
    mutate(psolcolors = case_when(
      psol == "Charlied" ~ "black", psol == "Dewatering" ~ "blue",
      psol == "Escort" ~ "purple", psol == "Firefighting" ~ "red",
      psol == "Freed from Aground" ~ "sienna", psol == "Fuel/Oil Transfer" ~ "darkgreen",
      psol == "In Water Rescue" ~ "cyan", psol == "Information/Advice" ~ "slateblue",
      psol == "Jump Start" ~ "yellow", psol == "Medical" ~ "mediumspringgreen",
      psol == "Other" ~ "deeppink", psol == "Parbuckling" ~ "goldenrod",
      psol == "Repaired" ~ "steelblue", psol == "Search" ~ "violet",
      psol == "Tow" ~ "tomato", psol == "Transport" ~ "midnightblue",
      TRUE ~ "grey"))%>%
    mutate(nodcolors = case_when(
      nod == "Disabled" ~ "darkgreen", nod == "Taking on Water" ~ "blue",
      nod == "Medical" ~ "mediumspringgreen", nod == "Fire" ~ "red",
      nod == "Aground" ~ "sienna", nod == "Bridge Jumper" ~ "black",
      nod == "PIW" ~ "cyan", nod == "Overdue" ~ "slateblue", 
      nod == "Unknown Circumstance" ~ "deeppink", nod == "Collision/Allision" ~ "goldenrod",
      nod == "Flare" ~ "steelblue", nod == "No Distress (Good Intent / Hoax)" ~ "violet",
      nod == "Outside SOPs" ~ "tomato", nod == "Capsized" ~ "midnightblue",
      TRUE ~ "grey"  # Default color for unaccounted PSolutions
    ))
  return(df)
  
}

MergeFrames = function(genFrame, coordFrame){
  #make a binded together frame
  comboFrame <-  rbind(genFrame,coordFrame)
  comboFrame <- colorCoordiante(comboFrame)
  
  return(comboFrame)
}

AddToRequestList = function(curRequestList, newRequest){
  #make curRequestList NULL if there isn't already one
  if (is.null(curRequestList)){
    curRequestList = list(newRequest)
  }
  else{
    curRequestList[[length(curRequestList)+1]] = newRequest
  }
  
  return(curRequestList)
}

# initalization -------------------------------------------------------
requestList = NULL
sortedData <- dataValidation(df) # removes any entries that are not accepted bay Pro Staff/ are not denoted in case form
totalObservations <- nrow(sortedData)
sortedData <- splitData(sortedData) # splitting data into entries with coordinates and those with only relative coordinates
coordDF <- data.frame(sortedData[1])#dataframe with coordinates
rLocationDF <- data.frame(sortedData[2]) # dataframe relative positions
coordDF <- prepCoordFrame(coordDF)# reformating dataframe columns to contain Lat & Long and be same as coordDF
coordDataBase = read_csv('RLocationLatLongs.csv', col_names = TRUE) #getting dataframe with saved Lat and Longs gained from click map
rLocationDF <- PopulateLocationData(coordDataBase, rLocationDF) 
missingreactDF <- FindMissingLocationData(rLocationDF) # separate entries that do not still have a Lat and Long
coordDataBase <- AddLocationsToDataList(coordDataBase, requestList) 
# creating dataframe for all entries with Lat and Long

rLocationGPS <- rLocationDF%>%
  filter(!is.na(Latitude))
missingreactDF <- colorCoordiante(missingreactDF)%>%
  mutate(dateMonth = format(date, "%Y-%m")) %>%
  left_join(fullrotation, by = "date")
totalFrame <- MergeFrames(rLocationGPS, coordDF)%>%
  mutate(dateMonth = format(date, "%Y-%m")) %>%
  left_join(fullrotation, by = "date")
missingDF(missingreactDF)
dataBase(coordDataBase)
caseFiles(totalFrame)

team_overview <- tagList(
  tags$p(),
  tags$p(),
  tags$h1("Eckerd College Search and Rescue Team (ECSAR)",  style = "text-align: center; color: #b23a48;'"),
  tags$hr(),
  tags$h2("Education Through Leadership", style = "text-align: center; color: #80ded9;"),
  tags$hr(),  # Horizontal line
  tags$p("Eckerd College Search and Rescue is a year-round, 24-hour, daily, volunteer,
        maritime aid organization almost exclusively staffed by undergraduate
        students. No cocurricular program like this one exists at any other educational
        institution in the U.S."),
  tags$h2("History", style = "text-align: center; color: #80ded9;"),
  tags$hr(),  # Horizontal line
  tags$p("The team was founded in 1971 in an effort to provide safety services for the college’s 
         watersports activities. In 1977, EC-SAR extended its rescue services to the Tampa Bay boating 
         community. EC-SAR received its first test of international proportions when the team was one 
         of the first rescue units to respond to the Skyway Bridge disaster in May of 1980. The program 
         has since grown to become one of the most respected search and rescue organizations on the west c
         oast of Florida."),
  tags$h2("Membership", style = "text-align: center; color: #80ded9;"),
  tags$hr(),  # Horizontal line
  tags$p("EC-SAR is a group of highly determined student volunteers who commit 10 to 15 hours
        per week to response, training and equipment upkeep. Students serve a 24-hour-duty
        rotation every three days, participate in weekly patrols, and monitor VHF radios and
        phones for incoming calls for assistance."),
  tags$p("High expectations elevate the commitment level of the students, creating camaraderie and 
        pride in performance that have earned the program its reputation of being a top-notch student development organization.
        Participation fosters lifelong friendships and instills a commitment to volunteerism
        that students carry forward after graduation."),
  
  tags$h2("Operations", style = "text-align: center; color: #80ded9;"),
  tags$hr(),  # Horizontal line
  tags$p("Working closely with the U.S. Coast Guard, 911 Emergency Medical Services, and other
        state and local agencies, EC-SAR provides 24-hour maritime assistance to the boaters
        of Tampa Bay and adjacent waterways. EC-SAR serves a 500-square-nautical-mile
        area of response, adding to the maritime safety of Pinellas, Hillsborough and Manatee
        counties—including 26 municipalities with resident populations totaling more than
        2.9 million."),
  
  tags$h3("Training"),
  tags$p("Student members of EC-SAR participate in a training program designed to develop
        proficiency in a variety of skills used in maritime search and rescue. Basic and advanced
        categories include:"),
  tags$ul(
    tags$li("Boating safety"),
    tags$li("Seamanship"),
    tags$li("Line work"),
    tags$li("Basic life support"),
    tags$li("In-water rescue"),
    tags$li("Emergency rescue techniques"),
    tags$li("Communications"),
    tags$li("Navigation"),
    tags$li("Boat handling"),
    tags$li("Search planning and patterns"),
    tags$li("Vessel and equipment maintenance"),
    tags$li("Maritime regulations")
  ),
  
  tags$h2("EC-SAR Facts", style = "text-align: center; color: #80ded9;"),
  tags$hr(),  # Horizontal line
  tags$h3("Current Fleet of Vessels"),
  tags$ul(
    tags$li("Rescue 4—2009 23-ft Century with Mercury 250-hpwr engine"),
    tags$li("Rescue 5—2016 24-ft Robalo with twin Yamaha 150-hpwr engines"),
    tags$li("Rescue 6—2004 26-ft Century with twin Yamaha 200-hpwr engines"),
    tags$li("Rescue 7—2022 24-ft Robalo with twin Yamaha 200-hpwr engines")
  ),
  tags$h3("Area Covered"),
  tags$p("Tampa and Boca Ciega bays (plus surrounding waters of southern Pinellas County)"),
  tags$p("Northern boundary—John’s Pass: N 27° 46.980’"),
  tags$p("Southern boundary—Longboat Pass: N 27° 26.655’"),
  tags$p("Approximately 10 miles offshore into the Gulf of Mexico"),
  
  tags$h2("Additional Resources", style = "text-align: center; color: #80ded9;"),
  tags$hr(),  # Horizontal line
  tags$p("If you are ever in need of assistance", style = "text-align: center;"),
  tags$li("Call 727.864.8256",style = "text-align: center;"),
  tags$li("Hail EC-SAR on VHF 16/68", style = "text-align: center;"),
  tags$p(),
  div(
    tags$h3("Links"),
    style = "text-align: center;",  # Apply text-align: center; to the container
    tags$a(href = "https://www.eckerd.edu/waterfront/ecsar/", "EC-SAR Home Page"),
    tags$p(),
    tags$a(href ="https://www.facebook.com/EckerdSAR/", "EC-SAR Facebook Page"),
    tags$p(),
    tags$a(href ="https://www.instagram.com/ecsar_ec/", "EC-SAR Instagram Page"),
    tags$p(),
    tags$a(href = "https://www.youtube.com/watch?v=s5y1WvBOgS8", "EC-SAR Video"),
    tags$p(),
    tags$hr(),
    tags$p()
  )
)

analysis_review <- tagList(
  tags$p(),
  tags$p(),
  tags$h1("Documentation", style = "text-align: center;"),
  tags$p(),
  tags$h3("Case Progression", style = "text-align: center;"),
  tags$p("EC-SAR cases are encoded in 14 unique values for nature of distress and 17 unique values for solutions. This flow make is 
         meant to create an interactive way of looking at the differenct case type and how they were ultimately resolved. When 
         a case's primary solution is set as `No Longer Needs Assistance` or `Case Accepted(Agency Assist)`, these categoies will be lumped 
         into the group called Charlied. Charlied refers to cases in which no assistance was rendered."),
  tags$hr(),
  tags$h3("Case Distribution", style = "text-align: center;"),
  tags$p("Eckered College Search and Rescue is a 24/7 volunteer response unit (execpt for 2.5 weeks in December for Winter Break). This window
  will allow th user to see the case occurrences over a given time frame. For the selected timeframe, individuals will be able to determine the 
  top 5 nature of distress, primary solutions, timeframe of case frequency, and total cases by day of the week. From this analysis, individuals 
  will can learn possible trends of cases over the year and develop assumptions about case possibilies."),
  tags$hr(),
  tags$h3("Response Readiness", style = "text-align: center;"),
  tags$p("This page looks at the minimum possible cases that a person is able to be called for give n their crew assignment during the school year
  and their crew assignment during the summer. EC-SAR a student volunteer oganization. Each person is placed in a duty rotation of 24hr on-call and 48hrs 
  off when school is in session.This means that you have to be ready to respond to a case within 5 minutes of being called. In the summer the on duty schedule is based on a two 
         crews that swtich off being on-call. The response time in the summer is 10 minutes. Along with the time investment put into being on-call as 
         well as EC-SAR training and academics, it is helpful to determine how likely you might be able to get a case. This anaylsis would be important for
         member ratability, training, and real world experience."),
  tags$p(),
  tags$hr(),
  tags$p()
)

psol_legend <- paste(
  '<div style="background-color: white; padding: 10px; border-radius: 5px;">',
  '<h4>Legend</h4>',
  '<div style="margin-bottom: 5px;"><span style="color: black;">■</span> Charlied</div>',
  '<div style="margin-bottom: 5px;"><span style="color: blue;">■</span> Dewatering</div>',
  '<div style="margin-bottom: 5px;"><span style="color: pruple;">■</span> Escort</div>',
  '<div style="margin-bottom: 5px;"><span style="color: red;">■</span> Firefighting</div>',
  '<div style="margin-bottom: 5px;"><span style="color: sienna;">■</span> Freed from Aground</div>',
  '<div style="margin-bottom: 5px;"><span style="color: darkgreen;">■</span> Fuel/Oil Transfer</div>',
  '<div style="margin-bottom: 5px;"><span style="color: cyan;">■</span> In Water Rescue</div>',
  '<div style="margin-bottom: 5px;"><span style="color: slateblue;">■</span> Information/Advice</div>',
  '<div style="margin-bottom: 5px;"><span style="color: yellow;">■</span> Jump Start</div>',
  '<div style="margin-bottom: 5px;"><span style="color: mediumspringgreen;">■</span> Medical</div>',
  '<div style="margin-bottom: 5px;"><span style="color: goldenrod;">■</span> Parbuckling</div>',
  '<div style="margin-bottom: 5px;"><span style="color: steelblue;">■</span> Repaired</div>',
  '<div style="margin-bottom: 5px;"><span style="color: violet;">■</span> Search</div>',
  '<div style="margin-bottom: 5px;"><span style="color: tomato;">■</span> Tow</div>',
  '<div style="margin-bottom: 5px;"><span style="color: midnightblue;">■</span> Transport</div>',
  '<div style="margin-bottom: 5px;"><span style="color: deeppink;">■</span> Other</div>',
  '</div>'
)

nod_legend <- paste(
  '<div style="background-color: white; padding: 10px; border-radius: 5px;">',
  '<h4>Legend</h4>',
  '<div style="margin-bottom: 5px;"><span style="color: sienna;">■</span> Aground</div>',
  '<div style="margin-bottom: 5px;"><span style="color: black;">■</span> Bridge Jumper</div>',
  '<div style="margin-bottom: 5px;"><span style="color: midnightblue;">■</span> Capsized</div>',
  '<div style="margin-bottom: 5px;"><span style="color: darkgreen;">■</span> Disabled</div>',
  '<div style="margin-bottom: 5px;"><span style="color: red;">■</span> Fire</div>',
  '<div style="margin-bottom: 5px;"><span style="color: steelblue;">■</span> Flare</div>',
  '<div style="margin-bottom: 5px;"><span style="color: mediumspringgreen;">■</span> Medical</div>',
  '<div style="margin-bottom: 5px;"><span style="color: violet;">■</span> No Distress (Good Intent / Hoax)</div>',
  '<div style="margin-bottom: 5px;"><span style="color: tomato;">■</span> Outside SOPs</div>',
  '<div style="margin-bottom: 5px;"><span style="color: slateblue;">■</span> Overdue</div>',
  '<div style="margin-bottom: 5px;"><span style="color: goldenrod;">■</span> Parbuckling</div>',
  '<div style="margin-bottom: 5px;"><span style="color: cyan;">■</span> PIW</div>',
  '<div style="margin-bottom: 5px;"><span style="color: blue;">■</span> Taking on Water</div>',
  '<div style="margin-bottom: 5px;"><span style="color: deeppink;">■</span> Other</div>',
  '</div>'
)
