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

dateLook<- df %>%
  select(date)%>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y"))

startDate<- min(dateLook$date)
endDate <- max(dateLook$date)
months <- seq(startDate, endDate, by = "month")
month_labels <- format(months, "%Y-%m")

alldates <- seq(startDate, endDate, by = "day")
weekdays <- weekdays(alldates)
date_weekday_df <- data.frame(Date = alldates, Weekday = weekdays)

# Plotting ---------------------------------------------------------------------------------------------
casePerDay <-function(df){
  df <- df %>%
    mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
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
  
  mergeweek <- left_join(date_weekday_df, caseCount, by = c("Date" = "date"))
  
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
      psol == "Charlied" ~ "green", psol == "Dewatering" ~ "blue",
      psol == "Escort" ~ "purple", psol == "Firefighting" ~ "red",
      psol == "Freed from Aground" ~ "sienna", psol == "Fuel/Oil Transfer" ~ "black",
      psol == "In Water Rescue" ~ "cyan", psol == "Information/Advice" ~ "slateblue",
      psol == "Jump Start" ~ "yellow", psol == "Medical" ~ "mediumspringgreen",
      psol == "Other" ~ "deeppink", psol == "Parbuckling" ~ "goldenrod",
      psol == "Repaired" ~ "steelblue", psol == "Search" ~ "violet",
      psol == "Tow" ~ "tomato", psol == "Transport" ~ "midnightblue",
      TRUE ~ "grey"))%>%
    mutate(nodcolors = case_when(
      nod == "Disabled" ~ "green", nod == "Dewatering" ~ "blue",
      nod == "Escort" ~ "purple", nod == "Fire" ~ "red",
      nod == "Aground" ~ "sienna", nod == "Bridge Jumper" ~ "black",
      nod == "Capsized" ~ "cyan", nod == "Overdue" ~ "slateblue",
      nod == "Medical" ~ "yellow",
      nod == "Unkown Circumstance" ~ "deeppink", nod == "Collision/Allision" ~ "goldenrod",
      nod == "Flare" ~ "steelblue", nod == "No Distress (Good Intent / Hoax)" ~ "violet",
      nod == "Outside SOPs" ~ "tomato", nod == "PIW" ~ "midnightblue",
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
totalFrame <- MergeFrames(rLocationGPS, coordDF)%>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         dateMonth = format(date, "%Y-%m")) 
missingreactDF <- colorCoordiante(missingreactDF)%>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         dateMonth = format(date, "%Y-%m")) 
missingDF(missingreactDF)
dataBase(coordDataBase)
caseFiles(totalFrame)

