library(readr)
library(dplyr)
library(stringr)
library(tidyverse)

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