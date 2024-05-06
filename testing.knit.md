
```r
library(readr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(stringr)
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ purrr     1.0.2
## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(lubridate)

df <- read_csv("2021_22.csv", col_names = FALSE)
```

```
## Rows: 377 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (7): X1, X2, X3, X4, X5, X6, X7
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
colnames(df) =c("case", "date", "rlocation", "nod", "psol", "ssol", "ict")
```

```r
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
```


```r
requestList = NULL
sortedData <- dataValidation(df) # removes any entries that are not accepted bay Pro Staff/ are not denoted in case form
sortedData <- splitData(sortedData) # splitting data into entries with coordinates and those with only relative coordinates
coordDF <- data.frame(sortedData[1])#dataframe with coordinates
rLocationDF <- data.frame(sortedData[2]) # dataframe relative positions
coordDF <- prepCoordFrame(coordDF)# reformating dataframe columns to contain Lat & Long and be same as coordDF
dataBase = read_csv('RLocationLatLongs.csv', col_names = TRUE) #getting dataframe with saved Lat and Longs gained from click map
```

```
## Rows: 147 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): RLocation
## dbl (2): Latitude, Longitude
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
rLocationDF <- PopulateLocationData(dataBase, rLocationDF) 
missingDF <- FindMissingLocationData(rLocationDF) # separate entries that do not still have a Lat and Long
dataBase <- AddLocationsToDataList(dataBase, requestList) 
      # creating dataframe for all entries with Lat and Long
totalFrame <- MergeFrames(rLocationDF, coordDF)
missingDF <- missingDF[, c(1,2,3,8, 9, 4, 5, 6, 7)]
missingDF <- colorCoordiante(missingDF)
      # colnames(totalFrame) <- c("case", "date", "rlocation", "Latitude", "Longitude", "nod", "psol", "ssol", "ict", "psolcolor", 'nodcolor')
      # missingDF(missingDF)
```


```r
library(ggplot2)

caseCount <- totalFrame %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
  group_by(date) %>%
  summarise(total = n())
```


```r
caseCount%>%
  ggplot(aes(x = date, y = total))+ 
  geom_line() + #geom_point() +
  theme_minimal()
```

<img src="testing_files/figure-html/unnamed-chunk-5-1.png" width="672" />



```r
startdate <- as.Date("2021-07-01")
enddate <- as.Date("2022-08-01")
alldates <- seq(startdate, enddate, by = "day")
weekdays <- weekdays(alldates)
date_weekday_df <- data.frame(Date = alldates, Weekday = weekdays)
```


```r
mergeweek <- left_join(date_weekday_df, caseCount, by = c("Date" = "date"))
```

seeing distribution of cases based on weekday


```r
weekdaydist <- mergeweek%>%
  group_by(Weekday)%>%
  summarise(total = sum(total, na.rm = TRUE))
weekdaydist
```

```
## # A tibble: 7 × 2
##   Weekday   total
##   <chr>     <int>
## 1 Friday       42
## 2 Monday       36
## 3 Saturday     95
## 4 Sunday       90
## 5 Thursday     29
## 6 Tuesday      35
## 7 Wednesday    26
```


```r
library(forcats)
countPsol <- totalFrame%>%
  group_by(psol)%>%
  summarise(total = n())%>%
  arrange(desc(total))%>%
  head(5)
```



```r
library(stringr)
timeframe <- totalFrame%>%
  filter(!is.na(ict))%>%
  mutate(ict = substr(ict, start = 1, stop = 2))%>%
  group_by(ict)%>%
  summarise(total = n())%>%
  arrange(desc(total))%>%
  head(5)

timeframe
```

```
## # A tibble: 5 × 2
##   ict   total
##   <chr> <int>
## 1 16       37
## 2 14       35
## 3 17       32
## 4 15       31
## 5 13       29
```

```r
# completeness of ict times
334/353
```

```
## [1] 0.9461756
```


```r
library(waffle)
library(ggplot2)
library(dplyr)

totalFrame2 <- totalFrame%>%
  select(nod)%>%
  group_by(nod)%>%
  na.omit()%>%
  summarise(total = n())%>%
  arrange(desc(total))
  

waffle(totalFrame2, color = c("red", "orange", "yellow", "green", "blue", "violet", "brown", "black", "grey", "seagreen", "pink", "yellowgreen", "lightblue", "salmon", "slateblue"))
```

<img src="testing_files/figure-html/unnamed-chunk-12-1.png" width="672" />


```r
totalFrame2 <- totalFrame%>%
  select(psol)%>%
  group_by(psol)%>%
  na.omit()%>%
  summarise(total = n())%>%
  arrange(desc(total))
  

waffle(totalFrame2, color = c("red", "orange", "yellow", "green", "blue", "violet", "brown", "black", "grey50", "seagreen", "pink", "yellowgreen", "lightblue", "salmon", "slateblue", "goldenrod"))
```

<img src="testing_files/figure-html/unnamed-chunk-13-1.png" width="672" />


```r
library(networkD3)
library(dplyr)

san <- totalFrame %>%
  group_by(nod, psol) %>%
  summarise(value = n()) %>%
  mutate(psol = case_when(
    psol == "Medical" ~ "Medical Assistance",
    TRUE ~ as.character(psol)
  ))
```

```
## `summarise()` has grouped output by 'nod'. You can override using the `.groups`
## argument.
```

```r
san2 <- totalFrame%>%
  filter(!is.na(ssol))%>%
  group_by(psol, ssol)%>%
  summarise(value = n()) %>%
  mutate(ssol = case_when(
    ssol == "Medical" ~ "Medical Assistance",
    TRUE ~ as.character(ssol)
  ))%>%
  rename(nod = psol, psol = ssol)
```

```
## `summarise()` has grouped output by 'psol'. You can override using the
## `.groups` argument.
```

```r
test <- rbind(san, san2)
```


```r
nodes <- data.frame(name = c(as.character(test$nod), as.character(test$psol)) %>% unique())
# 
test$IDnod <- match(test$nod, nodes$name) - 1
test$IDpsol <- match(test$psol, nodes$name) -1

p <- sankeyNetwork(Links = test, Nodes = nodes,
              Source = "IDnod", Target = "IDpsol",
              Value = "value", NodeID = "name",
              sinksRight=FALSE)
```

```
## Links is a tbl_df. Converting to a plain data frame.
```

```r
p
```

```{=html}
<div class="sankeyNetwork html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-8cbebc1562676f1e64cb" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-8cbebc1562676f1e64cb">{"x":{"links":{"source":[0,0,0,0,0,0,1,1,1,2,2,2,2,3,3,4,4,4,4,4,4,4,4,5,5,5,6,7,7,7,7,8,9,9,10,10,10,10,10,10,11,11,11,11,11,12,12,13,13,13,13,14,14,15,15,15,15,16,16,17,18,18,18,18,19,19,20,21,21,21,21,21,7,7,22,22,22,23,23,24,25,25,26,26,26,26,27,27],"target":[15,17,18,20,22,27,15,28,25,15,22,23,27,15,25,15,17,19,20,21,22,24,26,15,29,19,25,15,30,22,26,15,15,26,15,17,22,25,26,27,15,28,24,25,27,15,16,15,17,25,26,17,27,31,17,32,22,26,27,26,17,32,26,27,17,26,18,17,20,32,26,27,26,27,16,22,26,16,26,20,31,21,31,32,26,27,17,22],"value":[8,2,15,4,4,4,2,1,2,8,2,3,1,4,1,20,4,43,4,18,4,1,86,4,1,1,1,9,2,1,1,5,1,1,7,2,1,2,3,1,39,1,1,2,1,12,5,2,1,1,1,2,1,2,1,3,2,4,1,2,2,1,4,1,9,6,1,2,1,1,8,1,1,1,1,1,2,1,1,1,1,1,1,2,1,1,1,1]},"nodes":{"name":["Aground","Bridge Jumper","Capsized","Collision/Allision","Disabled","Fire","Flare","Medical","No Distress (Good Intent / Hoax)","Outside SOPs","Overdue","PIW","Taking on Water","Unknown Circumstance","Weather","Charlied","Dewatering","Escort","Freed from Aground","Fuel/Oil Transfer","Information/Advice","Jump Start","Other","Parbuckling","Repaired","Search","Tow","Transport","In Water Rescue","Firefighting","Medical Assistance","Agency Assist (Case Accepted)","No Longer Needs Assistance"],"group":["Aground","Bridge Jumper","Capsized","Collision/Allision","Disabled","Fire","Flare","Medical","No Distress (Good Intent / Hoax)","Outside SOPs","Overdue","PIW","Taking on Water","Unknown Circumstance","Weather","Charlied","Dewatering","Escort","Freed from Aground","Fuel/Oil Transfer","Information/Advice","Jump Start","Other","Parbuckling","Repaired","Search","Tow","Transport","In Water Rescue","Firefighting","Medical Assistance","Agency Assist (Case Accepted)","No Longer Needs Assistance"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":7,"fontFamily":null,"nodeWidth":15,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":false}},"evals":[],"jsHooks":[]}</script>
```

