library(tidyverse)
library(leaflet)
library(shiny)
library(plotly)
library(fs)
library(wbstats)
library(shinydashboard)
library(DT)
library(sf)
library(rgdal)
library(tigris)
library(acs)
library(stringr)


##GET DATA FROM NEW YORK TIMES COVID-19 US DATA REPOSITORY
getCovidData <- function(){
  download.file(
    url = "https://github.com/nytimes/covid-19-data/archive/master.zip",
    destfile = "data/COVID-19-data-master.zip"
  )

 #set datapath for specific file
  dataPath <- "covid-19-data-master/live/"

  #unzip download and get files
  unzip(
    zipfile = "data/COVID19-data-master.zip",
    file = paste0(dataPath, c("us-states.csv")),
    exdir = "data",
    junkpaths = T
)
    
  
}
#update covid data every numHours
update <- function(){
  numHours = 0.00001
  if(!dir_exists("data")){
    dir.create("data")
    getCovidData()
  }
  else if((!file.exists("data/COVID-19-data-master.zip")) || (as.double(Sys.time() - file_info("data/COVID-19-data-master.zip")$change_time, units = "hours") > numHours)){
    getCovidData()
  }
}
update()

#read in data from files
dataCases <- read_csv("data/us-states.csv")
totalDeceased <- dataCasesLive %>%select(deaths)
currentDate <- (dataCasesLive %>%select(date)) %>% slice(1)

#merge spatial data and coronavirus data
cases_state <- dataCasesLive %>%select(state, cases, deaths)
states_merged_cases <- geo_join(states, cases_state, "NAME", "state")
states_merged_cases <- subset(states_merged_cases, !is.na(cases))

#create choropleth map 
popup_cases <- paste0("Total Cases: ", as.character(states_merged_cases$cases))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_cases , 
              fillColor = ~pal(states_merged_cases$cases), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_cases) %>%
  addLegend(pal = pal, 
            values = states_merged_cases$cases, 
            position = "bottomright", 
            title = "CoronaVirus Cases")
