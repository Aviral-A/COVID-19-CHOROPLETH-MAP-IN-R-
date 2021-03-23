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
library(geojsonio)
library(rmapshaper)
library(gpclib)

##GET DATA FROM NEW YORK TIMES COVID-19 US DATA REPOSITORY
getCovidData <- function(){
  download.file(
    url = "https://github.com/nytimes/covid-19-data/archive/master.zip",
    destfile = "data/COVID-19-data-master.zip"
  )
  
  download.file(
    url = "https://public.opendatasoft.com/explore/dataset/us-county-boundaries/download/?format=geojson&timezone=America/Los_Angeles&lang=en",
    destfile = "data/us_counties_shapes.geojson"
  )
  #set datapath for specific file
  dataPath <- "covid-19-data-master/live/"
  
  #unzip download and get files
  unzip(
    zipfile = "data/COVID19-data-master.zip",
    file = paste0(dataPath, c("us-states.csv", "us-counties.csv")),
    exdir = "data",
    junkpaths = T
  )
  uscounties <- geojsonio::geojson_read("data/us_counties_shapes.geojson", what = "sp")
  uscounties$countyfp <- paste0(uscounties$statefp, uscounties$countyfp)
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
dataCases <- read_csv("data/us-counties.csv")
totalDeceased <- dataCases %>%select(deaths)
currentDate <- (dataCases %>%select(date)) %>% slice(1)

#merge spatial data and coronavirus data
cases_county <- dataCases %>%select(fips, cases, deaths)

counties_merged_cases <- geo_join(uscounties, cases_county, "countyfp", "fips")
counties_merged_cases <- subset(counties_merged_cases, !is.na(cases))

#create choropleth map 
popup_cases <- paste0(counties_merged_cases$name, "County total cases: ", as.character(counties_merged_cases$cases))
pal <- colorNumeric("viridis", NULL)
counties_merged_cases <- ms_simplify(counties_merged_cases)
leaflet() %>%
  addTiles() %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(stroke = FALSE, data = counties_merged_cases, 
              fillColor = ~pal(counties_merged_cases$cases), 
              fillOpacity = 1, 
              smoothFactor = 0.3,
              popup = ~popup_cases) %>%
  addLegend(pal = pal, 
            values = states_merged_cases$cases, 
            position = "bottomright", 
            title = "Coronavirus Cases")
