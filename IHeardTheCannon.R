#-------------------------------
# Mapping EIS Solar Facilities
#
#  Using R Leaflet
#-------------------------------

#Call the libraries
#install.packages('RCurl')
#install.packages('geojson')
install.packages('kmlPolygons')
library(dplyr)
library(rgdal)
library(leaflet)
library(geojson)
library(geojsonio)
library(sp)
library (readxl)
library (data.table)
library(scales)
library(htmltools)
#
#Delete the Global Environment
rm(list=ls())
#
#Set the Working Directory
#setwd("P:/PROJECTS/Pollinators/R")
setwd("C:/Users/lwalston/Downloads")
getwd()
#
#---------------------------------------------------------
###1 Import the Neighborhoods from Github
#---------------------------------------------------------
#
dat1 <- readOGR("https://raw.githubusercontent.com/mapguyusa/MyStuff/master/Richmond%20Neighborhood%20-%20Flips%20-%202018.kml", "RVA Neighborhoods")
head(dat1@data)
plot(dat1)
#
# Read the csv that was manually created
#attributes <- read.csv("https://raw.githubusercontent.com/mapguyusa/MyStuff/master/RVA_Neighborhoods.csv", header=TRUE)
attributes <- read.table("https://raw.githubusercontent.com/mapguyusa/MyStuff/master/RVA_Neighborhoods.txt", header=TRUE, sep="\t")
head(attributes)

sapply(attributes, typeof) #Make sure variables types are correct
#attributes$name <- as.character(attributes$name)
#attributes$average <- as.double(attributes$Average)

#--------------------------------------------------------
#     APPEND attributes to the neighborhoods
#--------------------------------------------------------
dat1$name <- as.character(dat1$Name)
dat1$Average <- attributes$Average[match(dat1$Name, attributes$Name)]
dat1$Year5 <- attributes$Year5[match(dat1$Name, attributes$Name)]
dat1$Year20 <- attributes$Year20[match(dat1$Name, attributes$Name)]
dat1$Year20[is.na(dat1$Year20)]<- 0 #convert NA to 0
#
#View the head of the data attribute table
head(dat1@data)
sapply(dat1@data, typeof)
#
#--------------------------
#  LABELS
#--------------------------
dat1$y5label <- as.character(percent(dat1$Year5))
dat1$y20label <- as.character(percent(dat1$Year20))
dat1$symbol5 <- dat1$Year5*100
dat1$symbol20 <- dat1$Year20*100
#------------------------------------------------
# Creat Bins and Color Palettes
#------------------------------------------------
#
bins.Y5 <- c(0.0, 2.5, 7.5, 10.0, 13.0)
bins.Y20 <-c(0.0, 2, 4, 6, 8, 10)
#
#color palettes
pal.Y5 <- colorBin("Greens", domain=dat1$symbol5, bins=bins.Y5)
pal.Y20 <- colorBin("Reds", domain=dat1$symbol20, bins=bins.Y20)

#---------------------
#  Labels
#---------------------
labels <- sprintf(
  "<strong>%s</strong><br/>5 Year Appreciation: %s <br/>20 Year Appreciation: %s",
  dat1$name, dat1$y5label, dat1$y20label
) %>% lapply(htmltools::HTML)
#
#------------------
# Leaflet #1 - 5 Year
#-----------------
#
map1 <- leaflet(dat1)%>%
  #setView(-87, 35, 4)%>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addPolygons(group = "Year 5", fillColor = ~pal.Y5(dat1$symbol5), weight = 2, opacity = 1, color = "black", dashArray = "3", fillOpacity = 0.99,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto"))%>%
   #Add Legend
  addLegend(group = "5 Year", pal=pal.Y5, values=~symbol5, opacity=0.7, title="5 Year Appreciation (%)", position = "bottomleft")

map1
#------------------
# Leaflet #2 - 20 Year
#-----------------
#
map2 <- leaflet(dat1)%>%
  #setView(-87, 35, 4)%>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addPolygons(group = "Year 20", fillColor = ~pal.Y20(dat1$symbol20), weight = 2, opacity = 1, color = "black", dashArray = "3", fillOpacity = 0.99,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto"))%>%
  #Add Legend
  addLegend(group = "20 Year", pal=pal.Y20, values=~symbol20, opacity=0.7, title="20 Year Appreciation (%)", position = "bottomleft")

map2

#-----------------------
#  Leaflet #3 - Combined
#-----------------------

map3 <- leaflet(dat1)%>%
  #setView(-87, 35, 4)%>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  #add Y5 and Y20 polygons
  addPolygons(group = "5 Year Appreciation", fillColor = ~pal.Y5(dat1$symbol5), weight = 2, opacity = 1, color = "black", dashArray = "3", fillOpacity = 0.99,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto"))%>%
  addPolygons(group = "20 Year Appreciation", fillColor = ~pal.Y20(dat1$symbol20), weight = 2, opacity = 1, color = "black", dashArray = "3", fillOpacity = 0.99,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto"))%>%
  #Add Legend
  addLegend(group = "20 Year Appreciation", pal=pal.Y20, values=~symbol20, opacity=0.7, title="20 Year Appreciation (%)", position = "bottomright")%>%
  addLegend(group = "5 Year Appreciation", pal=pal.Y5, values=~symbol5, opacity=0.7, title="5 Year Appreciation (%)", position = "bottomright")%>%
  #Add Layers Control
  addLayersControl(baseGroups = c("5 Year Appreciation", "20 Year Appreciation"), options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))

map3