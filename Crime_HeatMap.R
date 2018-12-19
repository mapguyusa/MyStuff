#------------------
# Crime Heatmap
#  in Philadelphia
#------------------

library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(leaflet)
library (readxl)
library (data.table)
library(scales)
library(htmltools)
library(mapview)
library(leaflet.extras)
library(magrittr)

#Delete the Global Environment
rm(list=ls())

#Read the csv file
crimes <- read.csv(file="https://raw.githubusercontent.com/mapguyusa/MyStuff/master/Philadelphia_Crime_2015.csv")

sapply (crimes, typeof)

print(crimes)

#Specify a map with center at center of all crime coordinates
mean.lng <- mean(crimes$Longitude)
mean.lat <- mean(crimes$Latitude)

Logo <- "https://raw.githubusercontent.com/mapguyusa/MyStuff/master/TMG.png"

#------------------
# Leaflet 
#------------------
#
crime.map <- leaflet(crimes)%>%
  setView(-75, 40, 10)%>%
  addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addMouseCoordinates(style="basic")%>%
  addLogo(Logo, url="http://www.mapguyusa.com/p/portfolio.html", position = "topright", width=150, height=150)%>%
  addHeatmap(lng=~Longitude, lat=~Latitude, blur=8, max=0.001, radius=5)%>%
crime.map