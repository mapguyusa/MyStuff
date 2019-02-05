#-------------------------------
# Interactive Map of Spending by State
#   Using Leaflet
#   January 2019
#-------------------------------
#
#Call the libraries
#install.packages('RCurl')
#install.packages('geojson')
#install.packages('geojsonio')
library(RCurl)
library(dplyr)
library(rgdal)
library(leaflet)
library(maps)
library(geojson)
library(geojsonio)
library (gdata)
library (readxl)
#
#Delete the Global Environment
rm(list=ls())
#
#Set the Working Directory
setwd("C:/Users/lwalston/Downloads")
getwd()
#setwd("P:/PROJECTS/Pollinators/R")
#
#---------------------------------------------------------
###1 Import the Spend Spreadsheet
#---------------------------------------------------------
#
#Read xlsx straight from working directory
SpendSheet <- read_xlsx("ggg.xlsx")
#
print(SpendSheet)
#
sapply (SpendSheet, typeof)
#
# Round Values to integer and Thousand Separator (e.g., 1,000)
#SolarStates$MW_Label <- format(as.integer(round((SolarStates$Solar_MW), 0)), big.mark = ",")
#SolarStates$AG_Label <- format(as.integer(round((SolarStates$TP_AG_AC), 0)), big.mark = ",")
##
#---------------------------------------------------------
# Import the states spatial polygons using geojsonio
# transfrom .json file into a spatial polygons data frame
#---------------------------------------------------------
states <- geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp")
#plot(states)
# check the class of the object
class(states)
names(states)
#
#--------------------------------------------------------
# Subset the polygons down to the lower 48
#      AND APPEND THE DATASETS
#--------------------------------------------------------
States48 <- subset(states, !(name %in% c('Alaska', 'District of Columbia', 'Hawaii', 'Puerto Rico')))
#
# Append the Spend Values from the Spreadsheet
States48$Spend <- SpendSheet$AVG_SPEND[match(States48$name, SpendSheet$STATE)]

#
#View the head of the data attribute table
head(States48@data)
#
#check variable types
is.numeric(States48$Spend) #should be true
#
#convert string to double (if needed)
#States48$Spend = as.double(States48$Spend)
#
#-----------------------------------------------------
#
#  NOW MAP IT OUT!
#
#-----------------------------------------------------
#create bins
bins.Spend <- c(150, 175, 200, 225, 250)
#
#color palettes
pal.Spend <- colorBin("Greens", domain=States48$Spend, bins=bins.Spend)

#
#Label Information
labels <- sprintf(
  "<font size = 4><und>%s</und></font><br/><strong>Average Spend: <font color = green>$%s</font></strong>",
  States48$name, States48$Spend
) %>% lapply(htmltools::HTML)
#
#Run the Leaflet
leaflet(States48)%>%
  #setView(-85.5, 35, 3)%>%
  setMaxBounds( lng1 = -85, lat1 = 36, lng2 = -90, lat2 = 46)%>%
  addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(minZoom = 4.5, maxZoom = 4.5))%>%
  addPolygons(group = "Average Spend", fillColor = ~pal.Spend(Spend), weight = 2, opacity = 1, color = "gray", dashArray = "3", fillOpacity = 0.7,
              highlight = highlightOptions(weight=5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto"))%>%
  #Add Legend
  addLegend(group = "Average Spend", pal=pal.Spend, values=~Spend, opacity=0.7, title="Average Spend ($)", position = "bottomleft")
  #Add Layers Control
  #addLayersControl(baseGroups = c("Agriculture", "Solar Development"), options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  







