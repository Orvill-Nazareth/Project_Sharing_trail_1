#packages----
library(sp)
library(readxl)
library(dplyr)
library(raster)
library(rgdal)
library(stringr)
library(leaflet)
library(magrittr)
#load the files as variables and load the boundary shapefile----
datasheet <- "C:/mydatasheet.xlsx"
excel_sheets(datasheet)
sheet_DS <- "sheet containing direct sighting data"
sheet_pellet <- "sheet containing pellet plots data"
sheet_h.dist_veg <- "sheet containing 400m interval data (human dist. and vegetation)"
shapefile_dir <- "path/of/shapefile/folder"
shapefile <- "name of shapefile"
bound <- readOGR(dsn = shapefile_dir, layer = shapefile) %>% 
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs"))
#name of the datafield in shapefile to be displayed in map----
#substitute the "column_name" with the column name that contains the string of text that you wish to display as the lable of the polygons in the shapefile
names(bound)
featurename <- ~`column_name`
#Transect lines----
df.transect <- read_excel(datasheet, sheet = sheet_DS) %>% 
  .[!duplicated(.$`Transect ID`),] %>% 
  data.frame(start_lat = .$`Start Latitude N Deg`+(.$`Start Latitude N Min`/60)+(.$`Start Latitude N Sec`/3600),
             start_long = .$`Start Longitude E Deg`+(.$`Start Longitude E Min`/60)+(.$`Start Longitude E Sec`/3600),
             end_lat = .$`End Latitude N Deg`+(.$`End Latitude N Min`/60)+(.$`End Latitude N Sec`/3600),
             end_long = .$`End Longitude E Deg`+(.$`End Longitude E Min`/60)+(.$`End Longitude E Sec`/3600),
             .,
             endlongdms = paste(.$`End Longitude E Deg`, "°", .$`End Longitude E Min`, "'", .$`End Longitude E Sec`, "''", sep = ""),
             endlatdms = paste(.$`End Latitude N Deg`, "°", .$`End Latitude N Min`, "'", .$`End Latitude N Sec`, "''", sep = ""),
             startlongdms = paste(.$`Start Longitude E Deg`, "°", .$`Start Longitude E Min`, "'", .$`Start Longitude E Sec`, "''", sep = ""),
             startlatdms = paste(.$`Start Latitude N Deg`, "°", .$`Start Latitude N Min`, "'", .$`Start Latitude N Sec`, "''", sep = ""))

transect <-  apply(df.transect,1,function(x){points <- data.frame(lng=as.numeric(c(x["start_long"], 
                                                                         x["end_long"])),
                                                        lat=as.numeric(c(x["start_lat"], 
                                                                         x["end_lat"])),stringsAsFactors = F)
                                   coordinates(points) <- c("lng","lat")
                                   Lines(Line(points),ID=x["Transect.ID"])})

row.names(df.transect) <- df.transect$`Transect.ID`

transect <- SpatialLinesDataFrame(SpatialLines(transect),df.transect)
#Species sightings----
df.sighting <- read_excel(datasheet, sheet = sheet_DS) %>% 
  .[!is.na(.$`Sl. No.`),] %>% 
  data.frame(latitude = .$`Latitude N Deg`+(.$`Latitude N Min`/60)+(.$`Latitude N Sec`/3600),
             longitude = .$`Longitude E Deg`+(.$`Longitude E Min`/60)+(.$`Longitude E Sec`/3600),
             latdms = paste(.$`Latitude N Deg`, "°", .$`Latitude N Min`, "'", .$`Latitude N Sec`, "''", sep = ""),
             longdms = paste(.$`Longitude E Deg`, "°", .$`Longitude E Min`, "'", .$`Longitude E Sec`, "''", sep = ""),
             .) %>% 
  SpatialPointsDataFrame(.[1:2],.)
#pellet----
df.pellet <- read_excel(datasheet, sheet = sheet_pellet) %>% 
  data.frame(start_lat = .$`Start GPS Location N Deg`+(.$`Start GPS Location N Min`/60)+(.$`Start GPS Location N Sec`/3600),
             start_long = .$`Start GPS Location E Deg`+(.$`Start GPS Location E Min`/60)+(.$`Start GPS Location E Sec`/3600),
             end_lat = .$`End GPS Location N Deg`+(.$`End GPS Location N Min`/60)+(.$`End GPS Location N Sec`/3600),
             end_long = .$ `End GPS Location E Deg`+(.$`End GPS Location E Min`/60)+(.$`End GPS Location E Sec`/3600),
             id = paste(.$`Transect ID`, .$`Plot No.`, sep = " "),
             .,
             endlongdms = paste(.$`End GPS Location E Deg`, "°", .$`End GPS Location E Min`, "'", .$`End GPS Location E Sec`, "''", sep = ""),
             endlatdms = paste(.$`End GPS Location N Deg`, "°", .$`End GPS Location N Min`, "'", .$`End GPS Location N Sec`, "''", sep = ""),
             startlongdms = paste(.$`Start GPS Location E Deg`, "°", .$`Start GPS Location E Min`, "'", .$`Start GPS Location E Sec`, "''", sep = ""),
             startlatdms = paste(.$`Start GPS Location N Deg`, "°", .$`Start GPS Location N Min`, "'", .$`Start GPS Location N Sec`, "''", sep = ""))

pellet <- apply(df.pellet,1,function(x){
  points <- data.frame(lng=as.numeric(c(x["start_long"], 
                                        x["end_long"])),
                       lat=as.numeric(c(x["start_lat"], 
                                        x["end_lat"])),stringsAsFactors = F)
  coordinates(points) <- c("lng","lat")
  Lines(Line(points),ID=x["id"])})
row.names(df.pellet) <- df.pellet$id
pellet <- SpatialLinesDataFrame(SpatialLines(pellet),df.pellet)
#400m interval points----
df.int <- read_excel(datasheet , sheet = sheet_h.dist_veg) %>% 
  data.frame(latitude = .$`Latitude N Deg`+(.$`Latitude N Min`/60)+(.$`Latitude N Sec`/3600),
             longitude = .$`Longitude E Deg`+(.$`Longitude E Min`/60)+(.$`Longitude E Sec`/3600),
             latdms = paste(.$`Latitude N Deg`, "°", .$`Latitude N Min`, "'", .$`Latitude N Sec`, "''", sep = ""),
             longdms = paste(.$`Longitude E Deg`, "°", .$`Longitude E Min`, "'", .$`Longitude E Sec`, "''", sep = ""),
             .) %>% 
  SpatialPointsDataFrame(.[1:2],.)
#map----
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  #boundary shapefile----
  addPolygons(data = bound,
              color = "#00FF00",
              fillOpacity = 0,
              label = featurename,
              weight = 1.5,
              group = "Beat Boundary") %>%
  #4oom interval points----
  addCircleMarkers(data = df.int,
                   lat = ~latitude,
                   lng = ~longitude,
                   radius = ~0.2,
                   color = "yellow",
                   popup = ~paste(`Transect.ID`,"<br>",
                                  `Plot.No.`, "<br>",
                                  "Location:", `latdms`, ",", `longdms`),
                   opacity = 1,
                   group = "Vegetation Plots") %>% 
  #species sightings----
  addCircleMarkers(data = df.sighting,
                   lat = ~latitude,
                   lng = ~longitude,
                   radius = ~1,
                   color = "red",
                   popup = ~paste(`Species..`,"<br>",
                                  "Transect ID", `Transect.ID`, "<br>",
                                  "Time:",`Sighting.Time`,"<br>",
                                  "Distance  (m):",`Sighting.Distance..m.`,"<br>",
                                  "Animal Bearing:",`Compass.Bearing.Animal`,"<br>",
                                  "Walk Bearing:",`Compass.Bearing.Walk`,"<br>",
                                  "No. of Animals:",`Total.no..of.Animal`,"<br>",
                                  "No. of Adults:",`Adult`,"<br>",
                                  "No. of Young:",`Young`,"<br>",
                                  "Habitat type:",`Habitat.type`,"<br>",
                                  "Terrain type:",`Terrain.type`, "<br>",
                                  "Location:", `latdms`, ",", `longdms`),
                   opacity = 1,
                   group = "Species Sighting") %>% 
  #pellet----
  addPolylines(data= pellet,
               label= ~as.character(id),
               color = "yellow",
               popup = ~paste("Start Location:", `startlatdms`, ",", `startlongdms`, "<br>",
                              "End Location:", `endlatdms`, ",", `endlongdms`),
               opacity = 1,
               weight = 1,
               group = "Pellet Plot") %>% 
  #transect lines----
  addPolylines(data= transect,
               label= ~`Transect.ID`,
               color = "red",
               popup = ~paste("Start Location:", `startlatdms`, ",", `startlongdms`, "<br>",
                              "End Location:", `endlatdms`, ",", `endlongdms`),
               opacity = 1,
               weight = 2,
               group = "Transect Line") %>% 
  #layer control----
  addLayersControl(
  overlayGroups = c("Beat Boundary", "Transect Line","Species Sighting","Pellet Plot","Vegetation Plots"),
  options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup("Species Sighting") %>% 
  hideGroup("Pellet Plot") %>% 
  hideGroup("Vegetation Plots")

#sort data for distance----
#convert bearings to angle----
#check the absolute difference between animal bearing and walk bearing call it 'x'.
#if x is greater than 90, as this may happen when working with bearings greater than 270 and lesser than 90, then subract x from 360 and call this value 'y'.
#if y is strill greater than 90, then use then use the back bearing of the walk and calculate the absolute difference between then animal bearing and the back bearing of the walk call it 'z'.

sighting_angle <- function(animal, walk){
   sighting_angle_raw = ifelse(abs(animal-walk)>90,360-abs(animal-walk),abs(animal-walk))
   backbearing_walk = ifelse(walk>180, walk-180, walk+180)
   sighting_angle_backbearing = ifelse(abs(animal-backbearing_walk)>90,360-abs(animal-backbearing_walk),abs(animal-backbearing_walk))
   return(ifelse(sighting_angle_raw>90,sighting_angle_backbearing,sighting_angle_raw))
  }
Dist_S <- read_excel(datasheet, sheet = sheet_DS) %>% 
  .[!is.na(.$`Sl. No.`),]
Distance <-  read_excel(datasheet, sheet = sheet_DS) %>% 
  .[!duplicated(.$`Transect ID`),] %>% 
  .[!.$`Transect ID` %in% Dist_S$`Transect ID`,] %>% 
  rbind(Dist_S,.) %>% 
  within(.,sightingangle <- sighting_angle(animal = `Compass Bearing Animal`, walk = `Compass Bearing Walk`))

# points to lines code borrowed from https://stackoverflow.com/questions/42487700/how-to-create-a-polyline-for-each-row
