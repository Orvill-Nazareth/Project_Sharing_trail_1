library(sp)
library(readxl)
library(dplyr)
library(raster)
library(rgdal)
library(stringr)
library(leaflet)
#load the files----
datasheet <- "G:\\active\\wii buxa\\data\\transect sampling mar2020\\data\\workspace\\buxa\\BD\\Herbivore Census_BD.xlsx"
shapefile_dir <- "G:/active/wii buxa/data/political/beat/corrected"
shapefile <- "Beat"
#name of the datafield in shapefile to be displayed in map----
featurename <- ~`Beat`
#DS----
    #Load the excel sheet from the raw file----
DS_raw <- read_excel(datasheet, sheet = "Direct Sighting (Start to End)")
    #extract data to map for only start and end points of the transect----
DS_raw <- arrange(DS_raw, `Date`)
transect_raw <- DS_raw[!duplicated(DS_raw$`Transect ID`),]
    #process data to map transect lines----
transect_lat_long <- within(transect_raw, {"endlongdms" <- paste(`End Longitude E Deg`, "?", `End Longitude E Min`, "'", `End Longitude E Sec`, "''", sep = "");
                                           "endlatdms" <- paste(`End Latitude N Deg`, "?", `End Latitude N Min`, "'", `End Latitude N Sec`, "''", sep = "");
                                           "startlongdms" <- paste(`Start Longitude E Deg`, "?", `Start Longitude E Min`, "'", `Start Longitude E Sec`, "''", sep = "");
                                           "startlatdms" <- paste(`Start Latitude N Deg`, "?", `Start Latitude N Min`, "'", `Start Latitude N Sec`, "''", sep = "");end_long <- `End Longitude E Deg`+(`End Longitude E Min`/60)+(`End Longitude E Sec`/3600);
                                            end_lat <- `End Latitude N Deg`+(`End Latitude N Min`/60)+(`End Latitude N Sec`/3600);
                                            start_long <- `Start Longitude E Deg`+(`Start Longitude E Min`/60)+(`Start Longitude E Sec`/3600);
                                            start_lat <- `Start Latitude N Deg`+(`Start Latitude N Min`/60)+(`Start Latitude N Sec`/3600)})
transect <- transect_lat_long[c("Division",                              "Range",                                 "Beat",                                  "Date",                                 
                                "Transect ID",                           "Transect Replicate/Day No.",            "Transect Bearing (Compass)",            "Transect Length (km)",                 
                                "Start Time",                            "Habitat Type",                          "Terrain Type",                          "End Time",                             
                                "Weather Sunny / Cloudy / Rain",         "Observer 1 (Team Leader) Name",         "Observer 1 (Team Leader) Designation",  "Observer 1 (Team Leader) Phone No.",   
                                "Observer 2 Name",                       "Observer 2 Designation",                "Observer 2 Phone No.",                  "Observer 3 Name",                      
                                "Observer 3 Designation",                "Observer 3 Phone No.",                  "Observer 4 Name",                       "Observer 4 Designation",               
                                "Observer 4 Phone No.",                  "Sl. No.",                               "Sighting Time",                         "Sighting Distance (m)",                
                                "Compass Bearing Animal",                "Compass Bearing Walk",                  "Species #",                             "Total no. of Animal",                  
                                "Adult",                                 "Young",                                 "Habitat type",                          "Terrain type",                         
                                "Remarks",                               "Datasheet Verified by",                 "Data entered by",                       "Mobile number of person entering data",
                                "start_lat",                             "start_long",                            "end_lat",                               "end_long",                             
                                "startlatdms",                           "startlongdms",                          "endlatdms",                             "endlongdms")]
    #create shapefile----
transect_lines <- apply(transect,1,function(x){
  points <- data.frame(lng=as.numeric(c(x["start_long"], 
                                        x["end_long"])),
                       lat=as.numeric(c(x["start_lat"], 
                                        x["end_lat"])),stringsAsFactors = F)
  coordinates(points) <- c("lng","lat")
  Lines(Line(points),ID=x["Transect ID"])
})
row.names(transect) <- transect$`Transect ID`
transect_lines <- SpatialLinesDataFrame(SpatialLines(transect_lines),transect)
    #process data to map for each sighting points----
sighting_raw <- DS_raw[!is.na(DS_raw$`Sl. No.`),]
sighting_lat_long <- within(sighting_raw, { latdms <- paste(`Latitude N Deg`, "?", `Latitude N Min`, "'", `Latitude N Sec`, "''", sep = "");
                                            longdms <- paste(`Longitude E Deg`, "?", `Longitude E Min`, "'", `Longitude E Sec`, "''", sep = "");
                                            longitude <- `Longitude E Deg`+(`Longitude E Min`/60)+(`Longitude E Sec`/3600);
                                            latitude <- `Latitude N Deg`+(`Latitude N Min`/60)+(`Latitude N Sec`/3600)})
sighting <- sighting_lat_long[c("Division",                              "Range",                                 "Beat",                                  "Date",                                 
                                "Transect ID",                           "Transect Replicate/Day No.",            "Transect Bearing (Compass)",            "Transect Length (km)",                 
                                "Start Time",                            "Habitat Type",                          "Terrain Type",                          "End Time",                             
                                "Weather Sunny / Cloudy / Rain",         "Observer 1 (Team Leader) Name",         "Observer 1 (Team Leader) Designation",  "Observer 1 (Team Leader) Phone No.",   
                                "Observer 2 Name",                       "Observer 2 Designation",                "Observer 2 Phone No.",                  "Observer 3 Name",                      
                                "Observer 3 Designation",                "Observer 3 Phone No.",                  "Observer 4 Name",                       "Observer 4 Designation",               
                                "Observer 4 Phone No.",                  "Sl. No.",                               "Sighting Time",                         "Sighting Distance (m)",                
                                "Compass Bearing Animal",                "Compass Bearing Walk",                  "Species #",                             "Total no. of Animal",                  
                                "Adult",                                 "Young",                                 "Habitat type",                          "Terrain type",                         
                                "Remarks",                               "Datasheet Verified by",                 "Data entered by",                       "Mobile number of person entering data",
                                "latitude",                              "longitude",                             "longdms",                               "latdms")]
    #create shapefile----
sighting_point <- SpatialPointsDataFrame(sighting[41:42],sighting)
#pellet----
    #load the excel sheet from the raw file----
pellet_raw <- read_excel(datasheet, sheet = "20mx2m plot")
    #process data to map pellet lines----
pellet_lat_long <- within(pellet_raw, {"endlongdms" <- paste(`End GPS Location E Deg`, "?", `End GPS Location E Min`, "'", `End GPS Location E Sec`, "''", sep = "");
                                       "endlatdms" <- paste(`End GPS Location N Deg`, "?", `End GPS Location N Min`, "'", `End GPS Location N Sec`, "''", sep = "");
                                       "startlongdms" <- paste(`Start GPS Location E Deg`, "?", `Start GPS Location E Min`, "'", `Start GPS Location E Sec`, "''", sep = "");
                                       "startlatdms" <- paste(`Start GPS Location N Deg`, "?", `Start GPS Location N Min`, "'", `Start GPS Location N Sec`, "''", sep = "");end_long <- `End GPS Location E Deg`+(`End GPS Location E Min`/60)+(`End GPS Location E Sec`/3600);
                                        end_lat <- `End GPS Location N Deg`+(`End GPS Location N Min`/60)+(`End GPS Location N Sec`/3600);
                                        start_long <- `Start GPS Location E Deg`+(`Start GPS Location E Min`/60)+(`Start GPS Location E Sec`/3600);
                                        start_lat <- `Start GPS Location N Deg`+(`Start GPS Location N Min`/60)+(`Start GPS Location N Sec`/3600);
                                        id <- paste(`Transect ID`, `Plot No.`, sep = " ")})
pellet <- pellet_lat_long[c("Division",                                         "Range",                                            "Beat",                                            
                            "Date",                                             "Transect ID",                                      "Observer Name",                                   
                            "Observer Mobile Number",                           "Plot No.",                                         "Start Time",                                      
                            "End Time",                                         "Chital",                                           "Sambar",                                          
                            "Barking Deer",                                     "Goral",                                            "Serow",                                           
                            "Hog deer",                                         "Wild pig",                                         "Elephant",                                        
                            "Rhinoceros",                                       "Gaur",                                             "Wild buffalo",                                    
                            "Hare",                                             "Macaque",                                          "Langur",                                          
                            "Cattle / Buffalo",                                 "Sheep / goat",                                     "Other wild animal",                               
                            "Remarks",                                          "Do goat / sheep graze the sampled area? Yes / No", "id",                                              
                            "start_lat",                                        "start_long",                                       "end_lat",                                         
                            "end_long",                                         "startlatdms",                                      "startlongdms",                                    
                            "endlatdms",                                        "endlongdms")]
    #create shapefile----
pellet_lines <- apply(pellet,1,function(x){
  points <- data.frame(lng=as.numeric(c(x["start_long"], 
                                        x["end_long"])),
                       lat=as.numeric(c(x["start_lat"], 
                                        x["end_lat"])),stringsAsFactors = F)
  coordinates(points) <- c("lng","lat")
  Lines(Line(points),ID=x["id"])
})
row.names(pellet) <- pellet$id
pellet_lines <- SpatialLinesDataFrame(SpatialLines(pellet_lines),pellet)
#veg----
    #load the excel sheet from the raw file----
veg_raw <- read_excel(datasheet , sheet = "Human Dist. (15m) & Veg (1m)")
    #process data to map vegetation plots----
veg_lat_long <- within(veg_raw, {latdms <- paste(`Latitude N Deg`, "?", `Latitude N Min`, "'", `Latitude N Sec`, "''", sep = "");
                                longdms <- paste(`Longitude E Deg`, "?", `Longitude E Min`, "'", `Longitude E Sec`, "''", sep = "");
                                longitude <- `Longitude E Deg`+(`Longitude E Min`/60)+(`Longitude E Sec`/3600);
                                latitude <- `Latitude N Deg`+(`Latitude N Min`/60)+(`Latitude N Sec`/3600)})
veg <- veg_lat_long[c("Division",                                        "Range",                                           "Beat",                                           
                      "Date",                                            "Transect ID",                                     "Plot No.",                                       
                      "Habitat type",                                    "Terrain type",                                    "Canopy Cover",                                   
                      "No. of Wood / Tree Cutting within the plot",      "No. of Tree Lopping within the plot",             "No. of Human / Livestock trail within the plot", 
                      "No. of People seen from the plot",                "No. of Livestock / Cattle seen from the plot",    "Grass/Bamboo Cutting within the plot (Yes/No)",  
                      "Percent cover of Dry Leaf Litter (%) in 1m Plot", "Dry Grass (%)",                                   "Green Grass (%)",                                
                      "Herb (%)",                                        "Weed (%)",                                        "Bare Ground (%)",                                
                      "latitude",                                        "longitude",                                       "longdms",                                        
                      "latdms")]
    #create shapefile----
Veg_point <- SpatialPointsDataFrame(veg[,22:23],veg)
   
#map----
    #add shapefiles to r----
 
bound_shp <- readOGR(dsn = shapefile_dir, layer = shapefile)
bound <- spTransform(bound_shp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    #create the compiled map----
map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(data = bound, color = "#00FF00", fillOpacity = 0, label = featurename, weight = 1.5, group = "Beat Boundary") %>% 
  addCircleMarkers(data = Veg_point, lat = ~latitude, lng = ~longitude, radius = ~0.2, color = "yellow", popup = ~paste(`Transect ID`,"<br>",`Plot No.`, "<br>",
                                                                                                                        "Location:", `latdms`, ",", `longdms`), opacity = 1, group = "Vegetation Plots") %>% 
  addCircleMarkers(data = sighting_point, lat = ~latitude, lng = ~longitude, radius = ~1, color = "red", popup = ~paste(`Species #`,"<br>",
                                                                                                                        "Time:",`Sighting Time`,"<br>",
                                                                                                                        "Distance  (m):",`Sighting Distance (m)`,"<br>",
                                                                                                                        "Animal Bearing:",`Compass Bearing Animal`,"<br>",
                                                                                                                        "Walk Bearing:",`Compass Bearing Walk`,"<br>",
                                                                                                                        "No. of Animals:",`Total no. of Animal`,"<br>",
                                                                                                                        "No. of Adults:",`Adult`,"<br>",
                                                                                                                        "No. of Young:",`Young`,"<br>",
                                                                                                                        "Habitat type:",`Habitat type`,"<br>",
                                                                                                                        "Terrain type:",`Terrain type`, "<br>",
                                                                                                                        "Location:", `latdms`, ",", `longdms`), opacity = 1, group = "Species Sighting") %>% 
  addPolylines(data=pellet_lines,label= ~as.character(id),color = "yellow", popup = ~paste("Start Location:", `startlatdms`, ",", `startlongdms`, "<br>",
                                                                                           "End Location:", `endlatdms`, ",", `endlongdms`), opacity = 1,weight = 1, group = "Pellet Plot") %>% 
  addPolylines(data=transect_lines,label= ~`Transect ID`,color = "red", popup = ~paste("Start Location:", `startlatdms`, ",", `startlongdms`, "<br>",
                                                                                       "End Location:", `endlatdms`, ",", `endlongdms`), opacity = 1,weight = 2, group = "Transect Line") %>% 
  addLayersControl(
  overlayGroups = c("Beat Boundary", "Transect Line","Species Sighting","Pellet Plot","Vegetation Plots"),
  options = layersControlOptions(collapsed = FALSE)
)
map %>% hideGroup("Species Sighting") %>% hideGroup("Pellet Plot") %>% hideGroup("Vegetation Plots")
#sort data for distance----
#convert bearings to angle----
DS_raw_bearing <- within (DS_raw, {sighting_angle <- ifelse( ifelse(abs(`Compass Bearing Animal`-`Compass Bearing Walk`)>90,
                                                                    360-abs(`Compass Bearing Animal`-`Compass Bearing Walk`),
                                                                    abs(`Compass Bearing Animal`-`Compass Bearing Walk`))>90,
                                                             90,
                                                             ifelse(abs(`Compass Bearing Animal`-`Compass Bearing Walk`)>90,
                                                                    360-abs(`Compass Bearing Animal`-`Compass Bearing Walk`),
                                                                    abs(`Compass Bearing Animal`-`Compass Bearing Walk`)))})

#format rows----
blank_transects <- transect_raw[!transect_raw$`Transect ID` %in% sighting_raw$`Transect ID`,]
distance <- rbind(sighting_raw, blank_transects)


# points to lines code borrowed from https://stackoverflow.com/questions/42487700/how-to-create-a-polyline-for-each-row
