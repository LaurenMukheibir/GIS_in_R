#GIS in R
#Shyshark Distribution across the Western Cape
#Lauren Mukheibir 
#02/03/2025

# Setting CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))


#installing packages
install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("rinat")
install.packages("sf")



# reading in libraries
library(tidyverse)
library(rinat)
library(sf)


#reading in data
ssHp <- get_inat_obs( taxon_name = "Haploblepharus pictus",
                    bounds = c (-35, 18, -33.5, 18.5),
                    maxresults = 1000)
View(ssHp)
head(ssHp)

ssHe <- get_inat_obs( taxon_name = "Haploblepharus edwardsii",
                      bounds = c (-35, 18, -33.5, 18.5),
                      maxresults = 1000)
View(ssHe)
head(ssHe)

ssPa <- get_inat_obs( taxon_name = "Poroderma africanum",
                      bounds = c (-35, 18, -33.5, 18.5),
                      maxresults = 1000)
View(ssPa)
head(ssPa)

#Filter returned observations by a range of column attribute criteria
ssHp <- ssHp %>% filter(positional_accuracy<46 & 
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")

class(ssHp)

ssHe <- ssHe %>% filter(positional_accuracy<46 & 
                          latitude<0 &
                          !is.na(latitude) &
                          captive_cultivated == "false" &
                          quality_grade == "research")

class(ssHe)

ssPa <- ssPa %>% filter(positional_accuracy<46 & 
                          latitude<0 &
                          !is.na(latitude) &
                          captive_cultivated == "false" &
                          quality_grade == "research")

class(ssPa)

#Make the dataframe a spatial object of class = "sf"
ssHe <- st_as_sf(ssHe, coords = c("longitude", "latitude"), crs = 4326)
print(ssHe)
class(ssHe)

ssHp <- st_as_sf(ssHp, coords = c("longitude", "latitude"), crs = 4326)
print(ssHp)
class(ssHp)

ssPa <- st_as_sf(ssPa, coords = c("longitude", "latitude"), crs = 4326)
print(ssPa)
class(ssPa)


# Should return "EPSG: 4326"
st_crs(ssHe)
names(ssHe)

#Plots
install.packages("ggplot2")
library(ggplot2)
ggplot() + geom_sf(data=ssHe)
ggplot() + geom_sf(data=ssHp)
ggplot() + geom_sf(data=ssPa)

library(rosm)
library(ggspatial)
ggplot() +
  annotation_map_tile(type = "cartolight", progress = "none") +
  geom_sf(data=ssHe)

ggplot() +
  annotation_map_tile(type = "cartolight", progress = "none") +
  geom_sf(data=ssHp)

ggplot() +
  annotation_map_tile(type = "cartolight", progress = "none") +
  geom_sf(data=ssPa)

library(leaflet)
library(htmltools)
leaflet() %>%
  # Add default OpenStreetMap map tiles
  addTiles(group = "Default") %>%  
  # Add our points
  addCircleMarkers(data = ssHe,
                   group = "Haploblepharus edwardsii",
                   radius = 3, 
                   color = "blue")

leaflet() %>%
  # Add default OpenStreetMap map tiles
  addTiles(group = "Default") %>%  
  # Add our points
  addCircleMarkers(data = ssHp,
                   group = "Haploblepharus pictus",
                   radius = 3, 
                   color = "blue") 

leaflet() %>%
  # Add default OpenStreetMap map tiles
  addTiles(group = "Default") %>%  
  # Add our points
  addCircleMarkers(data = ssPa,
                   group = "Haploblepharus pictus",
                   radius = 3, 
                   color = "blue") 

#common sense checks
library(mapview)
library(leafpop)

mapview(ssHe, 
        popup = 
          popupTable(ssHe,
                     zcol = c("user_login", "captive_cultivated", "url")))
mapview(ssHp, 
        popup = 
          popupTable(ssHp,
                     zcol = c("user_login", "captive_cultivated", "url")))
mapview(ssPa, 
        popup = 
          popupTable(ssPa,
                     zcol = c("user_login", "captive_cultivated", "url")))

## Add link to data - url
install.packages("dplyr")  # Only run if you haven't installed it yet
library(dplyr)

lssHe <- ssHe %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))

mapview(ssHe, 
        popup = 
          popupTable(lssHe,
                     zcol = c("user_login", "captive_cultivated", "click_url")))


lssHp <- ssHp %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))

mapview(ssHp, 
        popup = 
          popupTable(lssHp,
                     zcol = c("user_login", "captive_cultivated", "click_url")))

lssPa <- ssPa %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))

mapview(ssPa, 
        popup = 
          popupTable(lssPa,
                     zcol = c("user_login", "captive_cultivated", "click_url")))


# Extract coordinates (longitude and latitude) from the geometry column
library(sf)
ssHe_coords <- st_coordinates(ssHe)

# Add longitude and latitude as new columns to the dataframe
ssHe$longitude <- ssHe_coords[, 1]
ssHe$latitude <- ssHe_coords[, 2]

# Check the data
head(ssHe)

ssHp_coords <- st_coordinates(ssHp)
ssHp$longitude <- ssHp_coords[, 1]
ssHp$latitude <- ssHp_coords[, 2]


ssPa_coords <- st_coordinates(ssPa)
ssPa$longitude <- ssPa_coords[, 1]
ssPa$latitude <- ssPa_coords[, 2]

# Creating the interactive map with all three species
library(leaflet)

leaflet() %>%
  addTiles() %>%
  
  # Add circle markers for Haploblepharus edwardsii (blue)
  addCircleMarkers(data = ssHe,
                   ~longitude, ~latitude,  # Now use the new longitude and latitude columns
                   color = "blue", 
                   radius = 3, 
                   popup = ~paste("<b>Species:</b>", "Haploblepharus edwardsii", 
                                  "<br><b>User:</b>", user_login, 
                                  "<br><b>Cultivated:</b>", captive_cultivated, 
                                  "<br><a href='", url, "'>Link to observation</a>"),
                   group = "Haploblepharus edwardsii") %>%
  
  # Add circle markers for Haploblepharus pictus (green)
  addCircleMarkers(data = ssHp,
                   ~longitude, ~latitude,  # Use the new longitude and latitude columns
                   color = "green", 
                   radius = 3, 
                   popup = ~paste("<b>Species:</b>", "Haploblepharus pictus", 
                                  "<br><b>User:</b>", user_login, 
                                  "<br><b>Cultivated:</b>", captive_cultivated, 
                                  "<br><a href='", url, "'>Link to observation</a>"),
                   group = "Haploblepharus pictus") %>%
  
  # Add circle markers for Poroderma africanum (red)
  addCircleMarkers(data = ssPa,
                   ~longitude, ~latitude,  # Use the new longitude and latitude columns
                   color = "pink", 
                   radius = 3, 
                   popup = ~paste("<b>Species:</b>", "Poroderma africanum", 
                                  "<br><b>User:</b>", user_login, 
                                  "<br><b>Cultivated:</b>", captive_cultivated, 
                                  "<br><a href='", url, "'>Link to observation</a>"),
                   group = "Poroderma africanum") %>%
  
  # Add layer control to toggle between species
  addLayersControl(
    overlayGroups = c("Haploblepharus edwardsii", "Haploblepharus pictus", "Poroderma africanum"),
    options = layersControlOptions(collapsed = FALSE)
  )



