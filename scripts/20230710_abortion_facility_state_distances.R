library(sf)
library(tigris)
library(gmapsdistance)
library(ggmap)
library(tidyverse)


# Script to calculate distances from center of states neighboring Indiana that are 
# counted as having residents receive abortions in Indiana by Terminated Pregnancy 
# Reports published by Indiana Department of Health.
# REQUIRES GOOGLE API code - not supplied in this script but available here:
# # https://developers.google.com/maps/documentation/distance-matrix/get-api-key#key

# import state geometry 
states <- states(cb=T)

# filter to neighboring states listed in reports

states <- states %>% filter(NAME == "Illinois" | NAME == "Michigan" | NAME == "Kentucky" |
                              NAME == "Tennessee" | NAME == "Indiana" | NAME == "Ohio")

# using st_centroid from sf package add column to states dataframe with geographic center 
# coordinates to calculate distance from each state to each Indiana facility 
# listed in report. 

# generate longitude and latitude for center of each state and add to state dataframe
# as cntr column
states$cntr <- st_centroid(states$geometry)

# create states2 dataframe with center data and create coordinates column with longitude
# and latitude data in format gmapsdistance package can use: lon + lat    
# remove geometry column.

states2 <- data.frame(states %>% 
  mutate(
    lat = unlist(map(states$cntr,1)),
    lon = unlist(map(states$cntr,2)),
    coords = str_c(lon, lat, sep = "+"))) %>% 
  select(NAME, coords, -geometry)


# import provider location scraped from 2022 Terminated Pregnancy Reports and filter
# to 2022 data. Use facility and county column to generate new location column to
# send to geocode function to generate latitude and longitude coordinates then 
# format so gmapsdistance package can use it: lon + lat   
prov <- read_csv("Exported_Data/provider_residency.csv") %>% 
  # filter to current year
  filter(year ==2022) %>% 
  # combine facility name, County and IN and send to ggmap::geocode to generate
  # lat and lon coordinates
  mutate(geocode(
  location = str_c(facility, paste(county, "County, IN"), 
                   sep = ", "),
  output = "latlon"),
  # concatenate to format gmapsdistance package will use - lat+lon
  coord = str_c(lat, lon,sep = "+"))


# generate time and distance from each facility to center of states listed as 
# having residents receive abortions in Indiana. 

dist <- gmapsdistance(
  origin = states2$coords,
  destination = prov$coord,
  mode = "driving",
  key = google_api_key) 


# convert to dataframe
dist <- data.frame(dist)


# filter to distance only
dist <- dist %>% select(starts_with("Distance"))

# create vector of state names
sn <- states2$NAME

# vector of facility names
pf <- prov$facility

# rename columns after facilities 
colnames(dist) <- pf

# add state names as column
dist$state <- states2$NAME

# reorder columns so state names are first 
dist <- dist %>% select(state, 1:7)

# convert meters to miles

dist <- dist %>% pivot_longer(cols = 2:8,
                              names_to = "facility",
                              values_to = "distance") %>%
        mutate(distance = round(distance/1609))
  

# write to csv

write_csv(dist, "exported_data/facility_state_distance.csv")


