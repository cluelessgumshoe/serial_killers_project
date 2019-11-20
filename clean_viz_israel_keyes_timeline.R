##------------------------------------------------------------------------------------------##
##                     Israel Keyes - Location analysis                                     ##
##------------------------------------------------------------------------------------------##

#Data Source: http://dig.abclocal.go.com/wls/documents/wls_081313_Israel%20Keyes_timeline.pdf
## CrimeJunkiePodcast.com
#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages and install if not already 
p_load(tidyverse,magrittr,stringr,lubridate, maps, mapview, sf, feather,ggplot2)


#----------------#
# Load and Clean #
#----------------#

#read in csv; I used either Tableau or Power BI to convert the pdf

israel_keys <- read_csv("data/israel_keyes_crimejunkie_data.csv")

#cleaning

israel_keys_df <- israel_keys %>% #some cleaning by looking at pdf; a couple are only year
  mutate(DATES = if_else(str_detect(DATES,"1998 to 2001"),"7/9/1998 to 7/8/2001",DATES),
         DATES = if_else(str_detect(DATES,"2001 to 2007"),"7/9/2001 to 12/31/2007",DATES)) %>% #don't know for sure; inferring
  separate(DATES,c("FROM_DATE","TO_DATE"),"to") %>%
  mutate(FROM_DATE = mdy(FROM_DATE),
         TO_DATE = mdy(TO_DATE),
    DURATION =
    ifelse(is.na(TO_DATE),1,(TO_DATE - FROM_DATE)+1) #add one to count the day of
  ) %>% 
  arrange(FROM_DATE) %>% 
  filter(LOCATION != "Unknown") %>%  #there are only 2 and they will not do us much good for this
  separate(LOCATION,c("LOCATION","TO_LOCATION")," to ") 

#fix location designations to more than one place
is_key_df_1 <- israel_keys_df %>% #set the from place to 1 day to note presence,but the to place can be the duration
  mutate(DURATION = if_else(is.na(TO_LOCATION),DURATION,1)) %>% 
  select(-TO_LOCATION) 
  
  
is_key_df_2 <- israel_keys_df %>% #leave full durations for to_locations to represent time in those places for map
  select(-LOCATION) %>%
  filter(!is.na(TO_LOCATION)) %>%
  rename(LOCATION = TO_LOCATION)
  
#now bind these together to insert extra rows for the TO_LOCATIONS
israel_keys_df <- union_all(is_key_df_1,is_key_df_2) %>% 
  #add some more interesting data
  mutate(REASON = case_when(
    str_detect(ACTIVITY,"bduction") ~ "Abduction",
    str_detect(ACTIVITY,"irlines") | str_detect(ACTIVITY,"eservation") | str_detect(ACTIVITY,"orader")  ~ "Travel",
    str_detect(ACTIVITY,"oat") |  str_detect(ACTIVITY,"ayak") | str_detect(ACTIVITY,"anoe") |  str_detect(ACTIVITY,"amping")  ~ "Water/Camping" ,
    str_detect(ACTIVITY,"ental") ~ "Car Rental",
    str_detect(ACTIVITY,"uty") | str_detect(ACTIVITY,"Army")  ~ "Army",
    TRUE ~ "Other"
  ))


#----------------#
# get locations  #
#----------------#

#this method would work, but would require more data cleaning, since data set is small google is way easier
#location_data <- maps::us.cities 

# ggmap::register_google(key = "YOUR KEY")
# 
is_key_geo_df <- israel_keys_df %>%
  mutate_geocode(location = LOCATION) %>%
  arrange(FROM_DATE) %>%
  #stretch the lon and lat to be able to connect like routes
  rename(from_lon = lon,
         from_lat = lat) %>%
  mutate(to_lon = lead(from_lon,1),
         to_lat = lead(from_lat,1),
         #jitter slightly so that not identical for route map
         from_lat = jitter(from_lat, factor = 0.1),
         from_lon = jitter(from_lon, factor = 0.1),
         to_lat = jitter(to_lat, factor = 0.1),
         to_lon = jitter(to_lon, factor = 0.1))
feather::write_feather(is_key_geo_df,"data/is_key_geocoded.feather")

is_key_geo_df <- feather::read_feather("data/is_key_geocoded.feather")
#if making changes to buckets/tweaking, bind without re-geocoding
#is_key_geo_df <- is_key_geo_df %>% cbind(REASON = israel_keys_df$REASON) %>% as.data.frame()


#----------------#
# Durations mapp #
#----------------#

#make the sf object
iskey_locations <- st_as_sf(is_key_geo_df, coords = c("from_lon", "from_lat"), crs = 4326)
#set the color palette
mapviewOptions(vector.palette =  viridis::inferno)
#map with sizes and locations colored by reason buckets
mapview(iskey_locations, legend = F, alpha = 0, burst = T, 
        width = "2000", align = "center", cex = "DURATION", zcol = "REASON", 
        #by re-ording these, the dark map can be the default; spooky
        map.types = c("CartoDB.DarkMatter","CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","OpenTopoMap"))
        #this is neat, but looks better with zcol; the popup looks better with this though
        #popup = popupTable(iskey_locations, zcol = c("LOCATION","REASON", "ACTIVITY")))

#----------------#
# Routes map     #
#----------------#

#next challenge: map routes!
#https://rstudio-pubs-static.s3.amazonaws.com/259095_2f8cb24b43284692a8af916bd447931d.html

map_setup <- borders("world", colour="grey80", fill="grey3")

us_travel <- ggplot() + map_setup +
  geom_curve(data=is_key_geo_df,
             aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat),
             col="orangered4",
             size=.5,
             curvature=0.2) +
  geom_point(data=is_key_geo_df,
             aes(x=from_lon, y=from_lat), 
             colour="orangered2",
             size=1.5) +
  geom_point(data=is_key_geo_df,
             aes(x=to_lon, y=to_lat), 
             colour="orangered2") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12)) +
  ggtitle("Israel Keyes: Travel Routes") +
  #check the min/max of long and lat to get good zoom level
  coord_cartesian(ylim=c(10, 75), xlim=c(-160, 70))
us_travel

#----------------#
# Housekeeping   #
#----------------#

rm(list = c("israel_keyes","is_key_df_1","is_key_df_2"))
