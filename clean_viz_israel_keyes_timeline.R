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
p_load(tidyverse,magrittr,stringr,lubridate, maps, mapview, sf)


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
    str_detect(ACTIVITY,"irlines") | str_detect(ACTIVITY,"eservation")  ~ "Travel",
    str_detect(ACTIVITY,"oat") |  str_detect(ACTIVITY,"ayak") | str_detect(ACTIVITY,"anoe") |  str_detect(ACTIVITY,"amping")  ~ "Water/Camping" ,
    str_detect(ACTIVITY,"ental") ~ "Car Rental",
    str_detect(ACTIVITY,"uty") ~ "Army",
    TRUE ~ "Other"
  ))


#----------------#
# get locations  #
#----------------#

#this method would work, but would require more data cleaning, since data set is small google is way easier
#location_data <- maps::us.cities 

ggmap::register_google(key = "YOUR KEY")

is_key_geo_df <- israel_keys_df %>% 
  mutate_geocode(location = LOCATION) 
feather::write_feather(is_key_geo_df,"data/is_key_geocoded.feather")

#----------------#
# Make a map     #
#----------------#

#make the sf object
iskey_locations <- st_as_sf(is_key_geo_df, coords = c("lon", "lat"), crs = 4326)
#set the color palette
mapviewOptions(vector.palette =  viridis::inferno)
#map with sizes and locations colored by reason buckets
mapview(iskey_locations, legend = F, alpha = 0, burst = T, 
        width = "2000", align = "center", cex = "DURATION", zcol = "REASON")
        #this is neat, but looks better with zcol; the popup looks better with this though
        #popup = popupTable(iskey_locations, zcol = c("LOCATION","REASON", "ACTIVITY")))

#next challenge: map routes!
#https://rstudio-pubs-static.s3.amazonaws.com/259095_2f8cb24b43284692a8af916bd447931d.html



#----------------#
# Housekeeping   #
#----------------#

rm(list = c("israel_keyes","is_key_df_1","is_key_df_2"))
