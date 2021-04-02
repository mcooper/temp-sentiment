library(tidyverse)
library(lubridate)
library(sf)
library(tigris)

setwd('~/tweets/crime/CrimeOpenDatabase')

d1 <- bind_rows(lapply(list.files(pattern='core'), read.csv)

###########################
# Get FIPS of crime
##########################

#Get counties, subset to those pertainable to cities where we have data
cty <- counties() %>%
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  filter(FIPS %in% c("04019", "06075", "17031", "21111", "26163", "29037", "29047",
                     "29095", "29165", "29510", "36005", "36047", "36061", "36081",
                     "36085", "48121", "48367", "48439", "48453", "48491", "53033"))

ds <- st_as_sf(d1, coords = c('longitude', 'latitude'), crs=st_crs(cty))

start <- Sys.time()
int <- st_join(ds[1:100000, ], cty, join=st_within)
end <- Sys.time()
end - start

d1$fips <- paste0(int$STATEFP, int$COUNTYFP)

d2 <- d1 %>%
  mutate(year = substr(date_single, 1, 4))
  group_by(city_name) %>%

write.csv(d1, '../CrimeOpenDatabase.csv', row.names=F)

violent <- c("arson", "assault offenses", "burglary/breaking & entering", 
             "destruction/damage/vandalism of property", "kidnapping/abduction",
             "robbery", "homicide offenses")
