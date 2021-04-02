library(tidyverse)
library(data.table)
library(lubridate)
library(usmap)

setwd('~/tweets/crime/')

#######################################
# Get Agency Counties
#######################################
#Agency data in old format
cde_fs <- list.files(recursive=T, full.names=T, pattern='cde_agencies....$')
cde <- lapply(cde_fs, fread) %>%
  rbindlist(fill=T)

#Agency data in new format
st_fs <- grep('(?<!cde).agencies', list.files(recursive=T, full.names=T, pattern='agencies....$'),
               perl=T, value=T)

st <- lapply(st_fs, fread) %>%
  rbindlist(fill=T)
st <- st[ , c(2, 18, 55)]
st <- unique(st)

#Use primarily agencies from old data (has primary county fips)
cde <- cde %>%
  select(agency_id, state_abbr, primary_county, primary_county_fips) %>%
  unique

#Get agencies that only exist in new data
#For those with multiple counties, use the last, since that corresponds to the primary county
#in agencies that exists in the old data
# sel <- st[st$AGENCY_ID %in% cde$agency_id & grepl(';', st$COUNTY_NAME), ]
# m <- merge(sel, cde %>% rename(AGENCY_ID=agency_id), all.x=T)
get_last <- function(x){
  r <- str_split(x, '; ')[[1]]
  r[length(r)]
}
st <- st %>%
  filter(!AGENCY_ID %in% cde$agency_id, COUNTY_NAME != '') %>%
  mutate(COUNTY_NAME = sapply(COUNTY_NAME, get_last),
         STATE_ABBR = ifelse(STATE_ABBR == 'NB', 'NE', STATE_ABBR),
         COUNTY_NAME = case_when(COUNTY_NAME == 'LA PORTE' & STATE_ABBR == 'IN' ~ 'LAPORTE',
                                 COUNTY_NAME == 'ST JOSEPH' & STATE_ABBR == 'IN' ~ 'ST. JOSEPH',
                                 COUNTY_NAME == 'ST LOUIS' & STATE_ABBR == 'MN' ~ 'ST. LOUIS',
                                 COUNTY_NAME == 'ST CROIX' & STATE_ABBR == 'WI' ~ 'ST. CROIX',
                                 TRUE ~ COUNTY_NAME),
         FIPS = mapply(fips, state=STATE_ABBR, county=COUNTY_NAME))


agencies <- bind_rows(cde %>%
                        select(agency_id, fips=primary_county_fips),
                      st %>%
                        mutate(fips = as.numeric(FIPS)) %>%
                        select(agency_id=AGENCY_ID, fips))

agencies$agency_id <- as.character(agencies$agency_id)

########################################
# Get Incidents
########################################

old_fs <- list.files(recursive=T, full.names=T, pattern='nibrs_incident')
new_fs <- list.files(recursive=T, full.names=T, pattern='NIBRS_incident')

new_inc <- lapply(new_fs, function(x){fread(x, colClasses=list(character=1:15))}) %>%
  rbindlist(fill=T)
old_inc <- lapply(old_fs, function(x){fread(x, colClasses=list(character=1:17))}) %>%
  rbindlist(fill=T)

new <- new_inc %>%
  #filter(REPORT_DATE_FLAG == '') %>%
  mutate(date = as.character(dmy(INCIDENT_DATE))) %>%
  select(agency_id=AGENCY_ID, incident_id=INCIDENT_ID, date, hour=INCIDENT_HOUR, 
         report_date_flag=REPORT_DATE_FLAG)

old <- old_inc %>%
  #filter(report_date_flag == '') %>%
  mutate(date = substr(ymd_hms(incident_date), 1, 10)) %>%
  select(agency_id, incident_id, date, hour=incident_hour, report_date_flag)

inc <- bind_rows(new, old)
rm(list=c('new_inc', 'old_inc', 'new', 'old'))

###########################
# Get Offenses
#####################

old_fs <- list.files(recursive=T, full.names=T, pattern='nibrs_offense....$')
new_fs <- list.files(recursive=T, full.names=T, pattern='NIBRS_OFFENSE....$')

new <- lapply(new_fs, fread) %>%
  rbindlist(fill=T)
old <- lapply(old_fs, fread) %>%
  rbindlist(fill=T)

new2 <- new %>%
  select(offense_id=OFFENSE_ID, incident_id=INCIDENT_ID, offense_type_id=OFFENSE_TYPE_ID,
         location_id=LOCATION_ID)
old2 <- old %>%
  select(offense_id, incident_id, offense_type_id, location_id)

off <- bind_rows(new2, old2)
off$incident_id <- as.character(off$incident_id)

rm(list=c('new', 'old', 'new2', 'old2'))

############################################
# Get location types and offense types
#############################################

#Seems to be the same across all states/years
loc_type <- read.csv('AL-2009/nibrs_location_type.csv') %>%
  select(location_id, location_name)

#Later surveys have more offense types
off_type <- read.csv('WA-2019/WA/NIBRS_OFFENSE_TYPE.csv') %>%
  select(offense_type_id=OFFENSE_TYPE_ID, offense_name=OFFENSE_NAME,
         offense_category_name=OFFENSE_CATEGORY_NAME)

#######################################
# Combine All
####################################

all(off$incident_id %in% inc$incident_id)

comb <- merge(off, inc, all.x=T)
comb <- merge(comb, agencies, all.x=T, by='agency_id')
comb <- merge(comb, loc_type, all.x=T, by='location_id')
comb <- merge(comb, off_type, all.x=T, by='offense_type_id')

#Filter to only agencies that have monthly reporting from the beginning to the end of the dataset
comb$month <- paste0(substr(comb$date, 1, 7), '-01')
allmonths <- as.character(seq(ymd('2009-01-01'), ymd('2019-12-01'), 'month'))
comb <- comb %>%
  group_by(agency_id) %>%
  filter(all(allmonths %in% month))

fwrite(comb, 'all_crime_data.csv', row.names=F)

violent <- c('Homicide Offenses', 'Animal Cruelty', 'Arson', 'Assault Offenses',
             'Burglary/Breaking & Entering', 'Destruction/Damage/Vandalism of Property',
             'Kidnapping/Abduction',  'Robbery')

comb <- comb[ , .(homicides=sum(offense_category_name == 'Homicide Offenses'), 
                assault=sum(offense_category_name == 'Assault Offenses'),
                violent=sum(offense_category_name %in% violent)),
            .(fips, date)]

comb$date <- as.character(comb$date)
comb <- comb[!is.na(comb$fips), ]

fwrite(comb, 'crime_data.csv', row.names=F)

