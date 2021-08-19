library(sf)
library(tidyverse)

setwd('~/tweets/')

#Because it is 5-year average, use the middle year of the 5.  So 2011 is 2007-2011, so most accurate to 2009

year <- 2012
data_year <- ifelse(year < 2017, year + 2, 2018)

sp <- read_sf(paste0('wealth/nhgis0002_shape/US_blck_grp_', data_year, '.shp'))

sp_pts <- st_centroid(sp)

urb_rur <- read_sf('wealth/tl_2017_us_uac10.shp')

st_pts_pj <- st_transform(sp_pts, st_crs(urb_rur))

cts <- st_within(st_pts_pj, urb_rur)

sp$urban <- sapply(cts, length)

dat <- read.csv(list.files(path='wealth/nhgis0003_csv/',
                           full.names=T,
                           pattern=paste0(data_year, '5_', data_year, '.*grp_E.csv')))


names(dat)[38:94] <- c("TOTAL_POPULATION", "RACE_TOT", "RACE_NHISP_TOTXX", "RACE_WHITE", 
                       "RACE_BLACK", "RACE_INDIAN", "RACE_ASIAN", "RACE_OTHER1", "RACE_OTHER2",
                       "RACE_OTHER3", "RACE_OTHER4", "RACE_OTHER5", "RACE_HISP", 
                       "RACE_HISPXX1", "RACE_HISPXX2", "RACE_HISPXX3", "RACE_HISPXX4", 
                       "RACE_HISPXX5", "RACE_HISPXX6", "RACE_HISPXX7", "RACE_HISPXX8", 
                       "RACE_HISPXX9", "TRAN_TOT", "TRAN_CAR1", "TRAN_CAR2", "TRAN_CAR3", 
                       "TRAN_CAR4", "TRAN_CAR5", "TRAN_CAR6", "TRAN_CAR7", "TRAN_CAR8", 
                       "TRAN_PUB1", "TRAN_PUB2", "TRAN_PUB3", "TRAN_PUB4", "TRAN_PUB5", 
                       "TRAN_PUB6", "TRAN_CAR9", "TRAN_EXP1", "TRAN_EXP2", "TRAN_EXP3", 
                       "TRAN_OTHER", "TRAN_WFH", "TIME_TOT", "TIME_LT5", "TIME_5_9",
                       "TIME_10_14", "TIME_15_19", "TIME_20_24", "TIME_25_29", "TIME_30_34", 
                       "TIME_35_39", "TIME_40_44", "TIME_45_59", "TIME_60_89", "TIME_GT90",
                       "INCOME_PERCAP")

dat <- dat %>%
  mutate(RACE_WHITE = RACE_WHITE/RACE_TOT,
         RACE_BLACK = RACE_BLACK/RACE_TOT,
         RACE_OTHER = (RACE_OTHER1 + RACE_OTHER2 + RACE_OTHER3 + RACE_OTHER4 + 
                       RACE_OTHER4 + RACE_OTHER5 + RACE_INDIAN + RACE_ASIAN)/RACE_TOT,
         RACE_HISP = RACE_HISP/RACE_TOT,
         FIPS_COUNTY = substr(1000 + COUNTYA, 2, 5),
         FIPS_STATE = substr(100 + STATEA, 2, 3)) %>%
  select(RACE_WHITE, RACE_BLACK, RACE_OTHER, RACE_HISP, INCOME_PERCAP,
         GISJOIN, FIPS_STATE, FIPS_COUNTY, RACE_TOT)

m <- merge(sp, dat)

d <- st_drop_geometry(m)

d %>%
  mutate(poor = INCOME_PERCAP < 15000) %>%
  group_by(poor, urban) %>%
  summarize(blackness = weighted.mean(RACE_BLACK, w=(RACE_TOT)), n())

#   poor  urban blackness  `n()`
#   <lgl> <int>     <dbl>  <int>
# 1 FALSE     0    0.0573  55124
# 2 FALSE     1    0.119  129486
# 3 TRUE      0    0.221    4543
# 4 TRUE      1    0.270   26114
# 5 NA        0    0.0356    191
# 6 NA        1    0.375     373




