library(sf)
library(tidyverse)

setwd('~/tweets/')

#Because it is 5-year average, use the middle year of the 5.  So 2011 is 2007-2011, so most accurate to 2009

#Convert all to 2019 dollars
#https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=201106&year2=201906
convert <- data.frame(year=2011:2018,
                      conv=c(1.134772, 1.1161985, 1.0969534, 1.0746823, 
                             1.0733538, 1.0627546, 1.0456737, 1.0164848))

sp <- read_sf('wealth/nhgis0002_shape', 'US_blck_grp_2014')

xmin <- 1994000
ymin <- 794916
ymax <- 820000
xmax <- 2027500

#Crop to Suffolk County MA
sp <- sp %>%
  st_make_valid %>%
  st_crop(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

ggplot(sp) + 
  geom_sf()

dat <- read.csv('wealth/nhgis0003_csv/nhgis0003_ds206_20145_2014_blck_grp_E.csv')

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
  mutate(FIPS_COUNTY = substr(1000 + COUNTYA, 2, 5),
         FIPS_STATE = substr(100 + STATEA, 2, 3)) %>%
  select(INCOME_PERCAP, GISJOIN, FIPS_STATE, FIPS_COUNTY, TOTAL_POPULATION)

dat$INCOME_PERCAP <- dat$INCOME_PERCAP*convert$conv[convert$year==2014] 

m <- merge(sp, dat)

twt <- read.csv('wealth/results/2014-01-01.csv') %>%
  na.omit %>%
	st_as_sf(coords=c('lon', 'lat'), crs=4326, remove=F) %>%
	st_transform(crs=st_crs(m)) %>%
  st_crop(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax)

ggplot() +
  geom_sf(data=m, aes(fill=log(INCOME_PERCAP)), color=NA) + 
  geom_sf(data=twt, size=0.25) + 
  scale_fill_gradientn(colors=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4",
                                "#e4ea9a", "#fbf8c0", "#fce08a", "#faae61", 
                                "#f36c44", "#a01c44"),
                       na.value='#ffffff',
                       labels = function(x) prettyNum(round(exp(x), -3), big.mark=',')) + 
  theme_void() + 
  labs(fill="Per-Capita Income")
ggsave('~/temp-sentiment/res/Boston_Map.png', width=7, height=7)


