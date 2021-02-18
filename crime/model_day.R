library(data.table)
library(tidyverse)
library(lubridate)
library(fixest)
library(broom)

##################################
# read sentiment data from twitter
##################################
dat <- fread('~/tweets/all.csv')

dat$date <- paste0(dat$year, '-', dat$doy)

dat_day <- dat[ , .(afinn=mean(afinn), hedono=mean(hedono), vader=mean(vader), n=length(vader)), .(fips, date)]

###############################
# read crime data
###############################

crim <- fread('~/tweets/crime/all_crime_data.csv')

violent <- c('Homicide Offenses', 'Animal Cruelty', 'Arson', 'Assault Offenses',
             'Burglary/Breaking & Entering', 'Destruction/Damage/Vandalism of Property',
             'Kidnapping/Abduction',  'Robbery')

crim <- crim[ , .(homicides=sum(offense_category_name == 'Homicide Offenses'), 
                assault=sum(offense_category_name == 'Assault Offenses'),
                violent=sum(offense_category_name %in% violent)),
            .(fips, date)]

crim$date <- as.character(crim$date)
crim <- crim[!is.na(crim$fips), ]

##################################
# Read in population data
#################################
pop <- bind_rows(lapply(list.files('~/tweets/suicide', pattern='^Population.*txt$', full.names=T),
                        read.delim)) %>%
  select(Population, fips=County.Code, year=Yearly.July.1st.Estimates)

######################################
# Combine
###################################

comb <- merge(crim, dat_day, all.x=T)
comb$year <- as.numeric(substr(comb$date, 1, 4))
comb <- merge(comb, pop, all.x=T, by=c('year', 'fips'))
comb <- comb[!is.na(comb$afinn), ]
comb$state <- as.factor(floor(comb$fips/1000))
comb$month <- as.factor(substr(comb$date, 6, 7))

comb$countymonth <- paste0(comb$fips, '-', comb$month)
comb$stateyear <- paste0(comb$state, '-', comb$year)
comb$statemonth <- paste0(comb$state, '-', comb$month)
comb$dow <- as.factor(wday(ymd(comb$date)))
comb$doy <- substr(comb$date, 6, 10)

##################################
# Model
#################################3

gd <- expand.grid(list(out=c('violent', 'assault', 'homicides'),
                       ind=c('vader', 'afinn', 'hedono')))
for (i in 1:nrow(gd)){
  form <- as.formula(paste0(gd$out[i], ' ~ ', gd$ind[i], ' + Population | dow + doy + year + statemonth'))
  mod <- feglm(form, data=comb, family='poisson')
  td <- tidy(mod)
  gd$statistic2[i] <- td[td$term == gd$ind[i], 'statistic', drop=T]
}

#Super strong effects!!
[ins] 20:23:58 $> gd
        out    ind statistic2
1   violent  vader  -23.46551
2   assault  vader  -24.49976
3 homicides  vader  -11.60528
4   violent  afinn  -25.46395
5   assault  afinn  -25.88245
6 homicides  afinn  -11.63791
7   violent hedono  -39.83189
8   assault hedono  -39.22778
9 homicides hedono  -16.86443



