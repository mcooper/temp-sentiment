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

dat$hour <- as.numeric(substr(dat$tod, 1, 2))

dat <- dat[ , .(afinn=mean(afinn), hedono=mean(hedono), vader=mean(vader), n=length(vader)), .(fips, date, hour)]

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
            .(fips, date, hour)]

crim$date <- as.character(crim$date)
crim <- crim[!is.na(crim$fips) & !is.na(crim$hour), ]

##################################
# Read in population data
#################################
pop <- bind_rows(lapply(list.files('~/tweets/suicide', pattern='^Population.*txt$', full.names=T),
                        read.delim)) %>%
  select(Population, fips=County.Code, year=Yearly.July.1st.Estimates)

######################################
# Combine
###################################

comb <- merge(crim, dat, all.x=T)
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
  form <- as.formula(paste0(gd$out[i], ' ~ ', gd$ind[i], ' + Population | hour + dow + doy + year + statemonth'))
  mod <- feglm(form, data=comb, family='poisson')
  td <- tidy(mod)
  gd$statistic2[i] <- td[td$term == gd$ind[i], 'statistic', drop=T]
}

[ins] 20:23:58 $> gd
        out    ind statistic2
1   violent  vader -16.338280
2   assault  vader -18.104081
3 homicides  vader  -3.346718
4   violent  afinn -16.630834
5   assault  afinn -20.156434
6 homicides  afinn  -2.856319
7   violent hedono -13.307081
8   assault hedono -13.435093
9 homicides hedono  -3.350331










