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
  form <- as.formula(paste0(gd$out[i], ' ~ ', gd$ind[i], ' + log(Population) | dow + doy + year + statemonth'))
  mod <- feglm(form, data=comb, family=poisson(link=log), weight=comb$n)
  td <- tidy(mod)
  gd$statistic[i] <- td[td$term == gd$ind[i], 'statistic', drop=T]
}

#Super strong effects!!
        out    ind statistic
1   violent  vader -23.06569
2   assault  vader -28.63416
3 homicides  vader -10.55849
4   violent  afinn -38.77270
5   assault  afinn -31.05096
6 homicides  afinn -18.60137
7   violent hedono -26.54292
8   assault hedono -29.01932
9 homicides hedono -14.39753

# Although, not when we account for fips fixed effects :-(
        out    ind statistic2
1   violent  vader  0.1505547
2   assault  vader  0.3673532
3 homicides  vader -0.8659280
4   violent  afinn -0.3308100
5   assault  afinn -0.5347304
6 homicides  afinn -2.1425806
7   violent hedono -1.3216241
8   assault hedono -1.3683346
9 homicides hedono -0.9929105

