library(data.table)
library(zoo)
library(tidyverse)
library(lubridate)
library(fixest)

##################################
# read sentiment data from twitter
##################################
dat <- fread('~/tweets/all.csv')

dat$state <- floor(dat$fips/1000)
dat$date <- paste0(dat$year, '-', dat$doy)

dat <- dat[ , .(afinn=mean(afinn), hedono=mean(hedono), vader=mean(vader), n=length(vader)), .(state, date)]

#Get 30-day running averages of each sentiment score
sel <- dat %>%
  merge(expand.grid(list(state=unique(dat$state),
                         date=as.character(seq(ymd('2009-01-01'), ymd('2019-12-31'),
                                               by='day'))),
                    stringsAsFactors=F), all=T) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(afinn30 = zoo::rollmean(afinn, k=30, align='right', fill=NA, na.rm=T),
         hedono30 = zoo::rollmean(hedono, k=30, align='right', fill=NA, na.rm=T),
         vader30 = zoo::rollmean(vader, k=30, align='right', fill=NA, na.rm=T))

##############################
# Read in mental health data
###############################
mh <- fread('~/tweets/brfss/brfss_mentalhealth.csv')
names(mh) <- c('state', 'mh', 'date')
mh$date <- as.character(mh$date)
mh$mh_any <- mh$mh > 0

m <- merge(mh, sel, all.x=T, all.y=F) %>%
  na.omit
m$year <- substr(m$date, 1, 4)
m$yearmonth <- substr(m$date, 1, 7)
m$state <- factor(m$state)

##############################
# Try models
#############################

boots <- NULL
for (i in 1:100){
  print(i)
  sel <- m
  mod <- feglm(mh ~ vader30 | state + yearmonth, 
               data=sel[sample(1:nrow(sel), nrow(sel), replace=T), ],
               family='poisson')
  boots <- c(boots, mod$coefficients)
}


