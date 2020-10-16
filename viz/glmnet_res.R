library(data.table)
library(glmnet)
library(Matrix)
library(tidyverse)
library(ggplot2)

all <- fread('~/tweets/all.csv')

#Get levels identical to how they would be in all()
income_levels <- unique(all$income_percap_q)
income_levels <- income_levels[order(income_levels)]

fips_levels <- unique(all$fips)
fips_levels <- fips_levels[order(fips_levels)]

doy_levels <- unique(all$doy)
doy_levels <- doy_levels[order(doy_levels)]

dow_levels <- unique(all$dow)
dow_levels <- dow_levels[order(dow_levels)]

tod_levels <- unique(all$tod)
tod_levels <- tod_levels[order(tod_levels)]

preddf <- expand.grid(list(temp=-10:20, 
                           income_percap_q=factor(income_levels)))
preddf$fips=factor('25025', levels=factor(fips_levels))
preddf$doy=factor('12-01', levels=factor(doy_levels))
preddf$dow=factor('Friday', levels=factor(dow_levels))
preddf$tod=factor('02:1', levels=factor(tod_levels[tod_levels != 'PST']))
preddf$daynum=1000
preddf$year=2018

preddf$temp.hi <- 20:50
preddf$ppt <- 0:30


#######################
# Heat
#########################
load('~/tweets/heat_inco.Rdata')

pmm <- sparse.model.matrix( ~ 1 + temp.hi + income_percap_q + temp.hi*income_percap_q + fips + dow + doy + tod + year*daynum + year, data=preddf)

preddf$heat.res <- predict(mod, pmm)[,1]

#Add missing level for remaining predictions
preddf$tod=factor('02:1', levels=factor(tod_levels))

#######################
# Cold
#########################
load('~/tweets/cold_inco.Rdata')

pmm <- sparse.model.matrix( ~ 1 + temp + income_percap_q + temp*income_percap_q + fips + dow + doy + tod + year*daynum + year, data=preddf)

preddf$cold.res <- predict(mod, pmm)[,1]

#######################
# Rain
#########################
load('~/tweets/rain_inco.Rdata')

pmm <- sparse.model.matrix( ~ 1 + ppt + income_percap_q + ppt*income_percap_q + fips + dow + doy + year*daynum + year, data=preddf)

preddf$rain.res <- predict(mod, pmm)[,1]

######################
# Graphs
###################
ggplot(preddf %>% filter(temp.hi < 40)) + 
  geom_line(aes(x=temp.hi, y=heat.res, color=income_percap_q)) + 
  labs(y='Hedonometer Score', x='Hourly Heat Index Temperature (C)',
       color='Income Quintile') + 
  theme_bw()
ggsave('~/temp-sentiment/res/Income_Heat.png')

ggplot(preddf) + 
  geom_line(aes(x=temp, y=cold.res, color=income_percap_q)) + 
  labs(y='Hedonometer Score', x='Hourly Temperature (C)',
       color='Income Quintile') + 
  theme_bw()
ggsave('~/temp-sentiment/res/Income_Cold.png')

ggplot(preddf) + 
  geom_line(aes(x=ppt, y=rain.res, color=income_percap_q)) + 
  labs(y='Hedonometer Score', x='Daily Precipitation (mm)',
       color='Income Quintile') + 
  theme_bw()
ggsave('~/temp-sentiment/res/Income_Rain.png')

