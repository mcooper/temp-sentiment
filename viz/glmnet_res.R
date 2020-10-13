library(data.table)
library(glmnet)
library(Matrix)
library(tidyverse)
library(ggplot2)

all <- fread('~/tweets/all.csv')

income_levels <- unique(all$income_percap_q)
income_levels <- income_levels[order(income_levels)]

preddf <- expand.grid(list(temp=-10:20, 
                           income_percap_q=factor(income_levels)))
preddf$FIPS=factor('25025', levels=unique(all$FIPS))
preddf$dow=factor('Sunday', levels=unique(all$dow))
preddf$daynum=1000
preddf$month=factor('10', levels=unique(all$month))

preddf$temp.hi <- 20:50
preddf$ppt <- 0:30


#######################
# Heat
#########################
load('~/tweets/heat_inco.Rdata')

pmm <- sparse.model.matrix( ~ 1 + temp.hi + income_percap_q + temp.hi*income_percap_q + FIPS + dow + month + daynum, data=preddf)

preddf$heat.res <- predict(mod, pmm)[,1]

#######################
# Cold
#########################
load('~/tweets/cold_inco.Rdata')

pmm <- sparse.model.matrix( ~ 1 + temp + income_percap_q + temp*income_percap_q + FIPS + dow + month + daynum, data=preddf)

preddf$cold.res <- predict(mod, pmm)[,1]

#######################
# Rain
#########################
load('~/tweets/rain_inco.Rdata')

pmm <- sparse.model.matrix( ~ 1 + ppt + income_percap_q + ppt*income_percap_q + FIPS + dow + month + daynum, data=preddf)

preddf$rain.res <- predict(mod, pmm)[,1]

######################
# Graphs
###################

ggplot(preddf %>% filter(temp.hi < 40)) + 
  geom_line(aes(x=temp.hi, y=heat.res, color=income_percap_q)) + 
  labs(y='Hedonometer Score', x='Hourly Heat Index Temperature (C)',
       color='Income Quintile') + 
  theme_bw()
ggsave('~/temp-sentiment/res/Income Heat.png')

ggplot(preddf) + 
  geom_line(aes(x=temp, y=cold.res, color=income_percap_q)) + 
  labs(y='Hedonometer Score', x='Hourly Temperature (C)',
       color='Income Quintile') + 
  theme_bw()
ggsave('~/temp-sentiment/res/Income Cold.png')

ggplot(preddf) + 
  geom_line(aes(x=ppt, y=rain.res, color=income_percap_q)) + 
  labs(y='Hedonometer Score', x='Daily Precipitation (mm)',
       color='Income Quintile') + 
  theme_bw()
ggsave('~/temp-sentiment/res/Income Rain.png')

