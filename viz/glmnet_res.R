library(data.table)
library(glmnet)
library(Matrix)
library(tidyverse)
library(ggplot2)

all <- fread('~/tweets/all.csv')

preddf <- expand.grid(list(ppt=-10:20, 
                           income_percap_q=unique(all$income_percap_q)))
preddf$FIPS=factor('25025', levels=unique(all$FIPS))
preddf$dow=factor('Sunday', levels=unique(all$dow))
preddf$doy=300
preddf$month=factor('10', levels=unique(all$month))

pmm <- sparse.model.matrix( ~ 1 + ppt + income_percap_q + ppt*income_percap_q + FIPS + dow + month + doy, data=preddf)

preddf$res <- predict(mod, pmm)[,1]


ggplot(preddf) + 
  geom_line(aes(x=ppt, y=res, color=income_percap_q))
