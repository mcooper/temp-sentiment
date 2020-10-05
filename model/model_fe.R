library(data.table)
library(Hmisc)
library(biglm)
library(tidyverse)

setwd('/home/ubuntu/tweets')

all <- fread('all.csv')

#First some simple models with no fixed effects for various income quantiles
all$income_percap_q <- cut2(all$income_percap, g=6)
all$statecounty <- factor(paste0(all$state, all$county))
all$date <- factor(substr(all$tweet_created_at, 1, 10))


#Would be great to have fixed effects!
mod <- bigglm(hedono ~ temp.hi*income_percap_q + statecounty + date, data=all[all$temp.hi > 20, ])

#THIS DIDNT WORK ON A MASSIVE SERVER AFTER SEVERAL DAYS
# TRY STOCHACSTIC GRADIENT DESCENT: https://stats.stackexchange.com/questions/263429/how-to-run-linear-regression-in-a-parallel-distributed-way-for-big-data-setting
