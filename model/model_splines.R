library(mgcv)
library(data.table)
library(Hmisc)
library(dplyr)
library(parallel)

setwd('/home/ubuntu/tweets')

all <- fread('all.csv')

#First some simple models with no fixed effects for various income quantiles
all$income_percap_q <- cut2(all$income_percap, g=5)

#Get date of week
all$date <- as.Date(all$tweet_created_at)
all$dow <- weekdays(all$date)
all$doy <- yday(all$date)
all$daynum <- as.numeric(all$date - as.Date('2009-01-01'))

cl <- makeCluster(32)

###############################
# Look at heat
################################
system('~/telegram.sh "Starting Temp"')
mod <- gam(hedono ~ temp.hi*income_percap_q + dow + 
           s(doy, bs='cc') + 
           s(lat, lon, bs='sos', k=500) + 
           s(daynum, bs='cr'), 
           data=all[all$temp.hi > 20, ],
           #cluster=cl,
           knots=list(doy=c(0.5, 366.5)))

system('~/telegram.sh "Done with Temp"')
save(mod, file='temp.hi-smooths.Rdata')
rm(mod)
gc()

###############################
# Look at cold 
################################
system('~/telegram.sh "Starting Cold"')
mod <- gam(hedono ~ temp*income_percap_q + dow + 
           s(doy, bs='cc') + 
           s(lat, lon, bs='sos', k=500) + 
           s(daynum, bs='cr'), 
           data=all[all$temp.hi < 20, ],
           #cluster=cl,
           knots=list(doy=c(0.5, 366.5)))

system('~/telegram.sh "Done with Cold"')
save(mod, file='temp.co-smooths.Rdata')
rm(mod)
gc()

###############################
# Look at rain 
################################
system('~/telegram.sh "Starting Precip"')
mod <- gam(hedono ~ ppt*income_percap_q + dow + 
           s(doy, bs='cc') + 
           s(lat, lon, bs='sos', k=500) + 
           s(daynum, bs='cr'), 
           data=,
           #cluster=cl,
           knots=list(doy=c(0.5, 366.5)))
system('~/telegram.sh "Done with Precip"')
save(mod, file='temp.co-smooths.Rdata')
rm(mod)
gc()
