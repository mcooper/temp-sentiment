library(mgcv)
library(data.table)
library(Hmisc)
library(dplyr)
library(parallel)

setwd('/home/ubuntu/tweets')

all <- fread('all.csv')

###############################
# Look at heat
################################
system('~/telegram.sh "Starting Temp"')
samp <- sample(which(all$temp.hi > 20), 10000000)
mod <- gam(hedono ~ temp.hi*income_percap_q + dow + 
           s(doy, bs='cc') + 
           s(lat, lon, bs='sos') + 
           s(daynum, bs='cr'), 
           data=all[samp, ],
           #cluster=cl,
           #control=list(nthreads=32),
           method='REML',
           knots=list(doy=c(0.5, 366.5)))
system('~/telegram.sh "Done with Temp"')
save(mod, file='heat_inco_smooth.Rdata')
rm(mod)
gc()

###############################
# Look at cold 
################################
system('~/telegram.sh "Starting Cold"')
samp <- sample(which(all$temp < 20), 1000000)
mod <- gam(hedono ~ temp*income_percap_q + dow + 
           s(doy, bs='cc') + 
           s(lat, lon, bs='sos', k=500) + 
           s(daynum, bs='cr'), 
           data=all[samp, ],
           #control=list(nthreads=32),
           method='REML',
           #cluster=cl,
           knots=list(doy=c(0.5, 366.5)))
system('~/telegram.sh "Done with Cold"')
save(mod, file='cold_inco_smooth.Rdata')
rm(mod)
gc()

###############################
# Look at rain 
################################
system('~/telegram.sh "Starting Precip"')
samp <- sample(1:nrow(all), 1000000)
mod <- gam(hedono ~ ppt*income_percap_q + dow + 
           s(doy, bs='cc') + 
           s(lat, lon, bs='sos', k=500) + 
           s(daynum, bs='cr'), 
           data=all,
           #cluster=cl,
           knots=list(doy=c(0.5, 366.5)))
           #control=list(nthreads=32),
           method='REML',
system('~/telegram.sh "Done with Precip"')
save(mod, file='rain_inco_smooth.Rdata')
rm(mod)
gc()
