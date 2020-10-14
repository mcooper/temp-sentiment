# Get results from wealth/census data, which has state and county FIPS
# Calculate local time and derive variables like doy, dow, etc

library(tidyverse)
library(lubridate)
library(countytimezones)
library(foreach)
library(doParallel)

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

setwd('~/tweets/wealth/results/')

fs <- list.files('.', pattern='.csv$')

foreach(f=fs, .packages=c('tidyverse', 'lubridate', 'countytimezones')) %dopar%{
  print(f)

  d <- read.csv(f) %>%
    mutate(fips = paste0(substr(100 + FIPS_STATE, 2, 3), 
                         substr(1000 + FIPS_COUNTY, 2, 4))) %>%
    select(fips, id, tweet_created_at) %>%
    merge(county_tzs %>%
            mutate(fips = substr(100000 + fips, 2, 6)) %>%
            select(fips, tz),
          all.x=T, all.y=F)

  d$tweet_created_at_local <- mapply(FUN=format,
                                     x=ymd_hms(d$tweet_created_at),
                                     tz=d$tz,
                                     usetz=T)

  #Get date of week
  d$date <- as.Date(d$tweet_created_at_local)
  d$dow <- weekdays(d$date)
  d$doy <- substr(d$tweet_created_at_local, 6, 10)
  d$tod <- substr(d$tweet_created_at_local, 12, 15)
  d$daynum <- as.numeric(d$date - as.Date('2009-01-01'))
  d$year <- year(d$date)

  d$date <- NULL
  d$tz <- NULL

  write.csv(d, paste0('~/tweets/localtime/', f), row.names=F)
}

system('~/telegram.sh "done"')



