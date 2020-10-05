library(tidyverse)
library(stringdist)
library(lubridate)
library(suncalc)
library(weathermetrics)
library(foreach)
library(doParallel)

options(stringsAsFactors=F)

getRH <- function(t, vpd){
  #https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit 
  #The second answer worked better
  #"From Dennis Hartman "Global Physical Climatology" (p 350)"
  
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + t)))
  rh <- 100 - 100*(vpd/es)
  rh
}

daytemp <- function(h, tmax, tmin, rise, dl){
  t <- ifelse(h >= rise, h - rise, (24 - rise) + h)
  (tmax - tmin) * sin((pi*t)/(dl + 4)) + tmin
}

nighttemp <- function(h, tmax, tmin, set, dl, tset){
  t <- ifelse(h > set, h - set, (24 - set) + h) + 1
  tset - ((tset - tmin)/log(24 - dl)) * log(t)
}

getTempHour <- function(lat, lon, tmax, tmin, time){
  #Estimate hourly tempartures from max and min temps
  #Using methods from: 
  #   Calculating Chilling Hours and Chill Units from Daily Maximum and Minimum Temperature Observations
  #   Linville 1990, Horticultural Science
  #
  #   THIS WHOLE THING IS IN UTC!!

  times = getSunlightTimes(date=ymd(substr(time, 1, 10)), lat=lat, lon=lon, keep=c('sunrise', 'solarNoon', 'sunset'))

  hour <- hour(ymd_hms(time))

  rise = hour(times$sunrise) #Min time is sunrise
  set = hour(times$sunset)

  dl <- ifelse(set > rise, set - rise, 24 + (set - rise))

  #Determine night hours based on sunrise and sunset times
  night <- (0:23)[if (set > rise) (0:23 < rise | 0:23 > set) else (0:23 > set & 0:23 < rise)]

  #if tweet was during day
  if (!hour %in% night){
    t <- daytemp(hour, tmax, tmin, rise, dl)
  } else{
  #if tweet was at night, get sunset temp to get night temps
    td <- daytemp(set, tmax, tmin, rise, dl)
    t <- nighttemp(hour, tmax, tmin, set, dl, td)
  }
  return(t)
}

tempfs <- list.files('~/tweets/temps', pattern='.csv')
donefs <- list.files('~/tweets/hourly', pattern='.csv')

fs <- tempfs[!tempfs %in% donefs]

cl <- makeCluster(64, outfile = '')
registerDoParallel(cl)

foreach(f=fs, .packages=c('tidyverse', 'lubridate', 'suncalc', 'weathermetrics', 'stringdist')) %dopar% {
  tryCatch({
    options(stringsAsFactors=F)

    date <- substr(f, 1, 10)
    print(paste0(f))

    #Bizarrely, it seems like the id_strs changed on going through GEE?
    #But only at the end?
    #So, match on tweet_created_at, and the closest match for id_str
    temps <- read.csv(paste0('~/tweets/temps/', f)) %>%
      filter(!is.na(id)) %>%
      unique

    comb <- read.csv(paste0('~/tweets/tweets/', f)) %>%
      filter(tweet_created_at %in% temps$tweet_created_at) %>%
      unique

    print(paste0(date, 'Adding raw temp data'))
    for (i in 1:nrow(comb)){
      sel <- temps %>% filter(tweet_created_at == comb$tweet_created_at[i])

      if (nrow(sel) == 0){
        next
      } else if (nrow(sel) == 1){
        j <- 1
      } else {
        j <- which.min(stringdist(sel$id, comb$id[i]))
      }

      comb$ppt[i] <- sel$ppt[j]
      comb$tmax[i] <- sel$tmax[j]
      comb$tmin[i] <- sel$tmin[j]
      comb$vpdmax[i] <- sel$vpdmax[j]
      comb$vpdmin[i] <- sel$vpdmin[j]
    }

    print(paste0(date, 'Calculating RH max'))
    comb$rhmax <- getRH(t=comb$tmax, vpd=comb$vpdmax)
    comb$rhmin <- getRH(t=comb$tmin, vpd=comb$vpdmin)

    print(paste0(date, 'Calculating Heat Index'))
    comb$tmax.hi <- heat.index(t=comb$tmax, rh=comb$rhmax, temperature.metric='celsius', round=2) 
    comb$tmin.hi <- heat.index(t=comb$tmin, rh=comb$rhmin, temperature.metric='celsius', round=2) 

    print(paste0(date, 'Calculating Hourly Values'))
    comb$temp <- mapply(FUN=getTempHour, lat=comb$lat, lon=comb$lon, tmax=comb$tmax,
                        tmin=comb$tmin, time=comb$tweet_created_at)

    comb$temp.hi <- mapply(FUN=getTempHour, lat=comb$lat, lon=comb$lon, tmax=comb$tmax.hi,
                           tmin=comb$tmin.hi, time=comb$tweet_created_at)

    #Save all linking and biophysical variables
    print(paste0(date, 'Writing'))
    comb %>%
      select(id, tweet_created_at, ppt, temp, temp.hi, tmax.hi) %>%
      write.csv(paste0('~/tweets/hourly/', f), row.names=F)
    
    print(paste0("END: ", f))
  },
  error=function(e){
    print(f)
    print(e)
  })
}

system('~/telegram.sh "Done with Temps"')
