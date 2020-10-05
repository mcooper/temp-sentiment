library(ggplot2)
library(tidyverse)
library(stringdist)
library(lubridate)
library(suncalc)
library(weathermetrics)

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

times <- seq(ymd_hms('2020-07-15 00:00:01'), ymd_hms('2020-07-15 23:00:01'), by='hour')

df <- data.frame(lat=42.3555582, lon=-71.1329683, tmax=85, tmin=60, time=times)

df$temp <- mapply(FUN=getTempHour, lat=df$lat, lon=df$lon, tmax=df$tmax,
                        tmin=df$tmin, time=df$time)

df$time_local = df$time - hours(4)

ggplot() + 
	geom_line(data=df, aes(x=time_local, y=temp)) + 
  geom_hline(aes(yintercept=85)) + 
  geom_hline(aes(yintercept=60))


