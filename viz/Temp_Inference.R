library(tidyverse)
library(lubridate)
library(suncalc)
library(weathermetrics)
library(hms)

options(stringsAsFactors=F)

##################################
# Read in Temp Calc Functions
#####################################
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


df <- data.frame(time=seq(ymd_hms('2020-10-12 00:00:01'), ymd_hms('2020-10-12 23:00:01'), length.out=24))

df$temp <- mapply(FUN=getTempHour, lat=41, lon=-71, tmax=15, tmin=5, time=df$time)

df$const <- 'Hourly Temp'

df$time <- df$time - hours(4)

ggplot() + 
  geom_line(data=df, aes(x=hour(time), y=temp, linetype=const)) + 
  geom_hline(aes(yintercept=c(5, 15),
                 color=c('#ef8a62', '#67a9cf')),
             linetype=1) + 
  labs(y='Temperature (C)',
       x='Time of Day') + 
  scale_x_continuous(labels=function(x){paste0(x, ':00')},
                     expand=c(0,0)) + 
  scale_linetype_manual(values=c(`Hourly Temp`=2)) +
  scale_color_discrete(labels=c('Min Temp', 'Max Temp')) + 
  labs(linetype="Estimated",
       color="Known") + 
  theme_bw()
ggsave('~/temp-sentiment/res/Estimating_Hours.png', height=4, width=7)

