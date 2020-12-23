# First run:
# cd ~/tweets/hourly_nldas2
# cat *.csv | grep -a -v tweet_created_at > ../nldas2.csv

library(data.table)
library(weathermetrics)

setwd('~/tweets/')

dt <- fread("nldas2.csv", col.names=c('temp', 'id', 'tweet_created_at', 'speh', 'pres', 
                                      'prcp', 'srad', 'lrad', 'wndu', 'wndv'))

#Somehow, bizarrely, we have a lot of duplicates
dt <- unique(dt)
dt <- na.omit(dt)

get_ws <- function(wndu, wndv){
  return(sqrt(wndu^2 + wndv^2))
}

get_rh <- function(speh, temp, pres){
  #Adapted from https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity
  pres = pres * 0.01
  es = 6.112 * exp((17.67*temp)/(temp + 243.5))
  e = speh*pres / (0.378 * speh + 0.622)
  rh = e/es
  if (rh > 1){
    return(1)
  }
  if (rh < 0){
    return(0)
  }
  return(rh)
}

get_wbgt <- function(temp, rh, srad, ws){
  0.735*temp + 0.0374*rh + 0.00292*temp*rh + 7.619*srad - 4.557*srad^2 - 0.0572*ws -4.064
}

dt$ws <- get_ws(dt$wndu, dt$wndv)
dt$rh <- mapply(get_rh, speh=dt$speh, temp=dt$temp, pres=dt$pres)
dt$temp <- dt$temp - 273.15
dt$wbgt <- mapply(get_wbgt, temp = dt$temp, rh = dt$rh*100, 
                    srad = dt$srad/1000, ws = dt$ws)
dt$temp.hi <- heat.index(dt$temp, rh=dt$rh*100, temperature.metric='celsius')

 <- dt[ , c('id', 'tweet_created_at', 'temp', 'prcp', 'srad', 'wbgt', 'temp.hi')]

fwrite(dt, 'hourly_nldas2_all.csv')

system('~/telegram.sh "Done with temp processing"')




