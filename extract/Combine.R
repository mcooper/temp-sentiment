library(data.table)
library(countytimezones)
library(lubridate)

setwd('/home/ubuntu/tweets')

setDTthreads(64)

#To combine the daily csvs, I ran
# cat sentiment/*.csv | grep -v tweet_created_at > sentiment_all.csv

#Get Sentiment Data
sen <- fread('sentiment_all.csv',
             col.names=c('id', 'tweet_created_at', # 'weather_term', 
                        # 'afinn', 'textblob', 
                        'hedono'),
                        # 'vader', 'swn', 'wkwsci'),
             drop=c(3, 4, 5, 7, 8, 9))
sen <- unique(sen, by=c('id', 'tweet_created_at'))

#Get Climate Data
cli <- fread('hourly_all.csv', 
             col.names=c('id', 'tweet_created_at', 'ppt', 'temp', 
                        'temp.hi'), # 'tmax.hi'),
             drop=c(6))
cli <- unique(cli, by=c('id', 'tweet_created_at'))

#Get Census Data
cen <- fread('census_all.csv',
             col.names=c('id', 'tweet_created_at', 'income_percap', 
                         'lat', 'lon', # 'state',
                         # 'county', 
                         'race_white', 'race_black', 
                         'race_other', 'race_hisp'),
             drop=c(6, 7))
cen <- unique(cen, by=c('id', 'tweet_created_at'))

#Get Local Time Data
lti <- fread('localtime_all.csv',
             col.names=c('fips', 'id', 'tweet_created_at', 
                         'tweet_created_at_local', 'dow', 'doy', 'tod',
                         'daynum', 'year'))
lti <- unique(lti, by=c('id', 'tweet_created_at'))

setkeyv(sen, cols=c('id', 'tweet_created_at'))
setkeyv(cli, cols=c('id', 'tweet_created_at'))
setkeyv(cen, cols=c('id', 'tweet_created_at'))
setkeyv(lti, cols=c('id', 'tweet_created_at'))

all <- merge(cli, sen, all.x=T, all.y=F)
all <- merge(all, cen, all.x=T, all.y=F)
all <- merge(all, lti, all.x=T, all.y=F)

all <- na.omit(all)

#Get Income Quintiles, nationwide and by state
class <- c('Poorest', 'Poorer', 'Medium', 'Richer', 'Richest')
all$income_percap_q_nation <- class[as.numeric(Hmisc::cut2(all$income_percap, g=5))]
all$state <- substr(all$fips, 1, 2)
all$income_percap_q_state <- class[all[ , list(out = Hmisc::cut2(all$income_percap, g=5)), by='state']$out]

fwrite(all, 'all.csv', row.names=F)


