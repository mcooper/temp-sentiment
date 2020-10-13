library(data.table)

setwd('/home/ubuntu/tweets')

#To combine the daily csvs, I ran
# cat sentiment/*.csv | grep -v tweet_created_at > sentiment_all.csv

#Get Sentiment Data
sen <- fread('sentiment_all.csv',
             col.names=c('id', 'tweet_created_at', 'weather_term', 
                        'afinn', 'textblob', 'hedono', 
                        'vader', 'swn', 'wkwsci'),
             drop=c('afinn', 'textblob', 'vader', 'swn', 'wkwsci'),
             key=c('id', 'tweet_created_at'))
sen$id2 <- paste0(sen$id, sen$tweet_created_at)
sen <- sen[!duplicated(sen$id2), ]
sen[,id2:=NULL]

#Get Climate Data
cli <- fread('hourly_all.csv', 
             col.names=c('id', 'tweet_created_at', 'ppt', 'temp', 
                        'temp.hi', 'tmax.hi'),
             key=c('id', 'tweet_created_at'))
cli$id2 <- paste0(cli$id, cli$tweet_created_at)
cli <- cli[!duplicated(cli$id2), ]
cli[,id2:=NULL]

#Get Census Data
cen <- fread('census_all.csv',
             col.names=c('id', 'tweet_created_at', 'income_percap', 'lat', 'lon', 'state',
                         'county', 'race_white', 'race_black', 'race_other', 'race_hisp'),
             key=c('id', 'tweet_created_at'))
cen$id2 <- paste0(cen$id, cen$tweet_created_at)
cen <- cen[!duplicated(cen$id2), ]
cen[,id2:=NULL]

setkeyv(sen, cols=c('id', 'tweet_created_at'))
setkeyv(cli, cols=c('id', 'tweet_created_at'))
setkeyv(cen, cols=c('id', 'tweet_created_at'))

all <- merge(cli, sen, all.x=T, all.y=F)
all <- merge(all, cen, all.x=T, all.y=F)

#First some simple models with no fixed effects for various income quantiles
all$income_percap_q <- Hmisc::cut2(all$income_percap, g=5)

#Get date of week
all$date <- as.Date(all$tweet_created_at)
all$dow <- weekdays(all$date)
all$doy <- yday(all$date)
all$daynum <- as.numeric(all$date - as.Date('2009-01-01'))
all$FIPS <- paste0(substr(100 + all$state, 2, 3), substr(1000 + all$county, 2, 4))
all$month <- substr(all$tweet_created_at, 6, 7)

all[ , county:=NULL]
all[ , state:=NULL]

fwrite(all, 'all.csv', row.names=F)

sum <- all[ , list(ppt=mean(ppt), 
                    temp=mean(temp), 
                    temp.hi=mean(temp.hi),
                    tmax.hi=mean(tmax.hi),
                    hedono=mean(hedono),
                    n=.N), 
              by=list(dow, doy, daynum, income_percap_q, income_percap, state, county,
                      race_white, race_black, race_other, race_hisp)]

fwrite(sum, 'summarized.csv', row.names=F)

