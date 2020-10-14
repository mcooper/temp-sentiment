library(data.table)
library(countytimezones)
library(lubridate)

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
             col.names=c('id', 'tweet_created_at', 'income_percap', 
                         'lat', 'lon', 'state',
                         'county', 'race_white', 'race_black', 
                         'race_other', 'race_hisp'),
             key=c('id', 'tweet_created_at'))
cen$id2 <- paste0(cen$id, cen$tweet_created_at)
cen <- cen[!duplicated(cen$id2), ]
cen[,id2:=NULL]


#Get Local Time Data
lti <- fread('localtime_all.csv',
             col.names=c('fips', 'id', 'tweet_created_at', 
                         'tweet_created_at_local', 'dow', 'doy', 'tod',
                         'daynum', 'year'),
             key=c('id', 'tweet_created_at'))
lti$id2 <- paste0(lti$id, lti$tweet_created_at)
lti <- lti[!duplicated(lti$id2), ]
lti[,id2:=NULL]

setkeyv(sen, cols=c('id', 'tweet_created_at'))
setkeyv(cli, cols=c('id', 'tweet_created_at'))
setkeyv(cen, cols=c('id', 'tweet_created_at'))
setkeyv(lti, cols=c('id', 'tweet_created_at'))

all <- merge(cli, sen, all.x=T, all.y=F)
all <- merge(all, cen, all.x=T, all.y=F)
all <- merge(all, lti, all.x=T, all.y=F)


#Get Income Quintiles, nationwide and by state
class <- c('Poorest', 'Poorer', 'Medium', 'Richer', 'Richest')
all$income_percap_q_nation <- class[as.numeric(Hmisc::cut2(all$income_percap, g=5))]
all$income_percap_q_state <- class[all[ , list(out = Hmisc::cut2(all$income_percap, g=5)), by='state']$out]

################### TODO #########################
# Remove unncessary and redundant columsn before writing

fwrite(all, 'all.csv', row.names=F)


