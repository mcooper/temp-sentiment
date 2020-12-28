library(data.table)

setwd('/home/ubuntu/tweets')

setDTthreads(48)

#To combine the daily csvs, I ran
# cat sentiment/*.csv | grep -a -v tweet_created_at > sentiment_all.csv

#Get Sentiment Data
sen <- fread('sentiment_all.csv',
             col.names=c('id', 'tweet_created_at', 'weather_term', 
                         'afinn', # 'textblob', 
                         'hedono', 'vader'), # 'swn', 'wkwsci'),
             drop=c(5, 8, 9))
sen <- unique(sen, by=c('id', 'tweet_created_at'))

#Get Climate Data
cli <- fread('hourly_nldas2_all.csv', 
             col.names=c('temp', 'id', 'tweet_created_at', # 'speh', 'pres', 
                         'prcp', 'srad', 
                         # 'lrad', 'wndu', 'wndv', 'ws', 'rh', 
                         'wbgt', 'temp.hi'),
             drop=c(4, 5, 8, 9, 10, 11, 12))
cli <- unique(cli, by=c('id', 'tweet_created_at'))

#Get Census Data
cen <- fread('census_all.csv',
             col.names=c('id', 'tweet_created_at', 'income_percap', 
                         # 'lat', 'lon', # 'state',
                         # 'county', 
                         'race_white', 'race_black', 
                         'race_other', 'race_hisp'),
             drop=c(4, 5, 6, 7))
cen[ , race_majority := names(.SD)[max.col(.SD)], .SDcols= 4:7]
#cen[ , race_white:=NULL]
cen[ , race_black:=NULL]
cen[ , race_other:=NULL]
cen[ , race_hisp:=NULL]
cen <- unique(cen, by=c('id', 'tweet_created_at'))

#Get Landcover Data
lcv <- fread('landcover_all.csv',
             col.names=c('id', 'tweet_created_at', 'tree', 'impervious'),
                         # 'landcover', 'id_str')
             drop=c(5),
             blank.lines.skip=TRUE)
lcv <- unique(lcv, by=c('id', 'tweet_created_at'))


#Get Local Time Data
lti <- fread('localtime_all.csv',
             col.names=c('fips', 'id', 'tweet_created_at', 
                         # 'tweet_created_at_local',
                         'dow', 'doy', 'tod',
                         'daynum', 'year'),
             drop=c(4))
lti <- unique(lti, by=c('id', 'tweet_created_at'))

setkeyv(sen, cols=c('id', 'tweet_created_at'))
setkeyv(cli, cols=c('id', 'tweet_created_at'))
setkeyv(cen, cols=c('id', 'tweet_created_at'))
setkeyv(lcv, cols=c('id', 'tweet_created_at'))
setkeyv(lti, cols=c('id', 'tweet_created_at'))

all <- merge(cli, sen, all.x=T, all.y=F)
all <- merge(all, cen, all.x=T, all.y=F)
all <- merge(all, lti, all.x=T, all.y=F)
all <- merge(all, lcv, all.x=T, all.y=F)

all <- na.omit(all)

rm(sen, cli, cen, lcv, lti)

all[ , id:=NULL]
all[ , tweet_created_at:=NULL]

### FILTER OUT BAD tod Variables!
### SOMEHOW TOD WAS Time Zones for Certain Observations - 
all <- all[!all$tod %in% c('CDT', 'CST', 'EDT', 'EST', 'MST', 'MDT', 'PDT', 'PST'), ]

#Make Statemonth Fixed Effect
all$statemonth <- paste0(substr(100000 + all$fips, 2, 3), '-', substr(all$doy, 1, 2))

fwrite(all, 'all.csv', row.names=F)
fwrite(all[sample(.N, .N*0.01)], 'all_samp_1pct.csv', row.names=F)

system('~/telegram.sh "Donezo!"')
system('sudo poweroff')
