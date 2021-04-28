library(data.table)

setwd('~/tweets/')

data <- fread('all.csv')

# Day of Week, Time of Day
dow_tod <- data[ , .(vader = mean(vader)), .(dow, tod)]

dow_tod[dow_tod$vader %in% c(max(dow_tod$vader), min(dow_tod$vader)), ]

        dow  tod      vader
1: Thursday 04:0 0.07818882
2:   Sunday 08:0 0.21021752

# Day of week
dow_df <- data[ , .(vader = mean(vader)), .(dow)]

dow_df[dow_df$vader %in% c(max(dow_df$vader), min(dow_df$vader)), ]

         dow     vader
1:    Sunday 0.1366292
2:  Saturday 0.1371815
3:    Friday 0.1338290
4:  Thursday 0.1305909
5: Wednesday 0.1294679
6:   Tuesday 0.1267888
7:    Monday 0.1269388

# Day of Year
doy_df <- data[ , .(vader = mean(vader)), .(doy)]
doy_df <- doy_df[doy_df$doy != '02-29', ]

doy_df[doy_df$vader %in% c(max(doy_df$vader), min(doy_df$vader)), ]

     doy     vader
1: 12-25 0.2080047
2: 12-14 0.1201150


