setwd('~/tweets/')

library(tidyverse)
library(sf)
library(sp)
library(rnaturalearth)
library(viridis)
library(raster)
library(data.table)
library(lubridate)

options(scipen = 100)

all <- fread('all.csv')

sel <- all[sample(.N, nrow(all)/500)]

###############################
#Histogram of Temperatures
###############################
ggplot(sel) +
  geom_histogram(aes(x=temp.hi), bins=100, fill='#BBBBBB', color='#222222') + 
  theme_bw() +
  scale_y_continuous(expand=expand_scale(mult=c(0, 0.05)), labels=function(x){x*500}) + 
  labs(title='Distribution of Observed Heat Index Temperatures',
       x='Heat Index Temperature (Celsius)',
       y='Count of Tweets')
ggsave('~/temp-sentiment/res/Temperature_Histogram.png', width = 6, height=4.875)


###############################
#Histogram of Sentiment
###############################
ggplot(sel) +
  geom_(aes(x=hedono), bins=100, fill='#BBBBBB', color='#222222') + 
  theme_bw() +
  scale_y_continuous(expand=expand_scale(mult=c(0, 0.05)), labels=function(x){x*500}) + 
  labs(title='Distribution of Observed Hedonometer Scores',
       x='Hedonometer Score',
       y='Count of Tweets')
ggsave('../temp-sentiment/res/Sentiment_Histogram.png', width = 6, height=4.875)

##############################
#Timeline of Tweets
#############################
ggplot(sel) + 
  geom_bar(aes(x=ymd(substr(tweet_created_at, 1, 10)), y=..count..), 
           fill='#BBBBBB', color='#222222') + 
  theme_bw() + 
  scale_y_continuous(expand=expand_scale(mult=c(0, 0.05)), labels=function(x){x*500}) + 
  labs(title='Number of Tweets Over Time',
       x='Date',
       y='Count of Tweets')
ggsave('../temp-sentiment/res/TimeCount.png', width = 9, height=3.875)


###############################
#Map of Tweets
###############################

rm(all);rm(sel);gc()

all <- fread('all.csv')

#Note lat and long are swapped in this dataset!
cty <- ne_states()
cty_c <- crop(cty, extent(c(min(sel$lat), max(sel$lat), min(sel$lng), max(sel$lng))))
cty_sf <- st_as_sf(cty_c)

ggplot() +  
  stat_bin_hex(data = sel,
               aes(x = lat, y = lng, fill=log(..count..*500)), binwidth=0.25) +
  coord_sf(clip='on') + 
  scale_fill_viridis(option = 'inferno', 
                     #breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000)),
                     labels= c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
  geom_sf(data=cty_sf, color='white', fill=NA) +
  theme_void() + 
  labs(fill='Count of Tweets')

ggsave('../code/temp-sentiment/viz/Tweet_Map.png', width=4.875, height=6)
