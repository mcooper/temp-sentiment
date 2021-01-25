library(data.table)
library(tidyverse)

setwd('~/tweets/')

sui <- fread('suicide/Suicides.csv') %>%
  mutate(month = substr(Month, 6, 7)) %>%
  select(fips=County, year=Year, month, Population, Deaths) %>%
  mutate(state = floor(fips/1000))
data <- fread('all.csv')

data$month <- substr(data$doy, 1, 2)

sum <- data[ , .(vader=mean(vader)), .(fips, month, year)]

sui$rate <- sui$Deaths/sui$Population

m <- merge(sui, sum, all.x=T, all.y=F)


mod <- glm(rate ~ vader + as.factor(year) + as.factor(month) + as.factor(state), data=m)

#In most specifications, increased sentiment is associated with decreased suicide
#Could be a real effect (but how?), could be due to mis-specification
