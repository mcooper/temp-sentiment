library(data.table)
library(tidyverse)

setwd('~/tweets/')

data <- fread('all.csv')
data$month <- substr(data$statemonth, 4, 5)
data$state <- substr(100000 + data$fips, 2, 3)

# Export from https://wonder.cdc.gov/ucd-icd10.html
# 1. Group Results by Month and Weekday
# 2. Lower 48 states at all categories of urbanization
# 3. All ages, genders, origins, races
# 4. Years 2009-2019
# 5. All weekdays, all values, all places
# 6. Injry Intent and Mechanism: Injury Intent: Suicide, Mechanism: All Causes of Death
sui <- read.delim('suicide/Suicides by Day of Week-Month.txt', nrows=1056) %>%
  filter(Weekday != 'Unknown') %>%
  mutate(month = substr(Month, 1, 3),
         year = as.numeric(substr(Month.Code, 1, 4)),
         suicides = as.numeric(Deaths)) %>%
  select(month, year, dow=Weekday, suicides)

rescale <- function(x){
  x <- x - min(x)
  x <- x/max(x)
  x
}

# Day of Week
dow_df <- data[ , .(vader = mean(vader)), .(dow)]

m <- merge(dow_df, sui %>%
                     group_by(dow) %>%
                     summarize(suicides=sum(suicides))) %>%
  mutate(suicides = rescale(suicides),
         vader = rescale(vader),
         vader = 1 - vader) %>%
  gather(key, val, -dow)
m$dow <- factor(m$dow, levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                'Thursday', 'Friday', 'Saturday'))

ggplot(m) + 
  geom_line(aes(x=dow, y=val, color=key, group=key))

# Month of Year
moy_df <- data[ , .(vader = mean(vader)), .(month)]
moy_df$month <- month.abb[as.numeric(moy_df$month)]

m <- merge(moy_df, sui %>%
                     group_by(month) %>%
                     summarize(suicides=sum(suicides))) %>%
  mutate(suicides = rescale(suicides),
         vader = rescale(vader),
         vader = 1 - vader) %>%
  gather(key, val, -month)

m$month <- factor(m$month, levels=month.abb)

ggplot(m) + 
  geom_line(aes(x=month, y=val, color=key, group=key))

# State
sui <- read.delim('suicide/Suicides by State.txt', nrows=49) %>%
  mutate(suicides = as.numeric(Crude.Rate),
         state = as.numeric(State.Code)) %>%
  select(state, suicides)

state_df <- data[ , .(vader = mean(vader)), .(state)]
state_df$state <- as.numeric(state_df$state)

m <- merge(state_df, sui)
plot(m$vader, m$suicides)
summary(lm(suicides ~ vader, data=m))






