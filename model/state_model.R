library(data.table)
library(tidyverse)
library(fixest)
library(broom)
library(sf)
library(spData)

data <- fread('~/tweets/all.csv')

data$state <- substr(data$statemonth, 1, 2)
data$month <- substr(data$statemonth, 4, 5)

qs <- quantile(data$income_percap, seq(0, 1, by=0.05))
race_qs <- quantile(data$race_white, seq(0, 1, by=0.05))

data$income_percap <- log(data$income_percap)

data <- data[weather_term == 0, ]

data$raining <- data$prcp > 0

states <- unique(data$state)

##################################
# Just effects of Temperature
################################
alldat <- data.frame()
for (s in states){
  print(s)
  print(which(states == s))

  sel <- data[data$state == s & data$wbgt > 5, ]

  mod <- feols(vader ~ wbgt + raining + srad | dow + doy + tod + fips + year + month, data=sel)

  alldat <- bind_rows(alldat, 
                      tidy(mod) %>%
                        mutate(state = s))
}


alldat$GEOID <- alldat$state  

m <- merge(us_states, alldat %>% filter(term == 'wbgt'))

ggplot(m) + 
  geom_sf(aes(fill=estimate, color=p.value < 0.01)) + 
  scale_fill_gradient2(low='#ca0020', mid='#f7f7f7', high='#92c5de', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  theme_void() + 
  labs(main='Effect of Temperature on Twitter Sentiment', 
       fill='Estimate', color='Significant (p < 0.01)')
ggsave('~/temp-sentiment/res/map_wbgt.png')


#####################################
# Now effects of temp*income
#######################################

alldat2 <- data.frame()
for (s in states){
  print(s)
  print(which(states == s))

  sel <- data[data$state == s & data$wbgt > 5, ]

  mod <- feols(vader ~ wbgt*income_percap + raining*income_percap + srad*income_percap | dow + doy + tod + fips + year + month, data=sel)

  alldat2 <- bind_rows(alldat2, 
                      tidy(mod) %>%
                        mutate(state = s))
}

alldat3 <- alldat2 %>%
  select(term, estimate, state) %>%
  spread(term, estimate)

alldat3$tmp05q05 <- alldat3$`wbgt:income_percap`*qs[2]*5 + alldat3$wbgt*5
alldat3$tmp05q95 <- alldat3$`wbgt:income_percap`*qs[20]*5 + alldat3$wbgt*5
alldat3$tmp30q05 <- alldat3$`wbgt:income_percap`*qs[2]*30 + alldat3$wbgt*30
alldat3$tmp30q95 <- alldat3$`wbgt:income_percap`*qs[20]*30 + alldat3$wbgt*30

alldat3$change05 <- alldat3$tmp30q05 - alldat3$tmp05q05
alldat3$change95 <- alldat3$tmp30q95 - alldat3$tmp05q95

alldat3$changediff <- alldat3$change95 - alldat3$change05

alldat3$GEOID <- alldat3$state  

m <- merge(us_states, alldat3)

ggplot(m) + 
  geom_sf(aes(fill=changediff)) + 
  scale_fill_gradient2(low='#0571b0', mid='#f7f7f7', high='#ca0020', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  theme_void() + 
  labs(title='Difference in Sentiment From 5C to 30C (WBGT), income per capita at 95th percentile vs 5th Percentile',
       fill='Estimate')
ggsave('~/temp-sentiment/res/map_wbgt_income.png')



#####################################
# Now effects of temp*minority
#######################################

alldat4 <- data.frame()
for (s in states){
  print(s)
  print(which(states == s))

  sel <- data[data$state == s & data$wbgt > 5, ]

  mod <- feols(vader ~ wbgt*race_white + raining*race_white + srad*race_white | dow + doy + tod + fips + year + month, data=sel)

  alldat4 <- bind_rows(alldat4, 
                      tidy(mod) %>%
                        mutate(state = s))
}

alldat5 <- alldat4 %>%
  select(term, estimate, state) %>%
  spread(term, estimate)

alldat5$tmp05q05 <- alldat5$`wbgt:race_white`*race_qs[2]*5 + alldat5$wbgt*5
alldat5$tmp05q95 <- alldat5$`wbgt:race_white`*race_qs[20]*5 + alldat5$wbgt*5
alldat5$tmp30q05 <- alldat5$`wbgt:race_white`*race_qs[2]*30 + alldat5$wbgt*30
alldat5$tmp30q95 <- alldat5$`wbgt:race_white`*race_qs[20]*30 + alldat5$wbgt*30

alldat5$change05 <- alldat5$tmp30q05 - alldat5$tmp05q05
alldat5$change95 <- alldat5$tmp30q95 - alldat5$tmp05q95

alldat5$changediff <- alldat5$change95 - alldat5$change05

alldat5$GEOID <- alldat5$state  

m <- merge(us_states, alldat5)

ggplot(m) + 
  geom_sf(aes(fill=changediff)) + 
  scale_fill_gradient2(low='#0571b0', mid='#f7f7f7', high='#ca0020', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  theme_void() + 
  labs(title='Difference in Sentiment From 5C to 30C (WBGT), percent white at 95th percentile vs 5th Percentile',
       fill='Estimate')
ggsave('~/temp-sentiment/res/map_wbgt_race.png')

##########################################################


#  TEMP



##########################################################3


##################################
# Just effects of Temperature
################################
alltmp <- data.frame()
for (s in states){
  print(s)
  print(which(states == s))

  sel <- data[data$state == s & data$temp > 5, ]

  mod <- feols(vader ~ temp + raining + srad | dow + doy + tod + fips + year + month, data=sel)

  alltmp <- bind_rows(alltmp, 
                      tidy(mod) %>%
                        mutate(state = s))
}


alltmp$GEOID <- alltmp$state  

m <- merge(us_states, alltmp %>% filter(term == 'temp'))

ggplot(m) + 
  geom_sf(aes(fill=estimate, color=p.value < 0.01)) + 
  scale_fill_gradient2(low='#ca0020', mid='#f7f7f7', high='#92c5de', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  theme_void() + 
  labs(main='Effect of Temperature on Twitter Sentiment', 
       fill='Estimate', color='Significant (p < 0.01)')
ggsave('~/temp-sentiment/res/map_temp.png')


#####################################
# Now effects of temp*income
#######################################

alltmp2 <- data.frame()
for (s in states){
  print(s)
  print(which(states == s))

  sel <- data[data$state == s & data$temp > 5, ]

  mod <- feols(vader ~ temp*income_percap + raining*income_percap + srad*income_percap | dow + doy + tod + fips + year + month, data=sel)

  alltmp2 <- bind_rows(alltmp2, 
                      tidy(mod) %>%
                        mutate(state = s))
}

alltmp3 <- alltmp2 %>%
  select(term, estimate, state) %>%
  spread(term, estimate)

alltmp3$tmp05q05 <- alltmp3$`temp:income_percap`*qs[2]*5 + alltmp3$temp*5
alltmp3$tmp05q95 <- alltmp3$`temp:income_percap`*qs[20]*5 + alltmp3$temp*5
alltmp3$tmp30q05 <- alltmp3$`temp:income_percap`*qs[2]*30 + alltmp3$temp*30
alltmp3$tmp30q95 <- alltmp3$`temp:income_percap`*qs[20]*30 + alltmp3$temp*30

alltmp3$change05 <- alltmp3$tmp30q05 - alltmp3$tmp05q05
alltmp3$change95 <- alltmp3$tmp30q95 - alltmp3$tmp05q95

alltmp3$changediff <- alltmp3$change95 - alltmp3$change05

alltmp3$GEOID <- alltmp3$state  

m <- merge(us_states, alltmp3)

ggplot(m) + 
  geom_sf(aes(fill=changediff)) + 
  scale_fill_gradient2(low='#0571b0', mid='#f7f7f7', high='#ca0020', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  theme_void() + 
  labs(title='Difference in Sentiment From 5C to 30C, income per capita at 95th percentile vs 5th Percentile',
       fill='Estimate')
ggsave('~/temp-sentiment/res/map_temp_income.png')



#####################################
# Now effects of temp*minority
#######################################

alltmp4 <- data.frame()
for (s in states){
  print(s)
  print(which(states == s))

  sel <- data[data$state == s & data$temp > 5, ]

  mod <- feols(vader ~ temp*race_white + raining*race_white + srad*race_white | dow + doy + tod + fips + year + month, data=sel)

  alltmp4 <- bind_rows(alltmp4, 
                      tidy(mod) %>%
                        mutate(state = s))
}

alltmp5 <- alltmp4 %>%
  select(term, estimate, state) %>%
  spread(term, estimate)

alltmp5$tmp05q05 <- alltmp5$`temp:race_white`*race_qs[2]*5 + alltmp5$temp*5
alltmp5$tmp05q95 <- alltmp5$`temp:race_white`*race_qs[20]*5 + alltmp5$temp*5
alltmp5$tmp30q05 <- alltmp5$`temp:race_white`*race_qs[2]*30 + alltmp5$temp*30
alltmp5$tmp30q95 <- alltmp5$`temp:race_white`*race_qs[20]*30 + alltmp5$temp*30

alltmp5$change05 <- alltmp5$tmp30q05 - alltmp5$tmp05q05
alltmp5$change95 <- alltmp5$tmp30q95 - alltmp5$tmp05q95

alltmp5$changediff <- alltmp5$change95 - alltmp5$change05

alltmp5$GEOID <- alltmp5$state  

m <- merge(us_states, alltmp5)

ggplot(m) + 
  geom_sf(aes(fill=changediff)) + 
  scale_fill_gradient2(low='#0571b0', mid='#f7f7f7', high='#ca0020', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  theme_void() + 
  labs(title='Difference in Sentiment From 5C to 30C, percent white at 95th percentile vs 5th Percentile',
       fill='Estimate')
ggsave('~/temp-sentiment/res/map_temp_race.png')

















