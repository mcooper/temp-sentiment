library(tidyverse)

setwd('~/tweets/mod-res/')

MOD_RUN <- 'weather_inc5'
cat <- 'income_percap_q'

#############################
# Plot Predictions
##########################
pred <- read.csv(paste0(MOD_RUN, '_preds.csv'))
pred$category <- pred[ , cat]

pred$category <- as.factor(pred$category)

#levels(pred$category) <- c('Less Gray', 'Medium Gray', 'Very Gray')


######### Precip #########
precip <- pred %>%
  filter(srad == 0, temp.hi == 0)
ggplot(precip) + 
  geom_line(aes(x=log(precip + 1), y=predicted, color=category)) + 
  scale_x_continuous(label=function(x){round(exp(x) - 1, 2)}) + 
  labs(x='mm of precip (log scale)',
       y='sentiment score')
ggsave(paste0(MOD_RUN, '_precip.png'))

########## Temp.hi #########
temp.hi <- pred %>%
  filter(srad == 0, precip == 0)
ggplot(temp.hi) + 
  geom_line(aes(x=temp.hi, y=predicted, color=category)) + 
  labs(x='heat index temperature (C)',
       y='sentiment score')
ggsave(paste0(MOD_RUN, '_temp.hi.png'))

########## Srad #########
srad <- pred %>%
  filter(temp.hi == 0, precip == 0)
ggplot(srad) + 
  geom_line(aes(x=srad, y=predicted, color=category)) + 
  labs(x='sunlight (shortwave radiation - w/m^2)',
       y='sentiment score')
ggsave(paste0(MOD_RUN, '_srad.png'))
