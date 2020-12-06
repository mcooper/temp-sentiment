library(tidyverse)

setwd('~/tweets/mod-res/')

MOD_RUN <- 'impervcont_moreknots'
cat <- 'impervious'

#############################
# Plot Predictions
##########################
pred <- read.csv(paste0(MOD_RUN, '_preds.csv'))
pred$category <- factor(pred[ , cat])

######### Precip #########
precip <- pred %>%
  filter(srad == 0, temp.hi == 0)
ggplot(precip) + 
  geom_line(aes(x=log(precip + 1), y=predicted, color=category)) + 
  #scale_x_continuous(label=function(x){round(exp(x) - 1, 2)}) + 
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

#######################################
# Plot Coefficients
#######################################
coef <- read.csv(paste0(MOD_RUN, '_coefs.csv'))

########## Precip #########
precip_all <- coef[coef$names == 'precip', 'coefs']

precip_sel <- coef %>%
  filter(grepl('precip_q\\(', names))

precip_coefs <- data.frame(coef=precip_all, level=0)
for(i in 1:nrow(precip_sel)){
  precip_coefs <- bind_rows(precip_coefs, data.frame(coef=precip_sel$coefs[i] + precip_all, 
                                                     level=i))
}
ggplot(precip_coefs) + 
  geom_bar(aes(x=level, y=coef), stat='identity')

########## Temp.hi ########
temp.hi_all <- coef[coef$names == 'temp.hi', 'coefs']

temp.hi_sel <- coef %>%
  filter(grepl('temp.hi_q\\(', names))

temp.hi_coefs <- data.frame(coef=temp.hi_all, level=0)
for(i in 1:nrow(temp.hi_sel)){
  temp.hi_coefs <- bind_rows(temp.hi_coefs, data.frame(coef=temp.hi_sel$coefs[i] + temp.hi_all, 
                                                     level=i))
}
ggplot(temp.hi_coefs) + 
  geom_bar(aes(x=level, y=coef), stat='identity')

########## srad #########
srad_all <- coef[coef$names == 'srad', 'coefs']

srad_sel <- coef %>%
  filter(grepl('srad_q\\(', names))

srad_coefs <- data.frame(coef=srad_all, level=0)
for(i in 1:nrow(srad_sel)){
  srad_coefs <- bind_rows(srad_coefs, data.frame(coef=srad_sel$coefs[i] + srad_all, 
                                                     level=i))
}
ggplot(srad_coefs) + 
  geom_bar(aes(x=level, y=coef), stat='identity')



