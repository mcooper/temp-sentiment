library(data.table)
library(Hmisc)
library(biglm)
library(tidyverse)

setwd('/home/ubuntu/tweets')

all <- fread('all.csv')

#First some simple models with no fixed effects for various income quantiles
all$income_percap_q <- cut2(all$income_percap, g=6)

###############################
# Look at heat
################################
mod <- lm(hedono ~ temp.hi*income_percap_q, data=all[all$temp.hi > 20, ])

df <- expand.grid(list(income_percap_q = levels(all$income_percap_q),
                       temp.hi = seq(20, 50)))
p <- predict(mod, df, se.fit=T)
df$fit <- p$fit
df$max <- p$fit + p$se.fit*2
df$min <- p$fit - p$se.fit*2

df <- df %>%
  gather(var, val, -income_percap_q, -temp.hi)

ggplot(df) + 
  geom_line(aes(x=temp.hi, y=val, color=income_percap_q, linetype=var)) + 
  labs(main='Effect of Heat on Expressed Twitter Sentiment, by Income Group',
       x='Temperature (Heat Index)',
       y='Hedonometer Score') + 
  guides(linetype=F) + 
  scale_linetype_manual(values=c(3, 3, 1))
ggsave('~/temp-sentiment/res/heat_income.png')

rm(mod)
gc()

###############################
# Look at cold 
################################
mod <- lm(hedono ~ temp*income_percap_q, data=all[all$temp < 20, ])

df <- expand.grid(list(income_percap_q = levels(all$income_percap_q),
                       temp = seq(-10, 20)))
p <- predict(mod, df, se.fit=T)
df$fit <- p$fit
df$max <- p$fit + p$se.fit*2
df$min <- p$fit - p$se.fit*2

df <- df %>%
  gather(var, val, -income_percap_q, -temp)

ggplot(df) + 
  geom_line(aes(x=temp, y=val, color=income_percap_q, linetype=var)) + 
  labs(main='Effect of Cold on Expressed Twitter Sentiment, by Income Group',
       x='Temperature',
       y='Hedonometer Score') + 
  #guides(linetype=F) + 
  scale_linetype_manual(values=c(1, 3, 3))
ggsave('~/temp-sentiment/res/cold_income.png')

rm(mod)
gc()

###############################
# Look at rain 
################################
mod <- lm(hedono ~ ppt*income_percap_q, data=all)

df <- expand.grid(list(income_percap_q = levels(all$income_percap_q),
                       ppt = seq(0, 50)))
p <- predict(mod, df, se.fit=T)
df$fit <- p$fit
df$max <- p$fit + p$se.fit*2
df$min <- p$fit - p$se.fit*2

df <- df %>%
  gather(var, val, -income_percap_q, -ppt)

ggplot(df) + 
  geom_line(aes(x=ppt, y=val, color=income_percap_q, linetype=var)) + 
  labs(main='Effect of Precipitation on Expressed Twitter Sentiment, by Income Group',
       x='Temperature',
       y='Hedonometer Score') + 
  guides(linetype=F) + 
  scale_linetype_manual(values=c(1, 3, 3))
ggsave('~/temp-sentiment/res/rain_income.png')
