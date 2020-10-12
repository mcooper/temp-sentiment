library(tidyverse)
library(mgcv)
library(data.table)

setwd('~/tweets/')

load('heat_inco_smooth.Rdata')

df <- expand.grid(list(income_percap_q = factor(mod$xlevels$income_percap_q),
                       temp.hi = seq(20, 50)))
df$dow <- factor("Thursday", levels=mod$xlevels$dow)
df$daynum <- 500
df$doy <- 100
df$lat <- 42
df$lon <- 71

p <- predict(mod, df, se.fit=T)
df$fit <- p$fit
df$max <- p$fit + p$se.fit*2
df$min <- p$fit - p$se.fit*2

df <- df %>%
	select(income_percap_q, temp.hi, fit, max, min) %>%
  gather(var, val, -income_percap_q, -temp.hi)

ggplot(df) + 
  geom_line(aes(x=temp.hi, y=val, color=income_percap_q, linetype=var)) + 
  labs(main='Effect of Heat on Expressed Twitter Sentiment, by Income Group',
       x='Temperature (Heat Index)',
       y='Hedonometer Score') + 
  guides(linetype=F) + 
  scale_linetype_manual(values=c(min=3, max=3, fit=1))


