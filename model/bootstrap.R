library(ggplot2)
library(tidyverse)

set.seed(100)

n_sample <- 1000

truemod <- lm(price ~ depth, data=diamonds)

val90 <- NULL
se90 <- NULL
for (i in 1:n_sample){
  m <- lm(price ~ depth, data=diamonds[sample(1:nrow(diamonds), nrow(diamonds)*0.9), ])
  val90 <- c(val90, m$coefficients['depth'])
  se90 <- c(se90, summary(m)$coefficients['depth', 'Std. Error'])
}


val50 <- NULL
se50 <- NULL
for (i in 1:n_sample){
  m <- lm(price ~ depth, data=diamonds[sample(1:nrow(diamonds), nrow(diamonds)*0.5), ])
  val50 <- c(val50, m$coefficients['depth'])
  se50 <- c(se50, summary(m)$coefficients['depth', 'Std. Error'])
}

val10 <- NULL
se10 <- NULL
for (i in 1:n_sample){
  m <- lm(price ~ depth, data=diamonds[sample(1:nrow(diamonds), nrow(diamonds)*0.1), ])
  val10 <- c(val10, m$coefficients['depth'])
  se10 <- c(se10, summary(m)$coefficients['depth', 'Std. Error'])
}

plt <- bind_rows(data.frame(value=c(val90, se90), sample_percent='90',
                            parameter = rep(c('coefficient', 'standard error'), each=n_sample)),
                 data.frame(value=c(val50, se50), sample_percent='50',
                            parameter = rep(c('coefficient', 'standard error'), each=n_sample)),
                 data.frame(value=c(val10, se10), sample_percent='10',
                            parameter = rep(c('coefficient', 'standard error'), each=n_sample)))

true <- data.frame(value=c(truemod$coefficients['depth'], 
                           summary(truemod)$coefficients['depth', 'Std. Error']),
                   parameter = c('coefficient', 'standard error'))
                   
ggplot() + 
  geom_density(data=plt, aes(x=value, fill=sample_percent), alpha=0.5) + 
  geom_vline(data=true, aes(xintercept=value, color=parameter)) + 
  facet_wrap(parameter ~ ., scales="free")
ggsave('~/example.png')
