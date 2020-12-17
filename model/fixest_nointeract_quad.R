#Use E32

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/')

data <- fread('all.csv')

data <- data[weather_term == 0, ]

formula <- paste0("vader ~ temp + temp^2 + precip + precip^2 + srad + srad^2", 
                     " | dow + doy + tod + fips + year + statemonth")

start <- Sys.time()
mod <- feols(as.formula(formula), data)
end <- Sys.time()

######################################
#temp

preddf <- data.frame(temp=seq(min(data$temp), max(data$temp), length.out=250))

preddf$vader <- 1

mm <- model.matrix(vader ~ temp + I(temp^2),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

curve <- ggplot(preddf) + 
  geom_line(aes(x=temp, y=contrast)) + 
  geom_ribbon(aes(x=temp, ymin=ymin, ymax=ymax), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.4, 43.1)) + 
  labs(x='', y='Predicted Mood of Tweet') +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'temp']) +
  geom_histogram(aes(x=temp), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.38222, 43.02777)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/quad-temp-only.png', width=6, height=7)

########################################
#precip

preddf <- data.frame(precip=seq(min(data$precip), max(data$precip), length.out=250))

preddf$vader <- 1

mm <- model.matrix(vader ~ precip + I(precip^2),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

curve <- ggplot(preddf) + 
  geom_line(aes(x=log(precip + 1), y=contrast)) + 
  geom_ribbon(aes(x=log(precip + 1), ymin=ymin, ymax=ymax), alpha=0.5) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(x='', y='Predicted Mood of Tweet') +
  theme(legend.position=c(0.2, 0.8))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'precip']) +
  geom_histogram(aes(x=log(precip + 1))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Precipitation (mm)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/quad-precip-only.png', width=6, height=7)



########################################
#srad

preddf <- data.frame(srad=seq(min(data$precip), max(data$precip), length.out=250))

preddf$vader <- 1

mm <- model.matrix(vader ~ srad + I(srad^2),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

curve <- ggplot(preddf) + 
  geom_line(aes(x=srad, y=contrast)) + 
  geom_ribbon(aes(x=srad, ymin=ymin, ymax=ymax), alpha=0.5) + 
  labs(x='', y='Predicted Mood of Tweet') +
  theme(legend.position=c(0.2, 0.15))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'srad']) +
  geom_histogram(aes(x=srad)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  #scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Sunlight (Shortwave Radiation - w/m^2)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/quad-srad-only.png', width=6, height=7)


