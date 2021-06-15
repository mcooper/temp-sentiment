library(data.table)
library(survey)
library(tidyverse)
library(fixest)
library(splines)

RUN <- 'temporal2'

setwd('~/tweets/')

data <- fread('all.csv')

data <- data[weather_term == 0, ]
data <- data[data$wbgt > 5, ]

data$raining <- data$prcp > 0

data$tod_cont <- as.numeric(substr(data$tod, 1, 2)) + as.numeric(substr(data$tod, 4, 4))/6

knots <- seq(0, 24, by=2)

make_cyclic_splines <- function(knots, x){
  df <- data.frame(splineDesign(knots, x, ord=2, outer.ok=TRUE))
  names(df) <- paste0('s', 1:ncol(df))
  df$s0 <- ifelse(x < knots[2], 1 - df$s1,
                  ifelse(x > knots[length(knots) - 1], 1 - df[ , ncol(df)],
                         0))
  return(df)
}

data <- cbind(data, make_cyclic_splines(knots, data$tod_cont))

formula <- paste0('vader ~ ', 
                  paste0(paste0('wbgt:s', 0:(length(knots) - 2)), collapse=' + '),
                  " + raining + srad | dow + doy + tod + fips + year + statemonth")

for (i in 1:80){
  mod <- feols(as.formula(formula), data[sample(1:nrow(data), nrow(data), replace=T), ])
  cf <- coef(mod)
  vc <- vcov(mod)
  myobj <- list(vcov=vc, coef=cf)
  class(myobj) <- 'bootmod'
  saveRDS(myobj, file=paste0('bootstrap/', RUN, '/', Sys.time()))
  system(paste0('telegram "Did a business ', i, '"'))
  rm(list=c('mod', 'cf', 'vc', 'myobj'))
  gc()
}

system('sudo poweroff')


