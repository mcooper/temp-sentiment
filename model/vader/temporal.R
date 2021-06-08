library(data.table)
library(fixest)
library(tidyverse)

RUN <- 'temporal'

setwd('~/tweets/')

data <- fread('all.csv')

data <- data[weather_term == 0, ]

data$raining <- data$prcp > 0

data$tod_cont <- as.numeric(substr(data$tod, 1, 2)) + as.numeric(substr(data$tod, 4, 4))/6

#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='') {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  if (interact_var == ''){
    paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
  } else{
    paste(interact_var, '*', var.name, "+",
        paste("I(", interact_var, "*pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
  }
}

formula <- paste0("vader ~ ", 
                     piece.formula("tod_cont", seq(0, 24, 2), "wbgt"), ' + ',
                     'raining + srad',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)

cf <- coef(mod)
vc <- vcov(mod)
myobj <- list(vcov=vc, coef=cf)
class(myobj) <- 'bootmod'
saveRDS(myobj, file=paste0('bootstrap/', RUN, '/', Sys.time()))
system('telegram "Did a business "')




for (i in 1:80){
  mod <- feols(as.formula(formula), data[sample(1:nrow(data), nrow(data), replace=T), ])
  cf <- coef(mod)
  vc <- vcov(mod)
  myobj <- list(vcov=vc, coef=cf)
  class(myobj) <- 'bootmod'
  saveRDS(myobj, file=paste0('bootstrap/', RUN, '/', Sys.time()))
  system(paste0('~/telegram.sh "Did a business ', i, '"'))
  rm(list=c('mod', 'cf', 'vc', 'myobj'))
  gc()
}


system('sudo poweroff')


