library(data.table)
library(fixest)
library(tidyverse)

RUN <- 'hedono_wbgt_nointeract'

setwd('~/tweets/')

data <- fread('all.csv')

data <- data[weather_term == 0, ]

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

data$raining <- data$prcp > 0

#Save memory by deleting useless columns!
data <- data[ , c('wbgt', 'raining', 'srad', 'hedono', 'fips', 'year', 
                  'statemonth', 'tod', 'doy', 'dow')]

#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='') {
  knots <- knots[c(-1, -length(knots))]
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

formula <- paste0("hedono ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'raining + srad',
                     " | dow + doy + tod + fips + year + statemonth")

for (i in 3:80){
  mod <- feols(as.formula(formula), data[sample(1:nrow(data), nrow(data), replace=T), ])
  cf <- coef(mod)
  vc <- vcov(mod)
  myobj <- list(vcov=vc, coef=cf)
  class(myobj) <- 'bootmod'
  saveRDS(myobj, file=paste0('bootstrap/', RUN, '/', Sys.time()))
  if (i %% 10 == 0){
    system(paste0('~/telegram.sh "Did a business ', i, '"'))
  }
  rm(list=c('mod', 'cf', 'vc', 'myobj'))
  gc()
}


system('sudo poweroff')


