#Use E48

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

MOD_RUN <- 'race_income2'

setwd('~/tweets/')

data <- fread('all.csv')
data$income_percap <- log(data$income_percap)

data <- data[weather_term == 0, ]

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

data$raining <- data$prcp > 0

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

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], "race_black"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'race_black*income_percap*raining + race_black*srad*income_percap',
                     " | dow + doy + tod + fips + year + statemonth")


for (i in 1:80){
  mod <- feols(as.formula(formula), data[sample(1:nrow(data), nrow(data), replace=T), ])
  cf <- coef(mod)
  vc <- vcov(mod)
  myobj <- list(vcov=vc, coef=cf)
  class(myobj) <- 'bootmod'
  saveRDS(myobj, file=paste0('bootstrap/', MOD_RUN, '/', Sys.time()))
  if (i %% 10 == 0){
    system(paste0('~/telegram "Did a business ', i, '"'))
  }
  print(i)
  rm(list=c('mod', 'cf', 'vc', 'myobj'))
  gc()
}

system('sudo poweroff')



