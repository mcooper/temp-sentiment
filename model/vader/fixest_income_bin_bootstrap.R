library(data.table)
library(fixest)
library(tidyverse)
library(Hmisc)

MOD_RUN <- 'income_percap_q'

setwd('~/tweets/')

data <- fread('all.csv')

data$income_percap_q <- cut2(data$income_percap, g=3)

data <- data[weather_term == 0, ]

data$raining <- data$prcp > 0

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

#Define Segmenting Function
# DIFFERENT FOR GROUPING VARS
piece.formula <- function(var.name, knots, interact_var='', interact_type=c('factor', 'continuous', 'none')) {
  knots <- knots[c(-1, -length(knots))]
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  if (interact_type == 'none'){
    formula <- paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
  }
  if (interact_type == 'continuous'){
    formula <- paste(interact_var, '*', var.name, "+",
          paste("I(", interact_var, "*pmax(", var.name, formula.sign, abs(knots), ", 0))",
                collapse = " + ", sep=""))
  } 
  if (interact_type == 'factor'){
    formula <- paste(interact_var, '*', var.name, "+",
          paste(interact_var, "*I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
                collapse = " + ", sep=""))
  }
  formula
}




formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap_q", 'factor'), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], "", 'none'), ' + ',
                     'income_percap_q*raining + income_percap_q*srad',
                     " | dow + doy + tod + fips + year + statemonth")

for (i in 1:80){
  mod <- feols(as.formula(formula), data[sample(1:nrow(data), nrow(data), replace=T), ])
  cf <- coef(mod)
  vc <- vcov(mod)
  myobj <- list(vcov=vc, coef=cf)
  class(myobj) <- 'bootmod'
  saveRDS(myobj, file=paste0('bootstrap/', MOD_RUN, '/', Sys.time()))
  if (i %% 10 == 0){
    system(paste0('~/telegram.sh "Did a business ', i, '"'))
  }
  print(i)
  rm(list=c('mod', 'cf', 'vc', 'myobj'))
  gc()
}


system('sudo poweroff')


