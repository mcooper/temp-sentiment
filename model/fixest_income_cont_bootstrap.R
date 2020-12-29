library(data.table)
library(fixest)
library(survey)
library(tidyverse)

setwd('~/tweets/')

data <- fread('all.csv')
data$income_percap <- log(data$income_percap)

data <- data[weather_term == 0, ]

qs <- quantile(data$income_percap, seq(0, 1, by=0.05))

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)),
             "prcp"= c(min(data$prcp), 0.000001, 0.05, 0.5, max(data$prcp)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))


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
                     piece.formula("prcp", knots[['prcp']], "income_percap"), ' + ',
                     piece.formula("srad", knots[['srad']], "income_percap"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     piece.formula("prcp", knots[['prcp']], ""), ' + ',
                     piece.formula("srad", knots[['srad']], ""), 
                     " | dow + doy + tod + fips + year + statemonth")

for (i in 1:99){
  mod <- feols(as.formula(formula), data[sample(1:nrow(data), nrow(data), replace=T), ])
  cf <- coef(mod)
  vc <- vcov(mod)
  myobj <- list(vcov=vc, coef=cf)
  class(myobj) <- 'bootmod'
  saveRDS(myobj, file=paste0('bootstrap/run1/', Sys.time()))
  system(paste0('~/telegram.sh "Did a business ', i, '"'))
  rm(list=c('mod', 'cf', 'vc', 'myobj'))
}

