#E48
library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

MOD_RUN <- 'compare_race_income'

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

################################
# Both
###############################

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], "race_black"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'race_black*income_percap*raining + race_black*srad*income_percap',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)

bothaic <- AIC(mod)

rm(mod)
gc()

################################
# Income
###############################

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'income_percap*raining + srad*income_percap',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)

incoaic <- AIC(mod)

rm(mod)
gc()

################################
# Race
###############################

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "race_black"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'race_black*raining + race_black*srad',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)

raceaic <- AIC(mod)

rm(mod)
gc()

################################3
# None
##################################

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'raining + srad',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)

noneaic <- AIC(mod)

################################
# Save results
###########################
df <- data.frame(mod=c('both', 'income', 'race', 'none'), 
                 aic=c(bothaic, incoaic, raceaic, noneaic))

write.csv(df, '~/tweets/mod-res/compare.csv', row.names=F)

system('telegram "Donezo!"')




