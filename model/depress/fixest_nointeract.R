library(data.table)
library(fixest)
library(tidyverse)

setwd('~/tweets/')

data <- fread('all.csv')

data <- data[weather_term == 0, ]

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

data$raining <- data$prcp > 0

####################################
# No Interaction
####################################

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

formula <- paste0("depress_term ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'raining + srad',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feglm(as.formula(formula), data,
             family = binomial(link = 'logit'))
cf <- coef(mod)
vc <- vcov(mod)
myobj <- list(vcov=vc, coef=cf)
class(myobj) <- 'bootmod'
saveRDS(myobj, file=paste0('bootstrap/depress_test/no_interact'))
rm(mod)
gc()

##################################
# Income Continuous
################################
data$income_percap <- log(data$income_percap)

formula <- paste0("depress_term ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'income_percap*raining + income_percap*srad',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feglm(as.formula(formula), data,
             family = binomial(link = 'logit'))
cf <- coef(mod)
vc <- vcov(mod)
myobj <- list(vcov=vc, coef=cf)
class(myobj) <- 'bootmod'
saveRDS(myobj, file=paste0('bootstrap/depress_test/income_cont'))
rm(mod)
gc()

################################
# Race Binned
##################################

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

formula <- paste0("depress_term ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "race_majority", 'factor'), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], "", 'none'), ' + ',
                     'race_majority*raining + race_majority*srad',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feglm(as.formula(formula), data, 
             family = binomial(link = 'logit'))
cf <- coef(mod)
vc <- vcov(mod)
myobj <- list(vcov=vc, coef=cf)
class(myobj) <- 'bootmod'
saveRDS(myobj, file=paste0('bootstrap/depress_test/race_cont'))

system('telegram "Done with all"')
system('sudo poweroff')
