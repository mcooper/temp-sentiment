##Use E32

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/')

#Subset to Massachussets
data <- fread('all.csv') %>%
  filter(fips < 26000, fips >= 25000)
data$income_percap <- log(data$income_percap)

qs <- quantile(data$income_percap, seq(0, 1, by=0.05))
weekdaymeans <- data[ , .(Mean=mean(vader)), .(dow)]

#Tempknots
#Tempknots
knots = list("temp.hi"= c(min(data$temp.hi), -10, 0, 5, 10, 15, 20, 25, 30, 35, 
                          max(data$temp.hi)),
             "precip"= c(min(data$precip), 0.000001, 0.05, 0.5, max(data$precip)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, max(data$srad)))

#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='') {
  knots <- knots[ -length(knots)]
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
                     piece.formula("temp.hi", knots[['temp.hi']], "income_percap"), ' + ',
                     piece.formula("precip", knots[['precip']], "income_percap"), ' + ',
                     piece.formula("srad", knots[['srad']], "income_percap"), ' + ',
                     piece.formula("temp.hi", knots[['temp.hi']], ""), ' + ',
                     piece.formula("precip", knots[['precip']], ""), ' + ',
                     piece.formula("srad", knots[['srad']], ""), 
                     " + dow + doy + tod + fips + year")

mod <- lm(as.formula(formula), data)

fill_knots <- function(x, filln=10){
  vals <- NULL
  for (i in 2:length(x)){
    vals <- c(vals, seq(x[i-1], x[i], length.out=filln))
  }
  vals <- unique(vals)
  vals <- vals[order(vals)]
  vals
}

make_groups <- function(df, label, values){
  final <- data.frame()
  for (v in values){
    df[ , label] <- v
    final <- bind_rows(df, final)
  }
  final
}



preddf <- bind_rows(data.frame(precip=0, srad=0, temp.hi=seq(-20, 40)),
                    data.frame(temp.hi=0,srad=0,precip=c(0, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50)),
                    data.frame(precip=0, temp.hi=0, srad=seq(0, 1000, by=100)))

preddf <- bind_rows(preddf %>% mutate(income_percap=qs[2]),
                    preddf %>% mutate(income_percap=qs[11]),
                    preddf %>% mutate(income_percap=qs[20]))

preddf$dow <- data$dow[1]
preddf$doy <- data$doy[1]
preddf$tod <- data$tod[1]
preddf$fips <- data$fips[1]
preddf$year <- data$year[1]

p <- predict(mod, preddf, se.fit=T)

preddf$prediction <- p$fit
preddf$se <- p$se.fit

preddf$ymax <- preddf$prediction + preddf$se*1.96
preddf$ymin <- preddf$prediction - preddf$se*1.96

preddf$income_percap <- factor(preddf$income_percap)

ggplot(preddf %>% filter(temp.hi != 0)) +
  geom_line(aes(x=temp.hi, y=prediction, color=income_percap)) + 
  geom_ribbon(aes(x=temp.hi, ymax=ymax, ymin=ymin, fill=income_percap), alpha=0.25)
#Still massive standard errors!











