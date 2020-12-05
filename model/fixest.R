##Use E32

library(data.table)
library(fixest)
library(survey)
library(tidyverse)

setwd('~/tweets/')

#data <- fread('all.csv')
data <- fread('all_samp_1pct.csv')
data$income_percap <- log(data$income_percap)

#Tempknots
knots = list("temp.hi"= c(min(data$temp.hi), -10, 0, 5, 10, 15, 20, 25, 30, 35, 
                          max(data$temp.hi)),
             "precip"= c(min(data$precip), 0.000001, 0.05, 0.5, max(data$precip)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))

#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='') {
  knots <- knots[c(-1, -length(knots))]
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  if (interact_var != ''){
    var.name <- paste0(interact_var, "*", var.name)
  }
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

formula <- paste0("vader ~ ", 
                     piece.formula("temp.hi", knots[['temp.hi']], "income_percap"), ' + ',
                     piece.formula("precip", knots[['precip']], "income_percap"), ' + ',
                     piece.formula("srad", knots[['srad']], "income_percap"),
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)

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

######################################
#temp.hi

preddf <- data.frame(temp.hi=fill_knots(knots$temp.hi))
preddf <- make_groups(preddf, 'income_percap', c(9.51, 9.96, 10.23, 10.48, 10.9))

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("temp.hi", knots[['temp.hi']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)

ggplot(preddf) + 
  geom_line(aes(x=temp.hi, y=contrast, color=income_percap)) + 
  geom_ribbon(aes(x=temp.hi, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5)

########################################
#precip

preddf <- data.frame(precip=fill_knots(knots$precip))
preddf <- make_groups(preddf, 'income_percap', c(9.51, 9.96, 10.23, 10.48, 10.9))

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("precip", knots[['precip']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)

ggplot(preddf) + 
  geom_line(aes(x=log(precip), y=contrast, color=income_percap)) + 
  geom_ribbon(aes(x=log(precip), ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5)

########################################
#srad

preddf <- data.frame(srad=fill_knots(knots$srad))
preddf <- make_groups(preddf, 'income_percap', c(9.51, 9.96, 10.23, 10.48, 10.9))

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("srad", knots[['srad']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)

ggplot(preddf) + 
  geom_line(aes(x=srad, y=contrast, color=income_percap)) + 
  geom_ribbon(aes(x=srad, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5)

