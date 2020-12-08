##Use E64

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)
library(Hmisc)

options(scipen=100)

setwd('~/tweets/')

data <- fread('all.csv')

data$race <- factor(ifelse(data$majority == 'race_white', 'white', 'minority'))
qs <- levels(data$race)
#weekdaymeans <- data[ , .(Mean=mean(vader)), .(dow)]

#Tempknots
#Tempknots
knots = list("temp"= c(min(data$temp), -10, 0, 5, 10, 15, 20, 25, 30, 35, 
                          max(data$temp)),
             "precip"= c(min(data$precip), 0.000001, 0.05, 0.5, max(data$precip)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))

#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='', interact_type=c('factor', 'continuous')) {
  knots <- knots[c(-1, -length(knots))]
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  if (interact_type == 'continuous'){
    formula <- paste(interact_var, '*', var.name, "+",
          paste("I(", interact_var, "*pmax(", var.name, formula.sign, abs(knots), ", 0))",
                collapse = " + ", sep=""))
  } else{
    formula <- paste(interact_var, '*', var.name, "+",
          paste(interact_var, "*I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
                collapse = " + ", sep=""))
  }
  formula
}

formula <- paste0("vader ~ ", 
                     piece.formula("temp", knots[['temp']], "race", 'factor'), ' + ',
                     piece.formula("precip", knots[['precip']], "race", 'factor'), ' + ',
                     piece.formula("srad", knots[['srad']], "race", 'factor'),
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
#temp

preddf <- data.frame(temp=fill_knots(knots$temp))
preddf <- make_groups(preddf, 'race', qs)

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("temp", knots[['temp']], "race", "factor"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$race <- factor(preddf$race)

curve <- ggplot(preddf) + 
  geom_line(aes(x=temp, y=contrast, color=race)) + 
  geom_ribbon(aes(x=temp, ymin=ymin, ymax=ymax, fill=race), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.4, 43.1)) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'temp']) +
  geom_histogram(aes(x=temp), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.38222, 43.02777)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/temp-race.png', width=6, height=7)

########################################
#precip

preddf <- data.frame(precip=fill_knots(knots$precip))
preddf <- make_groups(preddf, 'race', qs)

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("precip", knots[['precip']], "race", "factor"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$race <- factor(preddf$race)

curve <- ggplot(preddf) + 
  geom_line(aes(x=log(precip + 1), y=contrast, color=race)) + 
  geom_ribbon(aes(x=log(precip + 1), ymin=ymin, ymax=ymax, fill=race), alpha=0.5) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.8))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'precip']) +
  geom_histogram(aes(x=log(precip + 1))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Precipitation (mm)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/precip-race.png', width=6, height=7)



########################################
#srad

preddf <- data.frame(srad=fill_knots(knots$srad))
preddf <- make_groups(preddf, 'race', qs)

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("srad", knots[['srad']], "race", "factor"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$race <- factor(preddf$race)

curve <- ggplot(preddf) + 
  geom_line(aes(x=srad, y=contrast, color=race)) + 
  geom_ribbon(aes(x=srad, ymin=ymin, ymax=ymax, fill=race), alpha=0.5) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.15))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'srad']) +
  geom_histogram(aes(x=srad)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  #scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Sunlight (Shortwave Radiation - w/m^2)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/srad-race.png', width=6, height=7)


