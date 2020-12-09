##Use E48

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/')

data <- fread('all.csv')
qs <- quantile(data$impervious, seq(0, 1, by=0.05))
weekdaymeans <- data[ , .(Mean=mean(vader)), .(dow)]

#Tempknots
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
  paste(interact_var, '*', var.name, "+",
        paste("I(", interact_var, "*pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

formula <- paste0("vader ~ ", 
                     piece.formula("temp.hi", knots[['temp.hi']], "impervious"), ' + ',
                     piece.formula("precip", knots[['precip']], "impervious"), ' + ',
                     piece.formula("srad", knots[['srad']], "impervious"),
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
preddf <- make_groups(preddf, 'impervious', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("temp.hi", knots[['temp.hi']], "impervious"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$impervious <- factor(preddf$impervious)

curve <- ggplot(preddf) + 
  geom_line(aes(x=temp.hi, y=contrast, color=impervious)) + 
  geom_ribbon(aes(x=temp.hi, ymin=ymin, ymax=ymax, fill=impervious), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.4, 43.1)) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nImpervious Land Cover\n(Percentile)',
       color = 'Census Block\nImpervious Land Cover\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'temp.hi']) +
  geom_histogram(aes(x=temp.hi), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.38222, 43.02777)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/temp.hi-impervious.png', width=6, height=7)

########################################
#precip

preddf <- data.frame(precip=fill_knots(knots$precip))
preddf <- make_groups(preddf, 'impervious', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("precip", knots[['precip']], "impervious"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$impervious <- factor(preddf$impervious)

curve <- ggplot(preddf) + 
  geom_line(aes(x=log(precip + 1), y=contrast, color=impervious)) + 
  geom_ribbon(aes(x=log(precip + 1), ymin=ymin, ymax=ymax, fill=impervious), alpha=0.5) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nImpervious Land Cover\n(Percentile)',
       color = 'Census Block\nImpervious Land Cover\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.8))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'precip']) +
  geom_histogram(aes(x=log(precip + 1))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Precipitation (mm)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/precip-impervious.png', width=6, height=7)



########################################
#srad

preddf <- data.frame(srad=fill_knots(knots$srad))
preddf <- make_groups(preddf, 'impervious', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("srad", knots[['srad']], "impervious"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$impervious <- factor(preddf$impervious)

curve <- ggplot(preddf) + 
  geom_line(aes(x=srad, y=contrast, color=impervious)) + 
  geom_ribbon(aes(x=srad, ymin=ymin, ymax=ymax, fill=impervious), alpha=0.5) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nImpervious Land Cover\n(Percentile)',
       color = 'Census Block\nImpervious Land Cover\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.15))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'srad']) +
  geom_histogram(aes(x=srad)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  #scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Sunlight (Shortwave Radiation - w/m^2)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/srad-impervious.png', width=6, height=7)


