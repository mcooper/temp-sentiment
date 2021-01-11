#Use E32

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/')

data <- fread('all.csv')
data$income_percap <- log(data$income_percap)

data <- data[weather_term == 0, ]

qs <- quantile(data$income_percap, seq(0, 1, by=0.05))
rqs <- quantile(data$race_white, seq(0, 1, by=0.05))
#weekdaymeans <- data[ , .(Mean=mean(vader)), .(dow)]
#max(weekdaymeans$Mean) - min(weekdaymeans$Mean)

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
                     piece.formula("wbgt", knots[['wbgt']], "race_white"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     'income_percap*raining + srad*income_percap',
                     " | dow + doy + tod + fips + year + statemonth")

start <- Sys.time()
mod <- feols(as.formula(formula), data)
system('~/telegram.sh "Done with Model"')
end <- Sys.time()

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

###################################
# Check SSR
##################################

######################################
#wbgt

preddf <- data.frame(wbgt=fill_knots(knots$wbgt))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])
preddf <- make_groups(preddf, 'race_white', rqs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("wbgt", knots[['wbgt']], "race_white"), ' + ',
                   piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                   piece.formula("wbgt", knots[['wbgt']], "income_percap"))),
                   data=preddf)

colnames(mm)[colnames(mm) == 'wbgt:income_percap'] <- 'income_percap:wbgt'
colnames(mm)[colnames(mm) == 'race_white:wbgt'] <- 'wbgt:race_white'
mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'race_white']
mm <- mm[ , colnames(mm) != 'income_percap']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income <- factor(round(exp(preddf$income_percap)))
preddf$race <- factor(round(preddf$race_white*100))

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=contrast, color=income, linetype=race)) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  labs(x='', y='Predicted Mood of Tweet') + 
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/wbgt-income-race-ref-segments-SE_Noweather_simple_sradprcp.png', width=6, height=7)

######################################
#prcp

preddf <- data.frame(raining=c(TRUE, FALSE))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(vader ~ raining*income_percap,
                   data=preddf)

colnames(mm)[colnames(mm) == 'rainingTRUE:income_percap'] <- 'income_percap:rainingTRUE'
mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)
levels(preddf$income_percap) <- preddf$income_percap %>%
                                   levels %>%
                                   as.numeric %>%
                                   exp %>%
                                   round(0) %>%
                                   format(big.mark = ',') %>%
                                   paste0('$', .)

levels(preddf$income_percap) <- paste0(levels(preddf$income_percap), ' (', 
                                       names(qs[c(2, 11, 20)]), ')')

curve <- ggplot(preddf %>% filter(raining)) + 
  geom_bar(aes(x=income_percap, y=contrast, fill=income_percap), stat='identity') + 
  geom_errorbar(aes(x=income_percap, ymin=ymin, ymax=ymax, color=income_percap)) + 
  labs(x='', y='Decrease in Mood of Tweet During Rainfall',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)')

ggsave(curve, filename='~/temp-sentiment/res/prcp-income-ref-segments-SE_Noweather_simple_sradprcp.png', width=6, height=7)

######################################
#srad

preddf <- data.frame(srad=seq(0, 1400, by=10))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(vader ~ srad*income_percap,
                   data=preddf)

colnames(mm)[colnames(mm) == 'srad:income_percap'] <- 'income_percap:srad'
mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)
levels(preddf$income_percap) <- preddf$income_percap %>%
                                   levels %>%
                                   as.numeric %>%
                                   exp %>%
                                   round(0) %>%
                                   format(big.mark = ',') %>%
                                   paste0('$', .)

levels(preddf$income_percap) <- paste0(levels(preddf$income_percap), ' (', 
                                       names(qs[c(2, 11, 20)]), ')')

curve <- ggplot(preddf) + 
  geom_line(aes(x=srad, y=contrast, color=income_percap)) + 
  geom_ribbon(aes(x=srad, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(0, 1400)) + 
  labs(x='', y='Change in Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)') +
  theme(legend.position=c(0.2, 0.8))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'srad']) +
  geom_histogram(aes(x=srad), bins=100) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-0.001, 1400)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Shortwave Radiation (W/m^2)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/srad-income-ref-segments-SE_Noweather_simple_sradprcp.png', width=6, height=7)

