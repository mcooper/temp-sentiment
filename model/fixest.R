##Use E32

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/')

data <- fread('all.csv')
data$income_percap <- log(data$income_percap)
data$prcp_q <- data$prcp > 0

qs <- quantile(data$income_percap, seq(0, 1, by=0.05))
weekdaymeans <- data[ , .(Mean=mean(vader)), .(dow)]

#Tempknots
#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)),
             "prcp"= c(min(data$prcp), 0.000001, 0.05, 0.5, max(data$prcp)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))

#Define Segmenting Function
piece.formula <- function(var.name, knots) {
  knots <- knots[c(-1, -length(knots))]
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']]), ' + ',
                     piece.formula("prcp", knots[['prcp']]), ' + ',
                     piece.formula("srad", knots[['srad']]),
                     " | dow + doy + tod + fips + year + statemonth + race_majority")



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
#wbgt

preddf <- data.frame(wbgt=fill_knots(knots$wbgt))

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("wbgt", knots[['wbgt']]))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE


curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=contrast)) + 
  geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.4, 43.1)) + 
  labs(x='', y='Predicted Mood of Tweet') +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.38222, 43.02777)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/wbgt-oldspecification-race.png', width=6, height=7)

########################################
#prcp

preddf <- data.frame(prcp=fill_knots(knots$prcp))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("prcp", knots[['prcp']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

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
  geom_line(aes(x=log(prcp + 1), y=contrast, color=income_percap)) + 
  geom_ribbon(aes(x=log(prcp + 1), ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.8))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'prcp']) +
  geom_histogram(aes(x=log(prcp + 1))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  scale_x_continuous(labels=function(x){round(exp(x) - 1, 2)}) + 
  labs(y="Tweet Count", x="Precipitation (mm)")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/prcp-income.png', width=6, height=7)



########################################
#srad

preddf <- data.frame(srad=fill_knots(knots$srad))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

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
ggsave('~/temp-sentiment/res/srad-income.png', width=6, height=7)


