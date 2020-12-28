#Use E32

library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/')

data <- fread('all.csv')
data <- data[weather_term == 0, ]

knots = list("temp"= c(min(data$temp), -10, 0, 5, 10, 15, 20, 25, 30, 35, 
data$income_percap <- log(data$income_percap)
qs <- quantile(data$income_percap, seq(0, 1, by=0.05))

data$temp.bin <- cut(data$temp, breaks=c(-10, 0, 5, 10, 15, 20, 25, 30, 35, 100))
data$precip.bin <- cut(data$precip, breaks=c(-100, 0.000001, 1000), labels=c("norain", "rain"))
data$srad.bin <- cut(data$srad, breaks=c(-100, 0.000001, 750, 1500), labels=c("dark", "cloudy", "sunny"))

formula <- paste0("vader ~ income_percap*temp.bin + 
                  income_percap*precip.bin + 
                  income_percap*srad.bin | dow + doy + tod + fips + year + statemonth")

start <- Sys.time()
mod <- feols(as.formula(formula), data)
end <- Sys.time()

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

preddf <- data.frame(temp.bin=unique(data$temp.bin))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(vader ~ income_percap*temp.bin,
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]
mm <- mm[ , !colnames(mm) %in% 'income_percap']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)

ggplot(preddf) + 
  geom_bar(aes(x=income_percap, y=contrast, fill=temp.bin), stat='identity', position='dodge') + 
  geom_errorbar(aes(x=income_percap, ymin=ymin, ymax=ymax, color=temp.bin), position='dodge')
ggsave('~/temp-sentiment/res/temp-income_bins.png')
