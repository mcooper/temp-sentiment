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

qs <- quantile(data$race_white, seq(0, 1, by=0.05))
#weekdaymeans <- data[ , .(Mean=mean(vader)), .(dow)]
#max(weekdaymeans$Mean) - min(weekdaymeans$Mean)

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
                     piece.formula("wbgt", knots[['wbgt']], "race_white"), ' + ',
                     piece.formula("prcp", knots[['prcp']], "race_white"), ' + ',
                     piece.formula("srad", knots[['srad']], "race_white"), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                     piece.formula("prcp", knots[['prcp']], ""), ' + ',
                     piece.formula("srad", knots[['srad']], ""), 
                     " | dow + doy + tod + fips + year + statemonth")

start <- Sys.time()
mod <- feols(as.formula(formula), data)
end <- Sys.time()

system('~/telegram.sh "Donezo"')

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
preddf <- make_groups(preddf, 'race_white', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                   piece.formula("wbgt", knots[['wbgt']], "race_white"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

colnames(mm)[colnames(mm) == 'wbgt:race_white'] <- 'race_white:wbgt'
mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'race_white']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$race_white <- factor(preddf$race_white)
levels(preddf$race_white) <- preddf$race_white %>%
                                   levels %>%
                                   as.numeric %>%
                                   "*"(100) %>%
                                   round(1) %>%
                                   paste0('%')

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=contrast, color=race_white)) + 
  geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax, fill=race_white), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nPercent White\n(Percentile)',
       color = 'Census Block\nPercent White\n(Percentile)',
       subtitle = paste0("AIC: ", AIC(mod), '\nSSR: ', mod$ssr)) +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/wbgt-race-ref-segments-SE_Noweather.png', width=6, height=7)
