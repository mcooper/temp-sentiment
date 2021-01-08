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
data$income_percap_q <- cut2(data$income_percap, g=3)
qs <- levels(data$income_percap_q)

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

data$raining <- data$prcp > 0

#Define Segmenting Function
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

formula <- paste0("vader ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap_q", 'factor'), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], "", 'none'), ' + ',
                     'income_percap_q*raining + srad*income_percap_q',
                     " | dow + doy + tod + fips + year + statemonth")

mod <- feols(as.formula(formula), data)
system('~/telegram.sh "done"')

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
preddf <- make_groups(preddf, 'income_percap_q', qs)

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("wbgt", knots[['wbgt']], "income_percap_q", "factor"), ' + ',
                   piece.formula("wbgt", knots[['wbgt']], "", "none"))),
                   data=preddf)

mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap_q[22263.6, 34129)']
mm <- mm[ , colnames(mm) != 'income_percap_q[34129.4,539681]']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(mod, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap_q <- factor(preddf$income_percap_q)


preddf <- preddf %>%
  gather(key, value, -wbgt, -income_percap_q, -vader) %>%
  #Normalize so that graph shows difference from wbgt = X
  group_by(income_percap_q, key) %>%
  mutate(value = value - value[wbgt == 5]) %>%
  spread(key, value)

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=contrast, color=income_percap_q)) + 
  #geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax, fill=income_percap_q), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.4, 43.1)) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)' ) +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-21.38222, 43.02777)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/wbgt-income_q.png', width=6, height=7)
