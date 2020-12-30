library(tidyverse)
library(cowplot)
library(survey)
library(data.table)

options(scipen=100)

setwd('~/tweets/bootstrap/run1/')

data <- fread('~/tweets/all.csv')
data$income_percap <- log(data$income_percap)

data <- data[weather_term == 0, ]

qs <- quantile(data$income_percap, seq(0, 1, by=0.05))

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)),
             "prcp"= c(min(data$prcp), 0.000001, 0.05, 0.5, max(data$prcp)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))

#Tempknots
knots = list("wbgt"= c(-30, -10, 0, 5, 10, 15, 20, 25, 
                          30),
             "prcp"= c(0, 0.000001, 0.05, 0.5, 10),
             "srad"= c(0, 0.000001, 250, 500, 1000, 1800))


######################################
#wbgt

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

vcov.bootmod <- function(object){
  return(object$vcov)
}
coef.bootmod <- function(object){
  return(object$coef)
}


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

preddf <- data.frame(wbgt=fill_knots(knots$wbgt))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                   piece.formula("wbgt", knots[['wbgt']], "income_percap"))),
                   data=preddf)

colnames(mm)[colnames(mm) == 'wbgt:income_percap'] <- 'income_percap:wbgt'
mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])


#Iterate over models
mods <- list.files()

for (modf in mods){

  mod <- readRDS(modf)

  res <- as.data.frame(svycontrast(mod, mmp))[ , 'contrast', drop=F]
  names(res) <- modf
  
  preddf <- cbind(preddf, res)
}

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

preddf <- preddf %>%
  gather(key, value, -wbgt, -income_percap, -vader) %>%
  mutate(group = paste0(income_percap, key))

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=value, color=income_percap, group=group)) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  labs(x='', y='Change in Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)') +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Wet Bulb Global Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/wbgt-income-ref-segments-bootstrap.png', width=6, height=7)
