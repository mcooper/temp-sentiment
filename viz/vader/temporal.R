library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)

options(scipen=100)

MOD_RUN <- 'temporal'

setwd(paste0('~/tweets/bootstrap/', MOD_RUN, '/'))

data <- fread('~/tweets/all.csv')
data <- data[weather_term == 0, ]
data$raining <- data$prcp > 0

data$tod_cont <- as.numeric(substr(data$tod, 1, 2)) + as.numeric(substr(data$tod, 4, 4))/6

####################################
#Functions
####################################

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


#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='') {
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

########################################
# WBGT
#########################################
preddf <- data.frame(wbgt=c(0, 1))
preddf <- make_groups(preddf, 'tod_cont', seq(0, 24, 0.1))
preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ", 
                     piece.formula("tod_cont", knots=seq(0, 24, 2), "wbgt"))),
                   data=preddf)

mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'wbgt:tod_cont']
mm <- mm[ , colnames(mm) != 'I(wbgt * pmax(tod_cont - 24, 0))']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

mod <- readRDS(list.files())

res <- as.data.frame(svycontrast(mod, mmp))[ , 'contrast', drop=F]

preddf <- cbind(preddf, res)

preddf <- preddf %>%
  spread(wbgt, contrast) %>%
  mutate(coef = `1` - `0`)

ggplot(preddf) + 
  geom_line(aes(x=tod_cont, y=coef))
ggsave('~/temp-sentiment/res/ts_segmented.png')
# Looks very uncertain off of one run, and somehow also 






















