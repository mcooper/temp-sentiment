library(data.table)
library(fixest)
library(survey)
library(tidyverse)
library(cowplot)
library(splines)
library(lubridate)

options(scipen=100)

MOD_RUN <- 'temporal2'

setwd(paste0('~/tweets/bootstrap/', MOD_RUN, '/'))

data <- fread('~/tweets/all.csv')

data <- data[weather_term == 0, ]
data <- data[data$wbgt > 5, ]

data$raining <- data$prcp > 0

data$tod_cont <- as.numeric(substr(data$tod, 1, 2)) + as.numeric(substr(data$tod, 4, 4))/6

knots <- seq(0, 24, by=2)

####################################
#Functions
####################################

make_cyclic_splines <- function(knots, x){
  df <- data.frame(splineDesign(knots, x, ord=2, outer.ok=TRUE))
  names(df) <- paste0('s', 1:ncol(df))
  df$s0 <- ifelse(x < knots[2], 1 - df$s1,
                  ifelse(x > knots[length(knots) - 1], 1 - df[ , ncol(df)],
                         0))
  return(df)
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
preddf <- data.frame(wbgt=1)
preddf <- make_groups(preddf, 'tod_cont', seq(0, 24, 0.1))
preddf$vader <- 1

preddf <- cbind(preddf, make_cyclic_splines(knots, preddf$tod_cont))

formula <- paste0('vader ~ ', 
                  paste0(paste0('wbgt:s', 0:(length(knots) - 2)), collapse=' + '))

mm <- model.matrix(as.formula(formula),
                   data=preddf)

mm <- mm[ , colnames(mm) != '(Intercept)']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

#Iterate over models
mods <- list.files()
for (modf in mods){

  mod <- readRDS(modf)

  res <- as.data.frame(svycontrast(mod, mmp))[ , 'contrast', drop=F]
  names(res) <- modf
  
  preddf <- cbind(preddf, res)
}

preddf2 <- preddf %>%
  select(-matches('s\\d'), -vader) %>%
  gather(mod, value, -wbgt, -tod_cont) %>%
  group_by(tod_cont) %>%
  summarize(ymin = quantile(value, probs=0.025),
            ymax = quantile(value, probs=0.975),
            pred = quantile(value, probs=0.5))

preddf2$tod <- ymd('2021-01-01') + 
  hours(floor(preddf2$tod_cont)) +
  minutes(floor(60*(preddf2$tod_cont %% 1)))

data$tod2 <- ymd_hms(paste0('2021-01-01 ', data$tod, '0:00'))

curve <- ggplot(preddf2) + 
  geom_ribbon(aes(x=tod, ymin=ymin*10, ymax=ymax*10), fill='#FF0000', alpha=0.5) + 
  geom_line(aes(x=tod, y=pred*10)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_datetime(expand=c(0, 0), 
                   limits=c(ymd_hms('2021-01-01 00:00:00'),
                            ymd_hms('2021-01-02 00:00:00')),
                   label=function(x){paste0(hour(x), ':00')},
                   breaks=seq(ymd_hms('2021-01-01 00:00:00'),
                              ymd_hms('2021-01-01 21:00:00'),
                              by='3 hours')) + 
  labs(x='Time of Day', 
       y=expression('Effect of Heat on Mood of Tweet')) + 
  theme_classic()  + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"),
        panel.grid.major = element_line(color = "lightgrey", size=0.5),
        panel.grid.minor = element_line(color = "lightgrey", size=0.25))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'tod2']) +
  geom_histogram(aes(x=tod2), bins=145) + 
  scale_x_datetime(expand=c(0, 0), 
                   limits=c(ymd_hms('2021-01-01 00:00:00'),
                            ymd_hms('2021-01-02 00:00:00')),
                   label=function(x){paste0(hour(x), ':00')},
                   breaks=seq(ymd_hms('2021-01-01 00:00:00'),
                              ymd_hms('2021-01-01 21:00:00'),
                              by='3 hours')) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}, expand=c(0,0),
                     breaks=c(10000)) + 
  labs(y="Tweet\nCount", x="Time of Day") + 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "lightgrey", size=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_line(color = "lightgrey", size=0.25))

plot_grid(curve, hist, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/ts_heat.png', width=4.5, height=4)

