library(tidyverse)
library(cowplot)
library(survey)
library(data.table)

options(scipen=100)

MOD_RUN <- 'afinn_wbgt_nointeract'

setwd(paste0('~/tweets/bootstrap/', MOD_RUN))

data <- fread('~/tweets/all.csv')

data <- data[weather_term == 0, ]

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

data$raining <- data$prcp > 0

#Save memory by deleting useless columns!
# data <- data[ , c('wbgt', 'raining', 'srad', 'afinn', 'fips', 'year', 
#                   'statemonth', 'tod', 'doy', 'dow', 'temp')]

######################################
# Define Functions
#####################################
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

#######################################
# Get plot accessories
#######################################

ref_temps <- NULL
for (t in seq(-20, 30, by=10)){
  ref_temps <- c(ref_temps, median(data$temp[data$wbgt > (t - 0.25) & data$wbgt < (t + 0.25)]))
}
ref_temps <- round(ref_temps)


########################################
# WBGT
#########################################
preddf <- data.frame(wbgt=seq(-22, 33))

preddf$afinn <- 1

mm <- model.matrix(as.formula(paste0("afinn ~ ",
                   piece.formula("wbgt", knots[['wbgt']], ""))),
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

preddf <- preddf %>%
  gather(key, value, -wbgt, -afinn) %>%
  #Normalize so that graph shows difference from wbgt = X
  group_by(key) %>%
  mutate(value = value - value[wbgt == 5]) %>%
  #Get ymin, ymax, and mean
  group_by(wbgt) %>%
  summarize(ymin = quantile(value, probs=0.025),
            ymax = quantile(value, probs=0.975),
            pred = quantile(value, probs=0.5))

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=pred)) + 
  geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-22, 33)) + 
  labs(x='', y='Change in Mood of Tweet') +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"),
        panel.grid.major = element_line(color = "lightgrey", size=0.5),
        panel.grid.minor = element_line(color = "lightgrey", size=0.25))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-22, 33)) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}, expand=c(0,0),
                     breaks=c(75000)) + 
  labs(y="Tweet\nCount", x="Wet Bulb Globe Temperature (C)") + 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "lightgrey", size=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_line(color = "lightgrey", size=0.25))


x2 <- ggplot() +
  scale_x_continuous(expand=c(0, 0), limits=c(-22, 33), labels=ref_temps) + 
  labs(x="Median Dry Bulb (Normal) Temperature (C)") + 
  theme_classic()

x <- get_x_axis(x2)
xl <- get_plot_component(x2, "xlab-b")

plot_grid(curve, hist, ggdraw(x), ggdraw(xl), align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2, 0.04, 0.04))
ggsave('~/temp-sentiment/res/afinn-wbgt.png', width=4.5, height=4)

