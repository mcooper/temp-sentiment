library(tidyverse)
library(cowplot)
library(survey)
library(data.table)
library(Hmisc)

MOD_RUN <- 'hedono_income_percap_q'

options(scipen=100)

setwd(paste0('~/tweets/bootstrap/', MOD_RUN, '/'))

data <- fread('~/tweets/all.csv')
data$income_percap_q <- cut2(data$income_percap, g=3)

data <- data[weather_term == 0, ]

data$raining <- data$prcp > 0

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)))

######################################
# Define Functions
#####################################
#Define Segmenting Function
# DIFFERENT FOR GROUPING VARS
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
qs <- c('[   17.6, 22263)', '[22263.6, 34129)', '[34129.4,539681]')
preddf <- make_groups(preddf, 'income_percap_q', qs)

preddf$hedono <- 1

mm <- model.matrix(as.formula(paste0("hedono ~ ", 
                     piece.formula("wbgt", knots[['wbgt']], "income_percap_q", 'factor'), ' + ',
                     piece.formula("wbgt", knots[['wbgt']], "", 'none'))),
                   data=preddf)


mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap_q[22263.6, 34129)']
mm <- mm[ , colnames(mm) != 'income_percap_q[34129.4,539681]']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

#Iterate over models
mods <- list.files()
for (modf in mods){

  mod <- readRDS(modf)

  res <- as.data.frame(svycontrast(mod, mmp))[ , 'contrast', drop=F]
  names(res) <- modf
  
  preddf <- cbind(preddf, res)
}

preddf$income_percap_q <- factor(preddf$income_percap_q)
levels(preddf$income_percap_q) <- c('Bottom', 'Middle', 'Top')

preddf <- preddf %>%
  gather(key, value, -wbgt, -income_percap_q, -hedono) %>%
  #Normalize so that graph shows difference from wbgt = X
  group_by(income_percap_q, key) %>%
  mutate(value = as.numeric(value),
         value = value - value[wbgt == 5]) %>%
  #Get ymin, ymax, and mean
  group_by(wbgt, income_percap_q) %>%
  dplyr::summarize(ymin = quantile(value, probs=0.025),
                   ymax = quantile(value, probs=0.975),
                   pred = quantile(value, probs=0.5))

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=pred, color=income_percap_q)) + 
  geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax, fill=income_percap_q), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-22, 33)) + 
  #scale_y_continuous(breaks=seq(0, -0.02, -0.005)) + 
  labs(x='', y='Change in Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\nTercile',
       color = 'Census Block\nIncome Per-Capita\nTercile') +
  theme_classic() + 
  theme(legend.position=c(0.3, 0.4),
        axis.text.x = element_blank(),
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
ggsave('~/temp-sentiment/res/hedono-wbgt-income_q.png', width=6, height=5)
