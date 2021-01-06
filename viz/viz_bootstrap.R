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
  #Normalize so that graph shows difference from wbgt = X
  group_by(income_percap, key) %>%
  mutate(value = value - value[wbgt == 0]) %>%
  #Get ymin, ymax, and mean
  group_by(wbgt, income_percap) %>%
  summarize(ymin = quantile(value, probs=0.025),
            ymax = quantile(value, probs=0.975),
            pred = quantile(value, probs=0.5))

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=pred, color=income_percap)) + 
  geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(-22, 33)) + 
  #scale_y_continuous(breaks=seq(0, -0.02, -0.005)) + 
  labs(x='', y='Change in Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)') +
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
ggsave('~/temp-sentiment/res/wbgt-income.png', width=6, height=5)

########################################
# Precipitation
#########################################
preddf <- data.frame(prcp=seq(0, 3.2, 0.01))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("prcp", knots[['prcp']], ""), ' + ',
                   piece.formula("prcp", knots[['prcp']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap']

mods <- list.files()
#Iterate over models
for (modf in mods){

  mod <- readRDS(modf)

  #It looks like some precip columns were dropped bc of collinearity
  #AND the columns that were dropped varied depending on the bootstrap iteration
  #So, we must subset the columns differently for each model
  mms <- mm[ , colnames(mm) %in% names(coef(mod))]
  mmp <- lapply(seq_len(nrow(mms)), function(i) mms[i,])

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
  gather(key, value, -prcp, -income_percap, -vader) %>%
  #Normalize so that graph shows difference from prcp = X
  group_by(income_percap, key) %>%
  mutate(value = value - value[prcp == 0]) %>%
  #Get ymin, ymax, and mean
  group_by(prcp, income_percap) %>%
  summarize(ymin = quantile(value, probs=0.025),
            ymax = quantile(value, probs=0.975),
            pred = quantile(value, probs=0.5))

preddf$prcp <- log(1 + preddf$prcp*1000)
data$prcp_l <- log(1 + data$prcp*1000)
preddf$ymin[preddf$ymin < -0.41] <- -0.41

(curve <- ggplot(preddf) + 
  geom_line(aes(x=prcp, y=pred, color=income_percap)) + 
  geom_ribbon(aes(x=prcp, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.1) + 
  scale_x_continuous(expand=c(0, 0), labels=function(x){round((exp(x) - 1)/1000, 2)}) + 
  scale_y_continuous(expand=expansion(mult = c(0, 0.05), add=c(0, 0))) + 
  labs(x='', y='Change in Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)') +
  theme_classic() + 
  theme(legend.position=c(0.3, 0.4),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"),
        panel.grid.major = element_line(color = "lightgrey", size=0.5),
        panel.grid.minor = element_line(color = "lightgrey", size=0.25)))

(hist <- ggplot(data[sample(which(data$prcp_l < preddf$prcp), 
                            nrow(data)*0.01), 'prcp_l']) +
  geom_histogram(aes(x=prcp_l), bins=100) + 
  scale_x_continuous(expand=c(0, 0), labels=function(x){round((exp(x) - 1)/1000, 2)}) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')},
                     expand=c(0,0)) + 
  labs(y="Tweet\nCount", x="Precipitation (mm)") + 
  theme_classic() + 
  theme(panel.grid.major.x = element_line(color = "lightgrey", size=0.5),
        panel.grid.minor.x = element_line(color = "lightgrey", size=0.25),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()))

plot_grid(curve, hist, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/prcp-income.png', width=6, height=5)


########################################
# Sunshine
#########################################
preddf <- data.frame(srad=seq(0, 1300, 1))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("srad", knots[['srad']], ""), ' + ',
                   piece.formula("srad", knots[['srad']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) != '(Intercept)']
mm <- mm[ , colnames(mm) != 'income_percap']

#Iterate over models
for (modf in mods){

  mod <- readRDS(modf)

  #It looks like some precip columns were dropped bc of collinearity
  #AND the columns that were dropped varied depending on the bootstrap iteration
  #So, we must subset the columns differently for each model
  mms <- mm[ , colnames(mm) %in% names(coef(mod))]
  mmp <- lapply(seq_len(nrow(mms)), function(i) mms[i,])

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
  gather(key, value, -srad, -income_percap, -vader) %>%
  #Normalize so that graph shows difference from srad = X
  group_by(income_percap, key) %>%
  mutate(value = value - value[srad == 0]) %>%
  #Get ymin, ymax, and mean
  group_by(srad, income_percap) %>%
  summarize(ymin = quantile(value, probs=0.025),
            ymax = quantile(value, probs=0.975),
            pred = quantile(value, probs=0.5))

preddf$srad <- log(1 + preddf$srad)
data$srad_l <- log(1 + data$srad)
preddf$ymax[preddf$ymax > 0.5] <- 0.5
preddf$ymin[preddf$ymin < -0.5] <- -0.5

(curve <- ggplot(preddf) + 
  geom_line(aes(x=srad, y=pred, color=income_percap)) + 
  geom_ribbon(aes(x=srad, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.1) + 
  scale_x_continuous(expand=c(0, 0), labels=function(x){round((exp(x) - 1), 2)}) + 
  scale_y_continuous(expand=expansion(mult = c(0, 0.05), add=c(0, 0))) + 
  labs(x='', y='Change in Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)') +
  theme_classic() + 
  theme(legend.position=c(0.15, 0.25),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"),
        panel.grid.major = element_line(color = "lightgrey", size=0.5),
        panel.grid.minor = element_line(color = "lightgrey", size=0.25)))

(hist <- ggplot(data[sample(which(data$srad_l < max(preddf$srad)), 
                            nrow(data)*0.01), 'srad_l']) +
  geom_histogram(aes(x=srad_l), bins=100) + 
  scale_x_continuous(expand=c(0, 0), labels=function(x){round((exp(x) - 1), 2)}) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')},
                     expand=c(0,0)) + 
  labs(y="Tweet\nCount", x="Sunshine (W/m^2)") + 
  theme_classic() + 
  theme(panel.grid.major.x = element_line(color = "lightgrey", size=0.5),
        panel.grid.minor.x = element_line(color = "lightgrey", size=0.25),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()))

plot_grid(curve, hist, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/srad-income.png', width=6, height=5)

