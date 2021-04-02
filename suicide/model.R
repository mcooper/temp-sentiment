library(data.table)
library(dplyr)
library(lubridate)
library(fixest)

# Set Working Directory
setwd('C:/Users/jeos6786/Documents/SESYNC')

##############################################
# read summarized sentiment data from twitter
##############################################
dat <- fread('summary_dow.csv')
dat$state <- substr(100000 + dat$fips, 2, 3)
dat$statemonth <- paste0(dat$state, '_', dat$month)

###############################
# read suicide data
###############################

# read in data

sui2009 <- read.csv("R:/County_Mort/mortality/2009sui2_sesync.csv")
sui2010 <- read.csv("R:/County_Mort/mortality/2010sui2_sesync.csv")
sui2011 <- read.csv("R:/County_Mort/mortality/2011sui2_sesync.csv")
sui2012 <- read.csv("R:/County_Mort/mortality/2012sui2_sesync.csv")
sui2013 <- read.csv("R:/County_Mort/mortality/2013sui2_sesync.csv")
sui2014 <- read.csv("R:/County_Mort/mortality/2014sui2_sesync.csv")
sui2015 <- read.csv("R:/County_Mort/mortality/2015sui2_sesync.csv")
sui2016 <- read.csv("R:/County_Mort/mortality/2016sui2_sesync.csv")
sui2017 <- read.csv("R:/County_Mort/mortality/2017sui2_sesync.csv")

sui <- bind_rows(sui2009, sui2010, sui2011, sui2012, sui2013, sui2014, sui2015, sui2016, sui2017)

######################################
# Combine
###################################
sub <- dat[dat$year==2009,]
comb <- merge(sui, sub, all.y = T)
sapply(comb, function(x){sum(is.na(x))})
comb[is.na(comb)] <- 0 #adds zeros to missing/NA values of mort from sui

##################################
# Model
#################################3

# Include log of population - https://stats.stackexchange.com/questions/52352/poisson-regression-dealing-with-widely-different-population-sizes

#sui
#mort
#

mod_vader <- feglm(sui ~ vader + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')
mod_hedono <- feglm(sui ~ hedono + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')
mod_afinn <- feglm(sui ~ afinn + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')
summary(mod_vader)
summary(mod_hedono)
summary(mod_afinn)

mod_vader <- feglm(mort ~ vader + log(Population) | fips + dow + year + state + month + statemonth, data=comb, family='poisson')
mod_hedono <- feglm(mort ~ hedono + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')
mod_afinn <- feglm(mort ~ afinn + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')


saveRDS(mod_vader, 'mod_vader.RDS')
saveRDS(mod_hedono, 'mod_hedono.RDS')
saveRDS(mod_afinn, 'mod_afinn.RDS')
