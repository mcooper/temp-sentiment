library(data.table)
library(dplyr)
library(lubridate)
library(fixest)

# TODO: Set Working Directory
setwd('')

##############################################
# read summarized sentiment data from twitter
##############################################
dat <- fread('~/tweets/summary_dow.csv')
dat$state <- substr(100000 + dat$fips, 2, 3)
dat$statemonth <- paste0(dat$state, '_', dat$month)

###############################
# read suicide data
###############################

# TODO: read in data
sui <- fread('')

# TODO: Summarize suicide data to:
#   - county-year-month-dow
#   - count of suicides
#   - count of other deaths indicative of poor mental health


######################################
# Combine
###################################

comb <- merge(sui, dat)

##################################
# Model
#################################3

mod_vader <- feglm(suicides ~ vader + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')
mod_hedono <- feglm(suicides ~ hedono + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')
mod_afinn <- feglm(suicides ~ afinn + log(Population) | dow + year + state + month + statemonth, data=comb, family='poisson')

saveRDS(mod_vader, 'mod_vader.RDS')
saveRDS(mod_hedono, 'mod_hedono.RDS')
saveRDS(mod_afinn, 'mod_afinn.RDS')
