library(data.table)
library(glmnet)
library(Matrix)
library(ggplot2)

all <- fread('~/tweets/all_samp_1pct.csv', stringsAsFactors=T)
all <- data.frame(all)

all$state <- as.factor(substr(all$fips + 100000, 2, 3)) #Use state-level FEs, since we dont have enough data for highly local FSs

#Make sure to make factors of vars that are numeric
all$year <- as.factor(all$year)
all$fips <- as.factor(all$fips)

source('~/temp-sentiment/model/run_model.R')

# Model Temperature
temp_res <- runEvalGlmnet(formula=vader ~ 1 + temp*temp_q + state + dow + 
                          doy + tod + year*daynum + year,
                data=all, 
                cut_qs=list("temp" = c(-33,  11, 17, 22, 27, 48)),
                control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'daynum'),
                outcome_var='vader',
                save=NULL,
                plot=TRUE)

# Model Heat Index
temp.hi_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q + state + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("temp.hi" = c(-33, -0.5, 3.5, 8.5, 12.5, 16.5, 19.5, 23, 26, 30.5, 55)),
              control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'daynum'),
              interact_var=NULL,
              outcome_var='vader',
              plot=TRUE)

# Model Precip
precip_res <- runEvalGlmnet(formula=vader ~ 1 + precip*precip_q + state + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("precip" = c(-0.1, 0.5, 1, 10, 100)),
              control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'daynum'),
              interact_var=NULL,
              outcome_var='vader',
              plot=TRUE)

# Model Cloudiness
srad_res <- runEvalGlmnet(formula=vader ~ 1 + srad*srad_q + state + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("srad" = c(-0.1, 50, 200, 500, 750, 1500)),
              control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'daynum'),
              interact_var=NULL,
              outcome_var='vader',
              plot=TRUE)

# All Together!
# Not enough memory to test locally :-(
all_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q + 
                             srad*srad_q + precip*precip_q + 
                             state + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("srad" = c(-0.1, 50, 200, 500, 750, 1500),
                          "precip" = c(-0.1, 0.5, 1, 10, 100),
                          "temp.hi" = c(-33, -0.5, 3.5, 8.5, 12.5, 16.5, 19.5, 23, 26, 30.5, 55)),
              control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'daynum'),
              interact_var=NULL,
              outcome_var='vader',
              plot=TRUE)

#################################
# Interact Income
#################################

# Model Heat Index
temp.hi_income_percap_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q*income_percap + 
                                 state + dow + doy + 
                                 tod + year*daynum + year,
              data=all, 
              cut_qs=list("temp.hi" = c(-33, -0.5, 3.5, 8.5, 12.5, 16.5, 19.5, 23, 26, 30.5, 55)),
              control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'daynum'),
              interact_var='income_percap',
              outcome_var='vader',
              plot=TRUE)
