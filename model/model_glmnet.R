library(data.table)
library(glmnet)
library(Matrix)
library(ggplot2)

all <- fread('~/tweets/all_samp_1pct.csv', stringsAsFactors=T)
all <- data.frame(all)

#Make sure to make factors of vars that are numeric
all$year <- as.factor(all$year)
all$fips <- as.factor(all$fips)

source('~/temp-sentiment/model/run_model.R')

setwd('~/tweets/res/')

# Model Temperature
temp_res <- runEvalGlmnet(formula=vader ~ 1 + temp*temp_q + fips + dow + 
                          doy + tod + year*daynum + year,
                data=all, 
                cut_qs=list("temp" = c(-33, 5, 15, 24, 48)),
                control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
                outcome_var='vader',
                save='temp.Rdata',
                plot=TRUE)

# Model Heat Index
temp.hi_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q + fips + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("temp.hi" = c(-33, 5, 15, 24, 55),
              control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
              save='temp.hi.Rdata',
              outcome_var='vader',
              plot=TRUE)

# Model Precip
precip_res <- runEvalGlmnet(formula=vader ~ 1 + precip*precip_q + fips + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("precip" = c(-0.1, 1, 10, 100)),
              control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
              save='precip.Rdata',
              outcome_var='vader',
              plot=TRUE)

# Model Cloudiness
srad_res <- runEvalGlmnet(formula=vader ~ 1 + srad*srad_q + fips + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("srad" = c(-0.1, 50, 200, 500, 750, 1500)),
              control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
              save='srad.Rdata',
              outcome_var='vader',
              plot=TRUE)

# All Together!
all_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q + 
                             srad*srad_q + precip*precip_q + 
                             fips + dow + doy + 
                             tod + year*daynum + year,
              data=all, 
              cut_qs=list("srad" = c(-0.1, 50, 200, 500, 750, 1500),
                          "precip" = c(-0.1, 1, 10, 100),
                          "temp.hi" = c(-33, 5, 15,  24, 55)),
              control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
              save='all.Rdata',
              outcome_var='vader',
              plot=TRUE)

# #################################
# # Interact Income
# #################################
# 
# # Model Heat Index
# temp.hi_income_percap_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q*income_percap + 
#                                  fips + dow + doy + 
#                                  tod + year*daynum + year,
#               data=all, 
#               cut_qs=list("temp.hi" = c(-33, -0.5, 3.5, 8.5, 12.5, 16.5, 19.5, 23, 26, 30.5, 55)),
#               control_vars=c('fips', 'dow', 'doy', 'tod', 'year', 'daynum'),
#               interact_var='income_percap',
#               outcome_var='vader',
#               plot=TRUE)
