## Need to install Intel MKL
## https://www.r-bloggers.com/2018/04/18-adding-intel-mkl-easily-via-a-simple-script/

library(data.table)
library(glmnet)
library(Matrix)
library(ggplot2)

all <- fread('~/tweets/all.csv', stringsAsFactors=T)
all <- data.frame(all)

#Exclude bad time of day levels
all <- all[!all$tod %in% c("CDT", "CST", "EDT", "EST", "MDT", "MST", "PDT", "PST"), ]

#Make Control Vars
all$month <- substr(all$doy, 1, 2)
all$state <- substr(10000 + all$fips, 2, 3)
all$year <- as.factor(all$year)
all$fips <- as.factor(all$fips)

#Convert All to Boolean
all$temp.hi_q <- cut(all$temp.hi, c(-33, 0, 20, 35, 55))
all$precip_q <- cut(all$precip, c(-0.1, 0.0001, 1, 75))
all$srad_q <- cut(all$srad, c(-0.1, 0.0001, 400, 1400))

source('~/temp-sentiment/model/run_model.R')

setwd('~/tweets/mod-res/')

mm <- sparse.model.matrix(vader ~ 1 + temp.hi_q +  
                             srad_q + precip_q + 
                             month*state + 
                             dow + doy + fips +
                             tod + year, data=all, verbose=T, drop.unused.levels=T)

mod <- glmnet(mm, data$vader, family="gaussian", alpha=0, lambda=0)

#Try once by specifying my own interactions (rather than letting sparse.model.matrix()) specify interactions

# All Together!
all_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi*temp.hi_q +  
                             srad*srad_q + precip*precip_q + 
                             month*state + 
                             dow + doy + fips +
                             tod + year,
              filterWeather=FALSE,
              data=all,
              cut_qs=list("srad" = c(-0.1, 0.001, 1500),
                          "precip" = c(-0.1, 0.001, 100),
                          "temp.hi" = c(-3300, 2500, 5500)),
              control_vars=c('state', 'dow', 'doy', 'tod', 'year', 'month', 'fips'),
              outcome_var='vader',
              save='all.Rdata',
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
