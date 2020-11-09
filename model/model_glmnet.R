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

source('~/temp-sentiment/model/run_model.R')

setwd('~/tweets/mod-res/')


#Try once by specifying my own interactions (rather than letting sparse.model.matrix()) specify interactions

#Make temp.hi vars
all$temp.hi.gt25 <- all$temp.hi * (all$temp.hi > 25)
all$temp.hi.lte25 <- all$temp.hi * (all$temp.hi <= 25)
all$temp.hi.gt25.int <- all$temp.hi > 25
all$temp.hi.lte25.int <- all$temp.hi <= 25

all$precip.gt0 <- all$precip * (all$precip > 0)
all$precip.gt0.int <- all$precip > 0
all$precip.eq0 <- all$precip == 0

all$srad.gt0 <- all$srad * (all$srad > 0)
all$srad.gt0.int <- all$srad > 0
all$srad.eq0 <- all$srad == 0

all$monthstate <- paste0(all$state, '-', all$month)

mm <- sparse.model.matrix(vader ~ 1 + temp.hi.gt25 + temp.hi.lte25 + temp.hi.intercept + 
                             srad.gt0 + srad.eq0 + precip.gt0 + precip.eq0 + 
                             monthstate + 
                             dow + doy + fips +
                             tod + year, 
                           data=all, 
                           verbose=T, 
                           drop.unused.levels=T)

# Dies on TOD.  Try again on bigger server, see if it makes it further

mod <- glmnet(mm, data$vader, family="gaussian", alpha=0, lambda=0)



# All Together!
all_res <- runEvalGlmnet(formula=vader ~ 1 + temp.hi.gt25 + temp.hi.lte25 + temp.hi.intercept + 
                             srad*srad.gt0 + srad.eq0 + precip.gt0 + precip.eq0 + 
                             monthstate + 
                             dow + doy + fips +
                             tod + year,
              filterWeather=TRUE,
              data=all,
              cut_qs=list("srad" = c(-0.1, 0.001, 1500),
                          "precip" = c(-0.1, 0.001, 100),
                          "temp.hi" = c(-33, 25, 55)),
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
