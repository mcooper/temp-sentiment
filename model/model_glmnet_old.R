library(data.table)
library(glmnet)
library(Matrix)
library(ggplot2)

all <- fread('~/tweets/all.csv')

all$fips <- factor(all$fips)

##############################################
# Model Heat Index With National Quintiles
###########################################
mm <- sparse.model.matrix(hedono ~ 1 + temp.hi + income_percap_q + temp.hi*income_percap_q + fips + dow + doy + tod + year*daynum + year, data=all[all$temp.hi > 20, ])

mod <- glmnet(mm, all$hedono[all$temp.hi > 20], family="gaussian", alpha=0, lambda=0)

save(mod, file='~/tweets/heat_inco.Rdata')

rm(mm)
gc()

####################
# Model Cold
####################
mm <- sparse.model.matrix(hedono ~ 1 + temp + income_percap_q + temp*income_percap_q + fips + dow + doy + tod + year*daynum + year, data=all[all$temp <= 20, ])

mod <- glmnet(mm, all$hedono[all$temp <= 20], family="gaussian", alpha=0, lambda=0)

save(mod, file='~/tweets/cold_inco.Rdata')

rm(mm)
gc()

#####################
# Model Precipitation
#####################
mm <- sparse.model.matrix(vader ~ 1 + precip + income_percap + precip*income_percap + fips + dow + doy + year*daynum + year, data=all)

mod <- glmnet(mm, all$vader, family="gaussian", alpha=0, lambda=0)
save(mod, file='~/tweets/rain_inco.Rdata')


##############################################
# Model Daily Tmax
###########################################
mm <- sparse.model.matrix(hedono ~ 1 + tmax.hi + income_percap_q + tmax.hi*income_percap_q + fips + dow + doy + year*daynum + year, data=all[all$tmax.hi > 20, ])

mod <- glmnet(mm, all$hedono[all$tmax.hi > 20], family="gaussian", alpha=0, lambda=0)

save(mod, file='~/tweets/heat_daily_inco.Rdata')

rm(mm)
gc()

####################
# Model Daily Tmin
####################
mm <- sparse.model.matrix(hedono ~ 1 + tmax.hi + income_percap_q + tmax.hi*income_percap_q + fips + dow + doy + year*daynum + year, data=all[all$tmax.hi <= 20, ])

mod <- glmnet(mm, all$hedono[all$tmax.hi <= 20], family="gaussian", alpha=0, lambda=0)

save(mod, file='~/tweets/cold_daily_inco.Rdata')

rm(mm)
gc()

system('~/telegram.sh "Donezo!"')


