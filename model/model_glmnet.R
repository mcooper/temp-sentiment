library(data.table)
library(glmnet)
library(Matrix)

all <- fread('~/tweets/all.csv')

####################
# Model Heat
####################
mm <- sparse.model.matrix(hedono ~ 1 + temp.hi + income_percap_q + temp.hi*income_percap_q + FIPS + dow + month + daynum, data=all[all$temp.hi > 20, ])

mod <- glmnet(mm, all$hedono[all$temp.hi > 20], family="gaussian", alpha=0, lambda=0)

save(mod, file='~/tweets/heat_inco.Rdata')

rm(mm)
gc()

####################
# Model Cold
####################
mm <- sparse.model.matrix(hedono ~ 1 + temp + income_percap_q + temp*income_percap_q + FIPS + dow + month + daynum, data=all[all$temp <= 20, ])

mod <- glmnet(mm, all$hedono[all$temp <= 20], family="gaussian", alpha=0, lambda=0)

save(mod, file='~/tweets/cold_inco.Rdata')

rm(mm)
gc()

#####################
# Model Precipitation
#####################
mm <- sparse.model.matrix(hedono ~ 1 + ppt + income_percap_q + ppt*income_percap_q + FIPS + dow + month + daynum, data=all)

mod <- glmnet(mm, all$hedono, family="gaussian", alpha=0, lambda=0)
save(mod, file='~/tweets/rain_inco.Rdata')
