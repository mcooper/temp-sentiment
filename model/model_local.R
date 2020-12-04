library(data.table)
library(glmnet)
library(Matrix)
library(ggplot2)
library(MatrixModels)

all <- fread('~/tweets/all_samp_1pct.csv', stringsAsFactors=T)
all <- data.frame(all)


form <- paste0("vader ~ income_percap*temp.hi + income_percap*I(pmax(temp.hi + 0, 0)) + income_percap*I(pmax(temp.hi +  15, 0)) + income_percap*I(pmax(temp.hi + 30, 0)) + statemonth + dow + doy + tod + year + fips")

mm <- sparse.model.matrix(as.formula(form), data=all)

mod <- MatrixModels:::lm.fit.sparse(mm, y=all$vader)


