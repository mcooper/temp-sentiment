library(tidyverse)

setwd('~/tweets/mod-res/')

options(stringsAsFactors=F)

MOD_RUN <- 'weather_income_full'
catname <- 'income_percap'

pred <- read.csv(paste0(MOD_RUN, '_preds.csv'))

coefs <- read.csv(paste0(MOD_RUN, '_coefs.csv'))

#####################################
# Determine model parameters/ set up
#####################################

pred$category <- pred[ , catname]
pred[ , catname] <- NULL

#Determine categories
cat <- coefs$names[grepl('precip_', coefs$names)]
cat <- unique(gsub('precip_|_\\d{1,2}\\.\\d*', '', cat))
cat <- cat[order(cat)]

#Determine precip knots
pcp_knt <- coefs$names[grepl(paste0(paste0('precip_', cat), collapse='|'), coefs$names)]
pcp_knt <- gsub(paste0(paste0('precip_', cat, '_'), collapse='|'), '', pcp_knt)
pcp_knt <- as.numeric(unique(pcp_knt))

#Determine temp.hi knots
tmp_knt <- coefs$names[grepl(paste0(paste0('temp.hi_', cat), collapse='|'), coefs$names)]
tmp_knt <- gsub(paste0(paste0('temp.hi_', cat, '_'), collapse='|'), '', tmp_knt)
tmp_knt <- as.numeric(unique(tmp_knt))

#Determine srad knot
srd_knt <- coefs$names[grepl(paste0(paste0('srad_', cat), collapse='|'), coefs$names)]
srd_knt <- gsub(paste0(paste0('srad_', cat, '_'), collapse='|'), '', srd_knt)
srd_knt <- as.numeric(unique(srd_knt))

###################################
#Make modex matrix of predictions
###################################
predX <- cbind(data.frame(intercept=1), pred[ , c('temp.hi', 'precip', 'srad')])

#Make segmented spline features
for (ct in cat){
  #Precip
  for (kt in pcp_knt){
    val <- pmax(0, predX$precip - kt)*(pred$category == ct)
    predX <- cbind(predX, val)
    names(predX)[length(names(predX))] <- paste0('precip_', ct, '_', kt)
  }

  #Temp.hi
  for (kt in tmp_knt){
    val <- pmax(0, predX$temp.hi - kt)*(pred$category == ct)
    predX <- cbind(predX, val)
    names(predX)[length(names(predX))] <- paste0('temp.hi_', ct, '_', kt)
  }

  #srad
  for (kt in srd_knt){
    val <- pmax(0, predX$srad - kt)*(pred$category == ct)
    predX <- cbind(predX, val)
    names(predX)[length(names(predX))] <- paste0('srad_', ct, '_', kt)
  }
}

#Add dummy vars for cats
for (ct in cat[2:length(cat)]){
  predX[ , paste0('x0_', ct)] <- (pred$category == ct)
}

#################################
#Make Predictions
#############################
pred$prediction <- NA
pred$std.err <- NA

for (i in 1:nrow(pred)){
  pred$prediction[i] <- sum(predX[i, ]*coefs$coefs[1:nrow(pred)])
  pred$std.err[i] <- sum(predX[i, ]*coefs$standarderror[1:nrow(pred)])
}

pred$ymin <- pred$prediction - pred$std.err*qnorm(0.975)
pred$ymax <- pred$prediction + pred$std.err*qnorm(0.975)

###########################
# Make Plots
###########################

## YOU CANT GET PREDICITON SE FROM COEF SEs!!
## https://stats.stackexchange.com/questions/64069/can-we-calculate-the-standard-error-of-prediction-just-based-on-simple-linear-re?rq=1

#temp.hi
ggplot(pred %>% filter(precip == 0, srad == 0)) + 
  geom_line(aes(x=temp.hi, y=prediction, color=category)) + 
  geom_ribbon(aes(x=temp.hi, ymax=ymax, ymin=ymin, fill=category), color=NA, alpha=0.1)

#precip
ggplot(pred %>% filter(temp.hi == 0, srad == 0)) + 
  geom_line(aes(x=precip, y=prediction, color=category)) +
  geom_ribbon(aes(x=precip, ymax=ymax, ymin=ymin, fill=category), color=NA, alpha=0.1)

#srad
ggplot(pred %>% filter(precip == 0, temp.hi == 0)) + 
  geom_line(aes(x=srad, y=prediction, color=category)) +
  geom_ribbon(aes(x=srad, ymax=ymax, ymin=ymin, fill=category), color=NA, alpha=0.1)

