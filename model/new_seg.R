
destfile='/nfs/textdataclimateshocks-data/data/processed/patents/sample/all_sample_1pct.csv'
url='https://restart001.blob.core.windows.net/mortalityblob/sample_tweets/all_samp_1pct.csv'
download.file(url, destfile,quiet=FALSE)

destfile='/nfs/textdataclimateshocks-data/data/processed/patents/sample/all_sample_10pct.csv'
url='https://restart001.blob.core.windows.net/mortalityblob/sample_tweets/all_samp_10pct.csv'
download.file(url, destfile,quiet=FALSE)


library(data.table)
library(glmnet)
library(Matrix)
library(ggplot2)


all <- fread('/nfs/textdataclimateshocks-data/data/processed/patents/sample/all_sample_1pct.csv')

#####################################################################################
#                       Model Heat Index With National Quintiles
#####################################################################################

for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){
  
  sub=all[all$income_percap_q == q, ]
  sub=sub[sub$temp.hi> -20 & sub$temp.hi <= 45, ]
  sub$tmin_q=cut(sub$temp.hi,seq(-20,45,5))
  mm <- sparse.model.matrix(vader ~ 1 +ppt+ temp.hi+temp.hi:tmin_q + factor(fips) + dow + doy+ tod 
                              + year*daynum + year, data=sub[sub$temp.hi> -20 & sub$temp.hi <= 45, ])
  

  mod <- glmnet(mm, sub$vader[sub$temp.hi > -20 & sub$temp.hi <= 45 ],family="gaussian",
                  alpha=0, lambda=0)
  
Heat_mod[[q]]=mod

fips_levels <- unique(sub$fips)
fips_levels <- fips_levels[order(fips_levels)]

doy_levels <- unique(sub$doy)
doy_levels <- doy_levels[order(doy_levels)]

dow_levels <- unique(sub$dow)
dow_levels <- dow_levels[order(dow_levels)]

tod_levels <- unique(sub$tod)
tod_levels <- tod_levels[order(tod_levels)]

preddf <- expand.grid(list(temp.hi=-19:45))
preddf$fips=factor('1001', levels=factor(fips_levels))
preddf$doy=factor('12-01', levels=factor(doy_levels))
preddf$dow=factor('Friday', levels=factor(dow_levels))
preddf$tod=factor('02:1', levels=factor(tod_levels))
preddf$daynum=1000
preddf$year=2018
preddf$ppt <- 15
preddf$tmin_q=cut(preddf$temp.hi,seq(-20,45,5))

pmm[[q]] <- sparse.model.matrix( ~ 1 +ppt+ temp.hi+temp.hi:tmin_q + fips + dow + doy+ tod 
                           + year*daynum + year, data=preddf)
}

plotdata=data.frame()
for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){
  mod=Heat_mod[[q]]
  pm=pmm[[q]]
  x=data.frame(Temp=c(-19:45),vader=predict(mod,pm)[,1],income_q=q)
  plotdata=rbind(plotdata,x)
}
  
ggplot(plotdata, aes(x=Temp, y=vader))+ theme_bw() +geom_smooth(aes(color=income_q))+
  scale_color_discrete(limits=c("Richest", "Richer","Medium",  "Poorer", "Poorest"))  
  
  
  




##############################################################################################
#                                         Model Precipitation                                #
##############################################################################################


for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){
  sub=all[all$income_percap_q == q, ]
  sub=sub[sub$temp.hi> -1 & sub$temp.hi <= 199, ]
  sub$pmin_q=cut(sub$ppt,seq(-1,199,20))
  sub=sub[which(!is.na(sub$pmin_q)),]
  mm <- sparse.model.matrix(vader ~ 1 + ppt + pmin_q:ppt + temp.hi + factor(fips) + dow + doy 
                              + year*daynum + year, data=sub)
    
  mod <- glmnet(mm, sub$vader, family="gaussian", alpha=0, lambda=0)
    

    fitlist[[q]]=mod
    
    fips_levels <- unique(sub$fips)
    fips_levels <- fips_levels[order(fips_levels)]
    
    doy_levels <- unique(sub$doy)
    doy_levels <- doy_levels[order(doy_levels)]
    
    dow_levels <- unique(sub$dow)
    dow_levels <- dow_levels[order(dow_levels)]
    
    tod_levels <- unique(sub$tod)
    tod_levels <- tod_levels[order(tod_levels)]
    
    preddf <- expand.grid(list(ppt=0:199))
    preddf$fips=factor('1001', levels=factor(fips_levels))
    preddf$doy=factor('12-01', levels=factor(doy_levels))
    preddf$dow=factor('Friday', levels=factor(dow_levels))
    preddf$tod=factor('02:1', levels=factor(tod_levels))
    preddf$daynum=1000
    preddf$year=2018
    preddf$temp.hi <- 20
    preddf$pmin_q=cut(preddf$ppt,seq(-1,199,20))
    
    pmm[[q]] =sparse.model.matrix( ~ 1 +ppt+pmin_q:ppt+ temp.hi + fips + dow + doy 
                                     + year*daynum + year, data=preddf)
}
  
plotdata=data.frame()
for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){
  mod=fitlist[[q]]
  pm=pmm[[q]]
  x=data.frame(ppt=c(0:199),vader=predict(mod,pm)[,1],income_q=q)
  plotdata=rbind(plotdata,x)
}

ggplot(plotdata, aes(x=ppt, y=vader))+ theme_bw() +geom_smooth(aes(color=income_q))+
  scale_color_discrete(limits=c("Richest", "Richer","Medium",  "Poorer", "Poorest"))  




