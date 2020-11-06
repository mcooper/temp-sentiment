
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


all <- fread('/nfs/textdataclimateshocks-data/data/processed/patents/sample/all_sample_10pct.csv')

#####################################################################################
#                       Model Heat Index With National Quintiles
#####################################################################################
unique(all$income_percap_q)

Heat_plot=data.frame()
for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){

  sub=all[all$income_percap_q == q, ]
  fitlist=list()
  pred=list()
  i=1
  for (tmin in seq(20,45,5)){
    print(tmin)
    mm <- sparse.model.matrix(vader ~ 1 +ppt+ temp.hi + fips + dow + doy+ tod 
                            + year*daynum + year, data=sub[sub$temp> tmin & sub$temp <= tmin+5, ])
  
    mod <- glmnet(mm, sub$vader[sub$temp > tmin & sub$temp<= tmin+5 ],family="gaussian",
                alpha=0, lambda=0)
    newxdata=data.frame()
    x=as.matrix(t(mm[1,]))
    newxdata=rbind(x,x)
    newxdata[1,3]=tmin
    newxdata[2,3]=tmin+5
    pred[[i]]=predict(mod,newx=newxdata,type='response')
    fitlist[[i]]=mod
    rm(mm)
    rm(mod)
    i=i+1
  }

  predplot=data.frame()
  predplot[1,1]=20
  predplot[1,2]=pred[[1]][1]
  predplot[1,3]=q
  i=2
  for (t in c(2:5)){
    pred[[t]][1]=pred[[t-1]][2]
    pred[[t]][2]=pred[[t-1]][2]-pred[[t]][1]+pred[[t]][2]
    predplot[i,1]=15+t*5
    predplot[i,2]=pred[[t]][1]
    predplot[i,3]=q
    i=i+1
  }
  Heat_plot=rbind(Heat_plot,predplot)
}


######################################################################################
#                                   Model Cold
######################################################################################
unique(all$income_percap_q)
Cold_plot=data.frame()
for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){
  
  sub=all[all$income_percap_q == q, ]
  fitlist=list()
  pred=list()
  i=1
  for (tmin in seq(0,20,5)){
    print(tmin)
    mm <- sparse.model.matrix(vader ~ 1 +ppt+ temp + fips + dow + doy
                              + tod + year*daynum + year, data=sub[sub$temp > tmin & sub$temp <= tmin+5, ])
    
    mod <- glmnet(mm, sub$vader[sub$temp > tmin & sub$temp <= tmin+5 ], family="gaussian", alpha=0, lambda=0)
    newxdata=data.frame()
    x=as.matrix(t(mm[1,]))
    newxdata=rbind(x,x)
    newxdata[1,3]=tmin
    newxdata[2,3]=tmin+5
    pred[[i]]=predict(mod,newx=newxdata,type='response')
    fitlist[[i]]=mod
    rm(mm)
    rm(mod)
    i=i+1
  }
  
  predplot=data.frame()
  predplot[1,1]=0
  predplot[1,2]=pred[[1]][1]
  predplot[1,3]=q
  i=2
  for (t in c(2:5)){
    pred[[t]][1]=pred[[t-1]][2]
    pred[[t]][2]=pred[[t-1]][2]-pred[[t]][1]+pred[[t]][2]
    predplot[i,1]=-5+t*5
    predplot[i,2]=pred[[t]][1]
    predplot[i,3]=q
    i=i+1
  }
  Cold_plot=rbind(Cold_plot,predplot)
}


##############################################################################
#                           Plot for Heat+Cold 
##############################################################################
Heat_plot2=Heat_plot
for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){

  lag=Cold_plot[Cold_plot$V3==q,][5,2]-Heat_plot[Heat_plot$V3==q,][1,2]
  Heat_plot2[Heat_plot2$V3==q,2]=Heat_plot2[Heat_plot2$V3==q,2]+lag
  
  }

All_plot=rbind(Cold_plot,Heat_plot2)
colnames(All_plot)=c('Temp','Vader','Income_q')
All_plot$Income_q <- factor(All_plot$Income_q, levels = c("Richest", "Richer","Medium",  "Poorer", "Poorest" ))
ggplot(All_plot[All_plot$Income_q=="Richest"|All_plot$Income_q=="Poorer",], 
       aes(x=Temp, y=Vader))+ theme_bw() +geom_line(aes(color=Income_q))


##############################################################################################
#                                         Model Precipitation                                #
##############################################################################################
ppt_plot=data.frame()

for (q in c("Medium",  "Poorest", "Poorer",  "Richest", "Richer" )){
  sub=all[all$income_percap_q == q, ]
  fitlist=list()
  pred=list()
  i=1
  for (pmin in seq(0,200,20)){
    print(pmin)
    mm <- sparse.model.matrix(vader ~ 1 + ppt + temp  + fips + dow + doy 
                              + year*daynum + year, data=sub[sub$ppt > pmin & sub$ppt <= pmin+20, ])

    mod <- glmnet(mm, sub$vader[sub$ppt > pmin & sub$ppt <= pmin+20 ], family="gaussian", alpha=0, lambda=0)
    
    newxdata=data.frame()
    x=as.matrix(t(mm[1,]))
    newxdata=rbind(x,x)
    newxdata[1,2]=pmin
    newxdata[2,2]=pmin+20
    pred[[i]]=predict(mod,newx=newxdata,type='response')
    fitlist[[i]]=mod
    rm(mm)
    rm(mod)
    i=i+1
  }
  
  predplot=data.frame()
  predplot[1,1]=0
  predplot[1,2]=pred[[1]][1]
  predplot[1,3]=q
  i=2
  for (t in c(2:11)){
    pred[[t]][1]=pred[[t-1]][2]
    pred[[t]][2]=pred[[t-1]][2]-pred[[t]][1]+pred[[t]][2]
    predplot[i,1]=-20+t*20
    predplot[i,2]=pred[[t]][1]
    predplot[i,3]=q
    i=i+1
  }
  ppt_plot=rbind(ppt_plot,predplot)
}



colnames(ppt_plot)=c('ppt','Vader','Income_q')
ppt_plot$Income_q <- factor(ppt_plot$Income_q, levels = c("Richest", "Richer","Medium",  "Poorer", "Poorest" ))

ggplot(ppt_plot, aes(x=ppt, y=Vader))+ theme_bw() +geom_line(aes(color=Income_q))

