library(data.table)
library(tidyverse)
library(lubridate)
library(fixest)
library(broom)

##################################
# read sentiment data from twitter
##################################
dat <- fread('~/tweets/all.csv')

dat$date <- paste0(dat$year, '-', dat$doy)

dat_day <- dat[ , .(afinn=mean(afinn), hedono=mean(hedono), vader=mean(vader), n=length(vader)), .(fips, date)]

###############################
# read crime data
###############################

crim <- fread('~/tweets/crime/crime_cod_nibrs.csv')
crim$date <- as.character(crim$date)

######################################
# Combine
###################################

comb <- merge(crim, dat_day, all.x=T)
comb$year <- as.numeric(substr(comb$date, 1, 4))
comb$state <- as.factor(floor(comb$fips/1000))
comb$month <- as.factor(substr(comb$date, 6, 7))

comb$countymonth <- paste0(comb$fips, '-', comb$month)
comb$stateyear <- paste0(comb$state, '-', comb$year)
comb$statemonth <- paste0(comb$state, '-', comb$month)
comb$dow <- as.factor(wday(ymd(comb$date)))
comb$doy <- substr(comb$date, 6, 10)

##################################
# Model
#################################3

gd <- expand.grid(list(out=c('violent', 'assault', 'homicides'),
                       src=c('nibrs', 'cod'),
                       ind=c('vader', 'afinn', 'hedono')))

for (i in 1:nrow(gd)){
  form <- as.formula(paste0(gd$out[i], '_', gd$src[i], ' ~ ', gd$ind[i], ' + log(population) | dow + doy + year + statemonth'))
  mod <- feglm(form, data=comb, family=poisson(link=log), weight=comb$n)
  td <- tidy(mod)
  gd$statistic[i] <- td[td$term == gd$ind[i], 'statistic', drop=T]
  gd$aic[i] <- AIC(mod)

  form <- as.formula(paste0(gd$out[i], '_', gd$src[i], ' ~ ', gd$ind[i], ' + log(population) | dow + doy + year + statemonth + fips'))
  mod <- feglm(form, data=comb, family=poisson(link=log), weight=comb$n)
  td <- tidy(mod)
  gd$statistic2[i] <- td[td$term == gd$ind[i], 'statistic', drop=T]
  gd$aic2[i] <- AIC(mod)
}

# Strong effects, until we control for FIPS
         out   src    ind   statistic statistic2        aic      aic2
1    violent nibrs  vader -13.5453717 16.1837008 1767637660 477729768
2    assault nibrs  vader -25.8231725  6.3352031  884795851 368529773
3  homicides nibrs  vader -13.8254127 -1.6684376   29946138  26150314
4    violent   cod  vader -11.3684215 -4.1938880  379249033 285038557
5    assault   cod  vader  -4.1869425 -0.8072122  323219418 258455786
6  homicides   cod  vader  -0.2787759  0.7107099   50680014  50161429
7    violent nibrs  afinn  -7.8900235 30.9332856 1767690065 477720023
8    assault nibrs  afinn -13.4956555  6.6119059  884582497 368528022
9  homicides nibrs  afinn -16.4669909 -0.9067878   29933542  26150619
10   violent   cod  afinn  -4.0147974  4.4679201  379559506 285070703
11   assault   cod  afinn  -1.2719049  3.2694930  323244510 258406842
12 homicides   cod  afinn  -0.3151255  1.0045080   50679991  50160083
13   violent nibrs hedono  -5.0744596  6.2316646 1767837181 477767294
14   assault nibrs hedono -10.3144823  2.6686240  885005542 368543515
15 homicides nibrs hedono -12.5209393  0.6446631   29974987  26151068
16   violent   cod hedono  -7.6982790  7.1845740  379372859 285001297
17   assault   cod hedono  -2.3361383  5.6428411  323217425 258337337
18 homicides   cod hedono  -2.2616862 -0.3306887   50671289  50162794

