library(data.table)

dat <- fread('~/tweets/all.csv')

cty <- read.csv('~/tweets/csa/fips-csa.csv')

cincinnati_fips <- cty$fips[cty$id == 178]

sel <- dat[dat$fips %in% cincinnati_fips, ]

sel$month <- substr(sel$statemonth, 4, 5)

sel2 <- sel[sel$month == '05', ]

sel3 <- sel2[sel2$weather_term == 0 , 
             .(temp = mean(temp), 
               prcp = mean(prcp), 
               srad = mean(srad), 
               temp.hi = mean(temp.hi),
               afinn = mean(afinn),
               hedono = mean(hedono),
               vader = mean(vader),
               income_percap = mean(income_percap),
               race_white = mean(race_white)),
             .(fips, dow, year, doy)]

fwrite(sel3, '~/tweets/cincinnati_sel.csv', row.names=F)
