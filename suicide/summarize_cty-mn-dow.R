library(data.table)
library(lubridate)

##################################
# read sentiment data from twitter
##################################
dat <- fread('~/tweets/all.csv')

dat_dow <- dat[ , .(afinn=mean(afinn), hedono=mean(hedono), vader=mean(vader), n=length(vader)), .(fips, statemonth, dow, year)]

dat_dow$month <- substr(dat_dow$statemonth, 4, 5)
dat_dow$statemonth <- NULL

##################################
# Read in population data
#################################
pop <- bind_rows(lapply(list.files('~/tweets/suicide', pattern='^Population.*txt$', full.names=T),
                        read.delim)) %>%
  select(Population, fips=County.Code, year=Yearly.July.1st.Estimates) %>%
  filter(year >= 2009, year <= 2019)

##############################
# Combine and write
##############################
all <- merge(dat_dow, pop)

fwrite(all, '~/tweets/summary_dow.csv', row.names=F)
