library(tidyverse)
library(haven)
library(lubridate)

setwd('~/tweets/brfss')

fs <- list.files()

all <- data.frame()
for (f in fs){
  print(f)
  d <- read_xpt(f)
  d <- d[ , c("_STATE", "IDATE", "MENTHLTH")]
  all <- bind_rows(all, d)
}

sel <- all %>%
  filter(!(is.na(MENTHLTH) | MENTHLTH %in% c(77, 99))) %>%
  mutate(date = mdy(IDATE),
         MENTHLTH = ifelse(MENTHLTH == 88, 0, MENTHLTH)) %>%
  select(-IDATE) %>%
  na.omit

write.csv(sel, 'brfss_mentalhealth.csv', row.names=F)
