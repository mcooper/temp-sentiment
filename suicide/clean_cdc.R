library(tidyverse)

setwd('~/tweets/suicide')

suicides <- bind_rows(lapply(list.files(pattern='^Suicide.*txt$'), read.delim)) %>%
  filter(!is.na(Deaths)) %>%
  select(County=County.Code, Year, Month=Month.Code, Deaths)
pop <- bind_rows(lapply(list.files(pattern='^Population.*txt$'), read.delim)) %>%
  select(Population, County=County.Code, Year=Yearly.July.1st.Estimates)

comb <- merge(suicides, pop, all.x=T, all.y=F)

write.csv(comb, 'Suicides.csv', row.names=F)
