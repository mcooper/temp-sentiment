library(tidyverse)
library(sf)

fips <- read_sf('~/tweets/csa/', 'USA_Counties') %>%
  st_centroid %>%
  select(FIPS, POPULATION)

csa <- read_sf('~/tweets/csa/', 'Combined_Statistical_Areas') %>%
  select(CSA)

j <- st_join(fips, csa, join=st_within) %>%
  filter(!is.na(CSA)) %>%
  group_by(CSA) %>%
  mutate(CSA_POPULATION = sum(POPULATION)) %>%
  select(-POPULATION) %>%
  st_drop_geometry

write.csv(j, '~/tweets/csa/fips-csa.csv', row.names=F)
