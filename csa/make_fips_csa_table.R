library(tidyverse)
library(sf)
library(USAboundaries)

fips <- read_sf('~/tweets/csa/', 'USA_Counties') %>%
  filter(!STATE_NAME %in% c('Alaska', 'Hawaii', 'Puerto Rico')) %>%
  st_centroid %>%
  select(FIPS, POPULATION, BLACK)

msa <- read_sf('~/tweets/csa', 'fb19722c-442d-4b5e-8649-268605c4817b2020313-1-17yu7nk.hr3gl') %>%
  filter(CBSA_TYPE == 'Metropolitan') %>%
  select(MSA_ID = CBSA_ID, MSA_NAME = NAME)

csa <- read_sf('~/tweets/csa/', 'Combined_Statistical_Areas') %>%
  select(CSA_ID = CSA, CSA_NAME = NAME)

j <- st_join(fips, csa, join=st_within) %>%
  st_join(msa, join=st_within)

j2 <- j %>%
  filter(!(is.na(CSA_ID) & is.na(MSA_ID))) %>%
  group_by(CSA_ID) %>%
  mutate(CSA_POP = sum(POPULATION),
         CSA_BLACK = sum(BLACK)) %>%
  group_by(MSA_ID) %>%
  mutate(MSA_POP = sum(POPULATION),
         MSA_BLACK = sum(BLACK)) %>%
  st_drop_geometry %>%
  arrange(CSA_POP, MSA_POP)
j2$MSA_POP[is.na(j2$MSA_ID)] <- NA
j2$CSA_POP[is.na(j2$CSA_ID)] <- NA
j2$MSA_BLACK[is.na(j2$MSA_ID)] <- NA
j2$CSA_BLACK[is.na(j2$CSA_ID)] <- NA

#filter to CSA_POP > 1 mill OR MSA_POP > 1 mill
j2 <- j2 %>%
  filter(CSA_POP > 1000000 |  MSA_POP > 1000000) %>%
  select(-matches('NAME')) %>%
  mutate(id = ifelse(is.na(CSA_ID), MSA_ID, CSA_ID),
         pop = ifelse(is.na(CSA_POP), MSA_POP, CSA_POP),
         black = ifelse(is.na(CSA_BLACK), MSA_BLACK, CSA_BLACK),
         black = black/pop) %>%
  ungroup %>%
  select(fips=FIPS, id, pop, black)

#Make combined shapefile of CSAs > 1 mill and MSAs > 1 mill
shp <- bind_rows(msa %>%
                  filter(MSA_ID %in% j2$id) %>%
                  select(id = MSA_ID),
                csa %>%
                  filter(CSA_ID %in% j2$id) %>%
                  select(id = CSA_ID))

s <- us_states(resolution='high')
shp2 <- st_intersection(st_make_valid(shp), st_union(s))

write.csv(j2, '~/tweets/csa/fips-csa.csv', row.names=F)
write_sf(shp2, '~/tweets/csv/', 'msa-csa', driver='ESRI Shapefile')
