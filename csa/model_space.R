library(data.table)
library(tidyverse)
library(fixest)
library(broom)
library(sf)
library(USAboundaries)

data <- fread('~/tweets/all.csv')
csadat <- fread('~/tweets/csa/fips-csa.csv')

qs_inc <- quantile(data$income_percap, seq(0, 1, by=0.05))
qs_white <- quantile(data$race_white, seq(0, 1, by=0.05))
qs_black <- quantile(data$race_black, seq(0, 1, by=0.05))

data <- merge(data, csadat, on='fips', all=F)

data$month <- substr(data$statemonth, 4, 5)
data$income_percap <- log(data$income_percap)
data <- data[weather_term == 0, ]
data$raining <- data$prcp > 0

csas <- tab(data$id)
csas <- as.numeric(names(csas))[order(csas, decreasing=T)]

#############################################
# Now effects of temp*income and temp*race
#############################################

allres <- data.frame()
for (csasel in csas){
  print(which(csas == csasel)/length(csas))

  sel <- data[data$id == csasel & data$wbgt > 5, ]

  mod <- feols(vader ~ wbgt + raining + srad | dow + doy + tod + fips + year + month, data=sel)
  base_res <- tidy(mod) %>%
    filter(term =='wbgt') %>%
    mutate(id = csasel)

  mod <- feols(vader ~ wbgt*income_percap + raining*income_percap + srad*income_percap | dow + doy + tod + fips + year + month, data=sel)
  inc_res <- tidy(mod) %>%
    filter(term =='wbgt:income_percap') %>%
    mutate(id = csasel)

  mod <- feols(vader ~ wbgt*race_white + raining*race_white + srad*race_white | dow + doy + tod + fips + year + month, data=sel)
  white_res <- tidy(mod) %>%
    filter(term == 'wbgt:race_white') %>%
    mutate(id = csasel)

  mod <- feols(vader ~ wbgt*race_black + raining*race_black + srad*race_black | dow + doy + tod + fips + year + month, data=sel)
  black_res <- tidy(mod) %>%
    filter(term == 'wbgt:race_black') %>%
    mutate(id = csasel)

  allres <- bind_rows(allres, base_res, inc_res, white_res, black_res)
}

write.csv(allres, '~/tweets/csa/model_res_geo.csv', row.names=F)

#########################
# Visualize
############################
csa_sf <- read_sf('~/tweets/csa', 'msa-csa')

states <- us_states(resolution='low') %>%
  filter(!state_name %in% c("Virgin Islands", "Alaska", "Hawaii", 
                      "Guam", "Puerto Rico", "Northern Mariana Islands",
                      "American Samoa"))

#############################
# Baseline results
###########################

m <- merge(csa_sf, allres %>% filter(term == 'wbgt'))
m$estimate <- 25*(m$estimate)
ggplot() + 
  geom_sf(data = states) + 
  geom_sf(data = m, aes(fill=estimate)) + 
  geom_sf(data = states, fill=NA) + 
  geom_sf(data = m, fill=NA, aes(color=p.value < 0.1, size=p.value < 0.1)) + 
  scale_fill_gradient2(low='#ca0020', mid='#f7f7f7', high='#0571b0') + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  scale_size_manual(values=c(1, 1.1)) +
  guides(color=FALSE, size=FALSE,
         fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme_void() + 
  theme(legend.position=c(0.2, 0.1),
        legend.direction='horizontal',
        legend.key.width=unit(2, 'cm'),
        legend.key.height=unit(1, 'cm'),
        legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2))) + 
  coord_sf(crs='epsg:2163') + 
  labs(fill='Change in Sentiment')
ggsave('~/temp-sentiment/res/map_wbgt.png', width=12, height=8)

#############################
# Income results
###########################
m <- merge(csa_sf, allres %>% filter(term == 'wbgt:income_percap'))
m$estimate <- 25*(m$estimate/(qs_inc[2] - qs_inc[20]))
ggplot() + 
  geom_sf(data = states) + 
  geom_sf(data = m, aes(fill=est_change)) + 
  geom_sf(data = states, fill=NA) + 
  geom_sf(data = m, fill=NA, aes(color=p.value < 0.05, size=p.value < 0.05)) + 
  scale_fill_gradient2(low='#e66101', mid='#f7f7f7', high='#5e3c99', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  scale_size_manual(values=c(1, 1.1)) +
  guides(color=FALSE, size=FALSE,
         fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme_void() + 
  theme(legend.position=c(0.2, 0.1),
        legend.direction='horizontal',
        legend.key.width=unit(2, 'cm'),
        legend.key.height=unit(1, 'cm'),
        legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2))) + 
  coord_sf(crs='epsg:2163') + 
  labs(fill='Gap in Sentiment')
ggsave('~/temp-sentiment/res/map_wbgt_income.png', width=12, height=8)

#############################
# Nonwhite results
###########################
m <- merge(csa_sf, allres %>% filter(term == 'wbgt:race_white'))
m$estimate <- 25*(m$estimate/(qs_white[2] - qs_white[20]))
ggplot() + 
  geom_sf(data = states) + 
  geom_sf(data = m, aes(fill=est_change)) + 
  geom_sf(data = states, fill=NA) + 
  geom_sf(data = m, fill=NA, aes(color=p.value < 0.05, size=p.value < 0.05)) + 
  scale_fill_gradient2(low='#d01c8b', mid='#f7f7f7', high='#4dac26', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  scale_size_manual(values=c(1, 1.1)) +
  guides(color=FALSE, size=FALSE,
         fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme_void() + 
  theme(legend.position=c(0.2, 0.1),
        legend.direction='horizontal',
        legend.key.width=unit(2, 'cm'),
        legend.key.height=unit(1, 'cm'),
        legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2))) + 
  coord_sf(crs='epsg:2163') + 
  labs(fill='Gap in Sentiment')
ggsave('~/temp-sentiment/res/map_wbgt_race.png', width=12, height=8)

#############################
# Black results
###########################
m <- merge(csa_sf, allres %>% filter(term == 'wbgt:race_black'))
m$estimate <- 25*(m$estimate/(qs_white[20] - qs_white[2]))
ggplot() + 
  geom_sf(data = states) + 
  geom_sf(data = m, aes(fill=estimate)) + 
  geom_sf(data = states, fill=NA) + 
  geom_sf(data = m, fill=NA, aes(color=p.value < 0.05, size=p.value < 0.05)) + 
  scale_fill_gradient2(low='#d01c8b', mid='#f7f7f7', high='#4dac26', midpoint=0) + 
  scale_color_manual(values=c('TRUE'='#000000', 'FALSE'=NA)) +
  scale_size_manual(values=c(1, 1.1)) +
  guides(color=FALSE, size=FALSE,
         fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme_void() + 
  theme(legend.position=c(0.2, 0.1),
        legend.direction='horizontal',
        legend.key.width=unit(2, 'cm'),
        legend.key.height=unit(1, 'cm'),
        legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2))) + 
  coord_sf(crs='epsg:2163') + 
  labs(fill='Gap in Sentiment')
ggsave('~/temp-sentiment/res/map_wbgt_black.png', width=12, height=8)

