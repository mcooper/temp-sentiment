library(data.table)
library(tidyverse)
library(lubridate)

setwd('~/tweets/')

data <- fread('all.csv')
data <- data[weather_term == 0, ]
data$date <- mdy(paste0(data$doy, '-', data$year))

boostrap_diff <- function(x, y, n=100, q=0.95, gc=FALSE){
  diffs <- c()
  for (i in 1:n){
    print(i/n)
    diffs <- c(mean(sample(x, length(x), replace=T)) - mean(sample(y, length(y), replace=T)), 
               diffs)
    if(gc){
      gc()
    }
  }

  min = quantile(diffs, probs=(1 - q)/2)
  max = quantile(diffs, probs=(1 - (1 - q)/2))
  med = quantile(diffs, probs=0.5)

  return(list(min=min, max=max, med=med))
}

######################################
# Day of Week difference
######################################
mon <- data$vader[data$dow == 'Monday']
sat <- data$vader[data$dow == 'Saturday']

dow_diff <- boostrap_diff(mon, sat)

#####################################
# Hurricane Harvey Difference
#####################################
counties <- c(48007, 48025, 48039, 48057, 48071, 48157, 48167,
  48175, 48199, 48201, 48239, 48245, 48255, 48273, 48291,
  48321, 48339, 48391, 48407, 48409, 48469, 48473, 48481)
event_date <- '2017/08/26'

sel <- data %>%
  filter(fips %in% counties, 
				 date >= ymd(event_date) | date < (ymd(event_date) + days(7)) | 
			   date < ymd(event_date) | date >= (ymd(event_date) - days(7))) %>%
  mutate(bef_aft=ifelse(date >= ymd(event_date), 'after', 'before'))

harvey_diff <- boostrap_diff(sel$vader[sel$bef_aft == 'before'], 
                             sel$vader[sel$bef_aft == 'after'])

#######################################
# Hurricane Sandy Difference
#######################################
counties <- c(9001, 34001, 34004, 34009, 34013, 34017, 34023, 34029, 34039, 36005, 36047, 36059, 36061, 36081, 36085, 36204)
event_date <- '2012/10/29'

sel <- data %>%
  filter(fips %in% counties, 
				 date >= ymd(event_date) | date < (ymd(event_date) + days(7)) | 
			   date < ymd(event_date) | date >= (ymd(event_date) - days(7))) %>%
  mutate(bef_aft=ifelse(date >= ymd(event_date), 'after', 'before'))

sandy_diff <- boostrap_diff(sel$vader[sel$bef_aft == 'before'], 
                            sel$vader[sel$bef_aft == 'after'])

################################
# Change due to heat in rich neighborhoods
# Change due to poor neighbor

poor <- read.csv('~/tweets/viz_cache/vader/wbgt-income.csv') %>%
  filter(wbgt == 25, income_percap == '$10,330 (5%)') %>%
  select(min=ymin, max=ymax, med=pred) %>%
  mutate(lab = 'Poor\n(5th %ile)')
rich <- read.csv('~/tweets/viz_cache/vader/wbgt-income.csv') %>%
  filter(wbgt == 25, income_percap == '$69,933 (95%)') %>%
  select(min=ymin, max=ymax, med=pred) %>%
  mutate(lab = 'Rich\n(95th %ile)')
black <- read.csv('~/tweets/viz_cache/vader/wbgt-race_q.csv') %>%
  filter(wbgt == 25, race_majority == 'Black') %>%
  select(min=ymin, max=ymax, med=pred) %>%
  mutate(lab = 'Majority\nBlack')
white <- read.csv('~/tweets/viz_cache/vader/wbgt-race_q.csv') %>%
  filter(wbgt == 25, race_majority == 'White') %>%
  select(min=ymin, max=ymax, med=pred) %>%
  mutate(lab = 'Majority\nWhite')

plt <- bind_rows(poor, rich, black, white,
                 data.frame(dow_diff) %>%
                   mutate(lab = 'Saturday\nto Monday'),
                 data.frame(sandy_diff) %>%
                   mutate(lab = 'Hurricane\nSandy'),
                 #data.frame(harvey_diff) %>%
                 #  mutate(lab = 'Hurricane\nHarvey')
                 )

write.csv(plt, 'compare_plot_dat.csv', row.names=F)
plt <- read.csv('~/tweets/compare_plot_dat.csv')

plt$lab <- factor(plt$lab, levels=c('Hurricane\nSandy', # 'Hurricane\nHarvey', 
                                    'Saturday\nto Monday', 'Majority\nWhite', 'Majority\nBlack',
                                    'Rich\n(95th %ile)', 'Poor\n(5th %ile)'))

annot <- data.frame(color=c(rep('Majority\nBlack', 2), 
                        rep('Hurricane\nSandy', 2)),
                    x=c(4.5, 4.5, 
                        1.5, 1.5),
                    y=c(0.0003, 0.000015, 
                        0.0003, 0.000015),
                    label=c('Heat Wave Impact By', 'Neighborhood Characteristics',
                            'Impact of', 'Comparison Events'))

ggplot() + 
  geom_bar(data=plt, aes(x=lab, y=med, fill=lab), stat='identity') + 
  geom_errorbar(data=plt, aes(x=lab, ymax=max, ymin=min)) + 
  geom_text(data=annot, aes(x=x, y=y, label=label, color=color), size=4) + 
  scale_fill_manual(values=c('Poor\n(5th %ile)'="#e66101", 
                              'Majority\nBlack'="#ca0020", 
                              'Majority\nWhite'="#f4a582", 
                              'Rich\n(95th %ile)'="#fdb863",
                              'Hurricane\nHarvey'="#636363", 
                              'Hurricane\nSandy'="#636363", 
                              'Saturday\nto Monday'="#636363")) + 
  scale_color_manual(values=c('Majority\nBlack'='black',
                              'Hurricane\nHarvey'='grey30')) + 
  scale_y_continuous(expand=expand_scale(mult=c(0.05, 0))) + 
  scale_x_discrete(position='top') + 
  labs(x='', y='Decrease in Sentiment') + 
  guides(fill=FALSE, color=FALSE) + 
  theme_classic() + 
  coord_cartesian(ylim=c(min(plt$min)*1.05, 0), clip='off') + 
  annotate("segment", x = 2.75, xend = 6.25, y = 0.0006, yend = 0.0006,
    colour = "black", size=0.25) +
  annotate("segment", x = 0.75, xend = 2.25, y = 0.0006, yend = 0.0006,
    colour = "grey30", size=0.25) +
  theme(axis.text.x = element_text(colour = c(rep('grey30',3), rep('black', 4))),
        #top, right, bottom, left
        plot.margin = unit(c(5, 0.025, 0.25, 0.025), "cm"))

ggsave('~/temp-sentiment/res/comparison_plot.png', width=5, height=2.5)

