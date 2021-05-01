library(data.table)
library(tidyverse)
library(lubridate)
library(grid)
library(png)

setwd('~/tweets/')

data <- fread('all.csv')
data <- data[weather_term == 0, ]

# Day of Week
dow_df <- data[ , .(vader = mean(vader), afinn=mean(afinn), hedono=mean(hedono)), .(dow)]

#####################
# Look at Hurricanes
#####################

data$date <- mdy(paste0(data$doy, '-', data$year))

harvey_counties <- c(48007, 48025, 48039, 48057, 48071, 48157, 48167,
  48175, 48199, 48201, 48239, 48245, 48255, 48273, 48291,
  48321, 48339, 48391, 48407, 48409, 48469, 48473, 48481)

harvey_date <- ymd('2017/08/26')

sandy_counties <- c(9001, 34001, 34004, 34009, 34013, 34017, 34023, 34029, 34039, 36005, 36047, 36059, 36061, 36081, 36085, 36204)

sandy_date <- ymd('2012/10/29')

harvey <- data %>%
  filter(fips %in% harvey_counties, date == harvey_date | date == harvey_date - days(7))
sandy <- data %>%
  filter(fips %in% sandy_counties, date == sandy_date | date == sandy_date - days(7))

harvey_sum <- harvey %>%
  mutate(bef_aft=ifelse(date >= harvey_date, 'after', 'before')) %>%
  group_by(bef_aft) %>%
  summarize(vader=mean(vader), afinn=mean(afinn), hedono=mean(hedono))

sandy_sum <- sandy %>%
  mutate(bef_aft=ifelse(date >= sandy_date, 'after', 'before')) %>%
  group_by(bef_aft) %>%
  summarize(vader=mean(vader), afinn=mean(afinn), hedono=mean(hedono))

###########################################
# Death of: Robin williams, Anthony Bourdain

# robin_date <- ymd('2014/08/11')
# tony_date <- ymd('2018/06/08')
# 
# robin <- data %>%
#   filter(date == robin_date | date == robin_date - days(7))
# 
# robin_sum <- robin %>%
#   mutate(bef_aft=ifelse(date >= robin_date, 'after', 'before')) %>%
#   group_by(bef_aft) %>%
#   summarize(vader=mean(vader), afinn=mean(afinn), hedono=mean(hedono))
# 
# tony_sum <- tony %>%
#   mutate(bef_aft=ifelse(date >= tony_date, 'after', 'before')) %>%
#   group_by(bef_aft) %>%
#   summarize(vader=mean(vader), afinn=mean(afinn), hedono=mean(hedono))
# 
# xmas_sum <- data %>%
#   mutate(is_xmas = month(date) == 12 & day(date) == 25)
# xmas_sum <- xmas_sum[ , .(vader = mean(vader), afinn=mean(afinn), hedono=mean(hedono)), 
#                      .(is_xmas)]
# 
################################
#


################################
# Change due to heat in rich neighborhoods
# Change due to poor neighbor

# robin_change <- robin_sum$vader[robin_sum$bef_aft == 'before'] - robin_sum$vader[robin_sum$bef_aft == 'after']
sandy_change <- sandy_sum$vader[sandy_sum$bef_aft == 'before'] - sandy_sum$vader[sandy_sum$bef_aft == 'after']
harvey_change <- harvey_sum$vader[harvey_sum$bef_aft == 'before'] - harvey_sum$vader[harvey_sum$bef_aft == 'after']
sandy_change <- sandy_sum$vader[sandy_sum$bef_aft == 'before'] - sandy_sum$vader[sandy_sum$bef_aft == 'after']

weekly_change <- dow_df$vader[dow_df$dow == 'Saturday'] - dow_df$vader[dow_df$dow == 'Monday']

rich_change <- 0.00125
poor_change <- 0.016

white_change <- 0.008
black_change <- 0.019

plt <- data.frame(lab=c('Hurricane\nHarvey',
                        'Hurricane\nSandy',
                    'Saturday\nto Monday', 'Rich\n(95th %ile)',
                    'Poor\n(5th %ile)', 'Majority\nWhite',
                    'Majority\nBlack'),
                  val=-c(harvey_change, sandy_change, weekly_change,
                        rich_change, poor_change, white_change, black_change))

write.csv(plt, 'compare_plot_dat.csv', row.names=F)
plt <- read.csv('~/tweets/compare_plot_dat.csv')

plt$lab <- factor(plt$lab, levels=c('Hurricane\nSandy', 'Hurricane\nHarvey', 'Saturday\nto Monday', 'Majority\nWhite', 'Majority\nBlack', 'Rich\n(95th %ile)', 'Poor\n(5th %ile)'))

annot <- data.frame(color=c(rep('Majority\nBlack', 2), 
                        rep('Hurricane\nHarvey', 1)),
                    x=c(5.5, 5.5, 2),
                    y=c(0.006, 0.0025, 0.0025) + 0.00875,
                    label=c('Heat Wave By', 'Neighborhood Characteristics',
                            'Comparison Events'))
ggplot() + 
  geom_bar(data=plt, aes(x=lab, y=val, fill=lab), stat='identity') + 
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
  coord_cartesian(ylim=c(-sandy_change*1.05, 0), clip='off') + 
  annotate("segment", x = 3.75, xend = 7.25, y = 0.008, yend = 0.008,
    colour = "black", size=0.25) +
  annotate("segment", x = 0.75, xend = 3.25, y = 0.008, yend = 0.008,
    colour = "grey30", size=0.25) +
  theme(axis.text.x = element_text(colour = c(rep('grey30',3), rep('black', 4))),
        #top, right, bottom, left
        plot.margin = unit(c(1, 0.025, 0.25, 0.025), "cm"))
ggsave('~/temp-sentiment/res/comparison_plot.png', width=6, height=3)

