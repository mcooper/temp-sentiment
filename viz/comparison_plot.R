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

data$date <- mdy(paste0(data$doy, '-', data$year))

data$month <- month(data$date)

mon_df <- data[ , .(vader = mean(vader), afinn=mean(afinn), hedono=mean(hedono)), .(month)]

make_dif <- function(counties, event_date){
  sel <- data %>%
    filter(fips %in% counties, date == ymd(event_date) | date == ymd(event_date) - days(7)) %>%
    mutate(bef_aft=ifelse(date >= ymd(event_date), 'after', 'before')) %>%
    group_by(bef_aft) %>%
    summarize(vader=mean(vader))
  
  change <- sel$vader[sel$bef_aft == 'before'] - sel$vader[sel$bef_aft == 'after']
  
  change
}


#####################
# 
#####################

#Harvey
harvey_counties <- c(48007, 48025, 48039, 48057, 48071, 48157, 48167,
  48175, 48199, 48201, 48239, 48245, 48255, 48273, 48291,
  48321, 48339, 48391, 48407, 48409, 48469, 48473, 48481)
harvey_date <- '2017/08/26'

harvey_change <- make_dif(harvey_counties, harvey_date)

#Sandy
sandy_counties <- c(9001, 34001, 34004, 34009, 34013, 34017, 34023, 34029, 34039, 36005, 36047, 36059, 36061, 36081, 36085, 36204)
sandy_date <- '2012/10/29'

sandy_change <- make_dif(sandy_counties, sandy_date)

#Oklahoma Tornadoes
okl_counties <- c(40017, 40051, 40027, 40087, 40109, 40083, 40081, 40125)
okl_date <- '2013-05-20'

okl_change <- make_dif(okl_counties, okl_date)

#Orlando Nightclub Shooting
orl_counties <- c(12117, 12095, 12097)
orl_date <- "2016-06-12"

orl_change <- make_dif(orl_counties, orl_date)

#Las Vegas Shooting
lvn_counties <- c(32003)
lvn_date <- "2017-10-02"

lvn_change <- make_dif(lvn_counties, lvn_date)

# Bay Area earthquake
bay_counties <- c(6041, 06081, 06075, 06055, 06095, 06013, 06097, 06001)
bay_date <- "2014-08-24"

bay_change <- make_dif(bay_counties, bay_date)

#Change Over Time
weekly_change <- dow_df$vader[dow_df$dow == 'Saturday'] - dow_df$vader[dow_df$dow == 'Monday']


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

okl_change
orl_change
lvn_change
bay_change

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
                        rep('Hurricane\nHarvey', 2)),
                    x=c(5.5, 5.5, 2, 2),
                    y=c(0.003, 0.00015, 0.003, 0.00015) + 0.008,
                    label=c('Heat Wave Impact By', 'Neighborhood Characteristics',
                            'Impact of', 'Comparison Events'))
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
  coord_cartesian(ylim=c(min(plt$val)*1.05, 0), clip='off') + 
  annotate("segment", x = 3.75, xend = 7.25, y = 0.006, yend = 0.006,
    colour = "black", size=0.25) +
  annotate("segment", x = 0.75, xend = 3.25, y = 0.006, yend = 0.006,
    colour = "grey30", size=0.25) +
  theme(axis.text.x = element_text(colour = c(rep('grey30',3), rep('black', 4))),
        #top, right, bottom, left
        plot.margin = unit(c(1, 0.025, 0.25, 0.025), "cm"))
ggsave('~/temp-sentiment/res/comparison_plot.png', width=5, height=2.5)

