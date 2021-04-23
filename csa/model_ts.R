library(data.table)
library(tidyverse)
library(fixest)
library(broom)
library(hms)
library(cowplot)

options(scipen=100)

data <- fread('~/tweets/all.csv')

data$month <- substr(data$statemonth, 4, 5)
data$income_percap <- log(data$income_percap)
data <- data[weather_term == 0, ]
data$raining <- data$prcp > 0
data$hour <- substr(data$tod, 1, 2)

qs_inc <- quantile(data$income_percap, seq(0, 1, by=0.05))
qs_white <- quantile(data$race_white, seq(0, 1, by=0.05))
qs_black <- quantile(data$race_black, seq(0, 1, by=0.05))

tods <- tab(data$hour[data$wbgt > 5])
tods <- data.frame(tod=names(tods), count=as.numeric(tods))

#############################################
# Now effects of temp*income and temp*race
#############################################

allres <- data.frame()
for (todsel in tods$tod){
  print(which(tods == todsel)/nrow(tods))

  sel <- data[data$hour == todsel & data$wbgt > 5, ]

  mod <- feols(vader ~ wbgt + raining + srad | dow + doy + tod + fips + year + month, data=sel)
  base_res <- tidy(mod) %>%
    filter(term =='wbgt') %>%
    mutate(tod = todsel)

   mod <- feols(vader ~ wbgt*income_percap + raining*income_percap + srad*income_percap | dow + doy + tod + fips + year + month, data=sel)
   inc_res <- tidy(mod) %>%
     filter(term =='wbgt:income_percap') %>%
     mutate(tod = todsel)

   mod <- feols(vader ~ wbgt*race_white + raining*race_white + srad*race_white | dow + doy + tod + fips + year + month, data=sel)
   white_res <- tidy(mod) %>%
     filter(term == 'wbgt:race_white') %>%
     mutate(tod = todsel)

   mod <- feols(vader ~ wbgt*race_black + raining*race_black + srad*race_black | dow + doy + tod + fips + year + month, data=sel)
   black_res <- tidy(mod) %>%
     filter(term == 'wbgt:race_black') %>%
     mutate(tod = todsel)

  allres <- bind_rows(allres, base_res, inc_res, white_res, black_res)
}

write.csv(allres, '~/tweets/csa/model_res_ts.csv', row.names=F)


#########################
# Visualize
############################

allres$tod <- hms::as_hms(paste0(allres$tod, ':00:00'))
tods$tods <-  hms::as_hms(paste0(tods$tod, ':00:00'))

############################
# Baseline effect
############################

m <- allres %>% filter(term == 'wbgt')
m$estimate <- 25*(m$estimate)
m$std.error <- 25*(m$std.error)

m$max <- m$estimate + m$std.error*qnorm(0.975)
m$min <- m$estimate + m$std.error*qnorm(0.025)

gap <- ggplot(m) + 
  geom_line(aes(x=tod, y=estimate), color='#FF0000') + 
  geom_point(aes(x=tod, y=estimate), color='#FF0000') + 
  geom_errorbar(aes(x=tod, ymin=min, ymax=max), color='#FF0000') + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  theme_classic() + 
  labs(y='Gap in Sentiment') + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"))

curve <- ggplot(tods) + 
  geom_bar(aes(x=tods, y=count), stat='identity') + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  scale_y_continuous(labels=function(x){format(x, big.mark=',')}, expand=c(0,0),
                     breaks=c(50000000)) + 
  theme_classic() + 
  labs(y="Tweets", x="Time of Day")

plot_grid(gap, curve, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/ts.png', width=6, height=5)


#############################
# Income results
###########################

m <- allres %>% filter(term == 'wbgt:income_percap')
m$estimate <- 25*(m$estimate/(qs_inc[2] - qs_inc[20]))
m$std.error <- 25*(m$std.error/(qs_inc[2] - qs_inc[20]))

m$max <- m$estimate + m$std.error*qnorm(0.975)
m$min <- m$estimate + m$std.error*qnorm(0.025)

gap <- ggplot(m) + 
  geom_line(aes(x=tod, y=estimate), color='#006400') + 
  geom_point(aes(x=tod, y=estimate), color='#006400') + 
  geom_errorbar(aes(x=tod, ymin=min, ymax=max), color='#006400') + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  theme_classic() + 
  labs(y='Gap in Sentiment') + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"))

curve <- ggplot(tods) + 
  geom_bar(aes(x=tods, y=count), stat='identity') + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  scale_y_continuous(labels=function(x){format(x, big.mark=',')}, expand=c(0,0),
                     breaks=c(50000000)) + 
  theme_classic() + 
  labs(y="Tweets", x="Time of Day")

plot_grid(gap, curve, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/ts_income.png', width=6, height=5)


#############################
# Nonwhite race results
###########################

m <- allres %>% filter(term == 'wbgt:race_white')
m$estimate <- 25*(m$estimate/(qs_white[2] - qs_white[20]))
m$std.error <- 25*(m$std.error/(qs_white[2] - qs_white[20]))

m$max <- m$estimate + m$std.error*qnorm(0.975)
m$min <- m$estimate + m$std.error*qnorm(0.025)

gap <- ggplot(m) + 
  geom_line(aes(x=tod, y=estimate), color='#00008B') + 
  geom_point(aes(x=tod, y=estimate), color='#00008B') + 
  geom_errorbar(aes(x=tod, ymin=min, ymax=max), color='#00008B') + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  theme_classic() + 
  labs(y='Gap in Sentiment') + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"))

curve <- ggplot(tods) + 
  geom_bar(aes(x=tods, y=count), stat='identity') + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  scale_y_continuous(labels=function(x){format(x, big.mark=',')}, expand=c(0,0),
                     breaks=c(50000000)) + 
  theme_classic() + 
  labs(y="Tweets", x="Time of Day")

plot_grid(gap, curve, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/ts_race.png', width=6, height=5)

#############################
# Black race results
###########################

m <- allres %>% filter(term == 'wbgt:race_black')
m$estimate <- 25*(m$estimate/(qs_black[20] - qs_black[2]))
m$std.error <- 25*(m$std.error/(qs_black[20] - qs_black[2]))

m$max <- m$estimate + m$std.error*qnorm(0.975)
m$min <- m$estimate + m$std.error*qnorm(0.025)

gap <- ggplot(m) + 
  geom_line(aes(x=tod, y=estimate), color='#00008B') + 
  geom_point(aes(x=tod, y=estimate), color='#00008B') + 
  geom_errorbar(aes(x=tod, ymin=min, ymax=max), color='#00008B') + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  theme_classic() + 
  labs(y='Gap in Sentiment') + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.025, 0.025, -1, 0.025), "cm"))

curve <- ggplot(tods) + 
  geom_bar(aes(x=tods, y=count), stat='identity') + 
  scale_x_time(expand=c(0, 0), label=function(x){substr(as.character(x), 1, 5)},
               breaks=hms::as_hms(paste0(seq(0, 23, 3), ':00:00'))) + 
  scale_y_continuous(labels=function(x){format(x, big.mark=',')}, expand=c(0,0),
                     breaks=c(50000000)) + 
  theme_classic() + 
  labs(y="Tweets", x="Time of Day")

plot_grid(gap, curve, align='v', axis='rl', ncol=1, 
          rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/ts_race.png', width=6, height=5)

