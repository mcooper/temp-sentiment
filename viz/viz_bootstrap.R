library(tidyverse)
library(cowplot)

options(scipen=100)

setwd('~/tweets/bootstrap/run1/')


qs <- c(`0%` = 2.86512542314108, `5%` = 9.24436078681336, `10%` = 9.50762643013236,
  `15%` = 9.66277359153295, `20%` = 9.7809565410907, `25%` = 9.87788738618477,
  `30%` = 9.96075970433711, `35%` = 10.0343792073139, `40%` = 10.1015016073241,
  `45%` = 10.1664238091772, `50%` = 10.2289059417366, `55%` = 10.2891959144632,
  `60%` = 10.3511698426856, `65%` = 10.4156184530856, `70%` = 10.4835944986479,
  `75%` = 10.5594328396115, `80%` = 10.6477488137358, `85%` = 10.7545576000752,
  `90%` = 10.9021985076694, `95%` = 11.155926196887, `100%` = 13.198733009187
  )

#Tempknots
knots = list("wbgt"= c(min(data$wbgt), -10, 0, 5, 10, 15, 20, 25, 
                          max(data$wbgt)),
             "prcp"= c(min(data$prcp), 0.000001, 0.05, 0.5, max(data$prcp)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))


######################################
#wbgt

vcov.bootmod <- function(object){
  return(object$vcov)
}
coef.bootmod <- function(object){
  return(object$coef)
}


fill_knots <- function(x, filln=10){
  vals <- NULL
  for (i in 2:length(x)){
    vals <- c(vals, seq(x[i-1], x[i], length.out=filln))
  }
  vals <- unique(vals)
  vals <- vals[order(vals)]
  vals
}

make_groups <- function(df, label, values){
  final <- data.frame()
  for (v in values){
    df[ , label] <- v
    final <- bind_rows(df, final)
  }
  final
}

preddf <- data.frame(wbgt=fill_knots(knots$wbgt))
preddf <- make_groups(preddf, 'income_percap', qs[c(2, 11, 20)])

preddf$vader <- 1

mm <- model.matrix(as.formula(paste0("vader ~ ",
                   piece.formula("wbgt", knots[['wbgt']], ""), ' + ',
                   piece.formula("wbgt", knots[['wbgt']], "income_percap"))),
                   data=preddf)

mm <- mm[ , colnames(mm) %in% names(coef(mod))]

#mm <- mm[ , grepl('wbgt', colnames(mm))]
#mm <- mm[ , colnames(mm) != 'income_percap']

mmp <- lapply(seq_len(nrow(mm)), function(i) mm[i,])

res <- svycontrast(myobj, mmp)

preddf <- cbind(preddf, as.data.frame(res))
preddf$ymin <- preddf$contrast - 1.96*preddf$SE
preddf$ymax <- preddf$contrast + 1.96*preddf$SE

preddf$income_percap <- factor(preddf$income_percap)
levels(preddf$income_percap) <- preddf$income_percap %>%
                                   levels %>%
                                   as.numeric %>%
                                   exp %>%
                                   round(0) %>%
                                   format(big.mark = ',') %>%
                                   paste0('$', .)

levels(preddf$income_percap) <- paste0(levels(preddf$income_percap), ' (', 
                                       names(qs[c(2, 11, 20)]), ')')

curve <- ggplot(preddf) + 
  geom_line(aes(x=wbgt, y=contrast, color=income_percap)) + 
  #geom_ribbon(aes(x=wbgt, ymin=ymin, ymax=ymax, fill=income_percap), alpha=0.5) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  labs(x='', y='Predicted Mood of Tweet',
       fill = 'Census Block\nIncome Per-Capita\n(Percentile)',
       color = 'Census Block\nIncome Per-Capita\n(Percentile)',
       subtitle = paste0("AIC: ", AIC(mod), '\nSSR: ', mod$ssr)) +
  theme(legend.position=c(0.2, 0.2))

hist <- ggplot(data[sample(1:nrow(data), nrow(data)*0.01), 'wbgt']) +
  geom_histogram(aes(x=wbgt), binwidth=1) + 
  scale_x_continuous(expand=c(0, 0), limits=c(min(knots$wbgt), max(knots$wbgt))) + 
  scale_y_continuous(labels=function(x){format(x*100, big.mark=',')}) + 
  labs(y="Tweet Count", x="Temperature")

plot_grid(curve, hist, align='v', axis='rl', ncol=1, rel_heights=c(0.8, 0.2))
ggsave('~/temp-sentiment/res/wbgt-income-ref-segments-noSE_Noweather.png', width=6, height=7)
