library(tidyverse)

setwd('~/tweets/summaries/')

fs <- list.files()
fs <- fs[fs != 'all_summarized.csv']

all <- list()
for (f in fs){
  df <- read.fwf(f, header=F, widths=c(11, 11))
  names(df) <- c(f, 'date')
  df$date <- as.character(df$date)
  all <- append(all, list(df))
}

res <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=T)}, all)

res <- res %>%
  arrange(date)

write.csv(res, 'all_summarized.csv', row.names=F)
