##E32

library(data.table)
library(fixest)
library(dplyr)
library(ggplot2)

setwd('~/tweets/')

data <- fread('all.csv')

#Tempknots
knots = list("temp.hi"= c(-10, 0, 5, 10, 15, 20, 25, 30, 35),
             "precip"= c(0.000001, 0.05, 0.5),
             "srad"= c(0.000001, 250, 500, 1000))

#Define Segmenting Function
piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}


system('~/telegram.sh "starting"')
form <- paste0("vader ~ ", piece.formula("temp.hi", knots[['temp.hi']]), ' + ',
                           piece.formula("precip", knots[['precip']]), ' + ',
                           piece.formula("srad", knots[['srad']]),
                           " | dow + doy + tod + fips + year + statemonth")
mod <- feols(as.formula(form), data)
system('~/telegram.sh "donezo!"')

pred <- bind_rows(data.frame(srad=0, precip=0, temp.hi=seq(-40, 50)),
                  data.frame(temp.hi=0, precip=0, srad=c(seq(0, 0.05, length.out=10),
                                                         seq(0.05, 0.5, length.out=10),
                                                         seq(0.5, 50, length.out=10))),
                  data.frame(temp.hi=0, srad=0, precip=c(seq(0, 250, length.out=10),
                                                          seq(250, 500, length.out=10),
                                                          seq(500, 1000, length.out=10),
                                                          seq(1000, 2000, length.out=10))))

pred$dow = data$dow[1]
pred$doy = data$doy[1]
pred$tod = data$tod[1]
pred$fips = data$fips[1]
pred$year = data$year[1]
pred$statemonth = data$statemonth[1]

res <- predict(mod, pred)
pred$prediction <- res

ggplot(pred %>% filter(srad==0, precip==0)) + 
  geom_line(aes(x=temp.hi, y=prediction))

ggplot(pred %>% filter(temp.hi==0, srad==0)) + 
  geom_line(aes(x=log(precip), y=prediction))

ggplot(pred %>% filter(temp.hi==0, precip==0)) + 
  geom_line(aes(x=srad, y=prediction))
