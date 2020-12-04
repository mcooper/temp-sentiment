## This looks like it will work, but is too large for an E16

library(data.table)
library(fixest)

setwd('~/tweets/')

data <- fread('all.csv')

#Tempknots
knots = list("temp.hi"= c(min(data$temp.hi), -10, 0, 5, 10, 15, 20, 25, 30, 35, 
                          max(data$temp.hi)),
             "precip"= c(min(data$precip), 0.000001, 0.05, 0.5, max(data$precip)),
             "srad"= c(min(data$srad), 0.000001, 250, 500, 1000, max(data$srad)))

#Define Segmenting Function
piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

form <- paste0("vader ~ ", piece.formula("temp.hi", knots[['temp.hi']]),
                           #piece.formula("precip", knots[['precip']]),
                           #piece.formula("srad", knots[['srad']]),
                           " | dow + doy + tod + fips + year + statemonth")

start <- Sys.time()
print(start)
mod <- feols(as.formula(form), data)
end <- Sys.time()
end - start
