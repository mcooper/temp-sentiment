library(tidyverse)

n <- 10000

data <- data.frame(x=runif(n, -2, 2))
data$i <- runif(n)

data$y1 <- data$x^2*data$i - data$x*data$i + rnorm(n)
data$y2 <- data$x^2*data$i - data$x*data$i + data$x^2 + data$x + rnorm(n)

ggplot(data) + 
  geom_point(aes(x=x, y=y1, color=i))
ggplot(data) + 
  geom_point(aes(x=x, y=y2, color=i))

#Tempknots
#Tempknots
knots = list("x"= c(seq(-2, 2, by=0.25)))

#Define Segmenting Function
piece.formula <- function(var.name, knots, interact_var='') {
  knots <- knots[c(-1, -length(knots))]
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  if (interact_var == ''){
    paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
  } else{
    paste(interact_var, '*', var.name, "+",
        paste("I(", interact_var, "*pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
  }
}

formula1.1 <- paste0("y1 ~ ", 
                     piece.formula("x", knots[['x']], "i"))
formula1.2 <- paste0("y1 ~ ", 
                     piece.formula("x", knots[['x']], "i"), ' + ',
                     piece.formula("x", knots[['x']], ""))
formula2.1 <- paste0("y2 ~ ", 
                     piece.formula("x", knots[['x']], "i"))
formula2.2 <- paste0("y2 ~ ", 
                     piece.formula("x", knots[['x']], "i"), ' + ',
                     piece.formula("x", knots[['x']], ""))

mod1.1 <- lm(as.formula(formula1.1), data)
mod2.1 <- lm(as.formula(formula2.1), data)
mod1.2 <- lm(as.formula(formula1.2), data)
mod2.2 <- lm(as.formula(formula2.2), data)

AIC(mod1.1)
AIC(mod1.2)
AIC(mod2.1)
AIC(mod2.2)

#mod1.1 is better than mod1.2, so when x interacts with i in all terms, 
#including standalone x segments makes for a worse model

#otoh, mod2.2 is better than mod2.1, so when x has a standalone component,
#the standalone x components should be included

#Given that it depends on the data-generating process, its best is to run both, and then check with AIC


