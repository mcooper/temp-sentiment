library(tidyverse)

n <- 5000
x <- runif(n, -pi, pi)
y <- 3 + sin(x) + rnorm(n)

plot(x, y)

df <- data.frame(x, y)

piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

mod0 <- lm(as.formula(paste0("y ~ ", piece.formula('x', seq(-3.14, 3.14, 0.05)))), data=df)
mod1 <- lm(as.formula(paste0("y ~ ", piece.formula('x', c(-3, -2, -1, 0, 1, 2, 3)))), data=df)
mod2 <- lm(as.formula(paste0("y ~ ", piece.formula('x', c(-2, 0, 2)))), data=df)
mod3 <- lm(y ~ x, data=df)
mod4 <- lm(y ~ x + I(x^2), data=df)

pdat <- data.frame(x=seq(-pi, pi, by=0.01))

pdat$mod0.fit <- predict(mod0, pdat, se=T)$fit
pdat$mod0.sefit <- predict(mod0, pdat, se=T)$se.fit
pdat$mod1.fit <- predict(mod1, pdat, se=T)$fit
pdat$mod1.sefit <- predict(mod1, pdat, se=T)$se.fit
pdat$mod2.fit <- predict(mod2, pdat, se=T)$fit
pdat$mod2.sefit <- predict(mod2, pdat, se=T)$se.fit
pdat$mod3.fit <- predict(mod3, pdat, se=T)$fit
pdat$mod3.sefit <- predict(mod3, pdat, se=T)$se.fit
pdat$mod4.fit <- predict(mod4, pdat, se=T)$fit
pdat$mod4.sefit <- predict(mod4, pdat, se=T)$se.fit

p <- pdat %>%
  gather(var, value, -x) %>%
  mutate(model = substr(var, 1, 4),
         param = substr(var, 6, nchar(var))) %>%
  select(-var) %>%
  spread(param, value) %>%
  mutate(ymin = fit - 1.96*sefit,
         ymax = fit + 1.96*sefit)

ggplot(p) + 
  geom_line(aes(x=x, y=fit, color=model)) + 
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=model), alpha=0.8)
