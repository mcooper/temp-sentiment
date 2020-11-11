N <- 1000 # the "large" sample size
x <- rnorm(N)
y <- rnorm(N)
m <- 100
n <- N/m

x.sq <- x.y <- rep(NA,n)

for (j in 1:n){
  x.sq[j] <- sum(x[((j-1)*m+1):(j*m)]^2)
  x.y[j] <- sum(x[((j-1)*m+1):(j*m)]*y[((j-1)*m+1):(j*m)])
}

(beta.hat <- sum(x.y)/sum(x.sq)) # recombining the samples and computing OLS (without intercept for simplicity)
[1] -0.04175772

coef(lm(y~x-1)) # here, of course, everything fits into memory and we can check equality
          x 
-0.04175772 
