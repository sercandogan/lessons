# Errors in Predictions
 x <- 10 * runif(50)
 y <- x + rnorm(50)
gx <- lm(y ~ x) 
summary(gx)

# What happends when we add some noise to the predictor?
z <- x + rnorm(50)
gz <- lm(y ~ z)
summary(gz)

z2 <- x + 5*rnorm(50)
gz2 <- lm(y ~z2)
summary(gz2)

matplot(cbind(x,z,z2),y, xlab = "x", ylab = "y")
abline(gx, lty=1)
abline(gz, lty=2)
abline(gz2, lty=5)
