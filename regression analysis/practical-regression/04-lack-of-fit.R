library(faraway)
# Testing for lack of fit ------------------------------------------
data(strongx)

# Variance is known
#' variance known be known from past experience

g2 <- lm(crossx ~ energy + I(energy^2), weights = sd^-2, data = strongx)

summary(g2)

plot(strongx$energy,strongx$crossx)
abline(g$coef)
0.6788 ^ 2 * 7 #> #[1] 3.225386

1 - pchisq(3.225386,7) #> [1] 0.86 > 0.05 
#' we cannot detect a lack of fit.
#' Plot the fit:

x <- seq(0.05,0.35, by = 0.01)
lines(x,g2$coef[1]+g2$coef[2]*g2$coef[3]*x^2,lty=2)
#' Thisseems clearly more appropriate than the linear model.


# Variance is unknown

