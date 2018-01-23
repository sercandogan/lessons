library(faraway)

# Generalized Least Squares -------------------------------
#' Errors are correlated or/and have not constant variance

data("longley")
g <- lm(Employed ~ GNP + Population, data = longley)
summary(g, cor=T)

library(nlme) # contains a GLS fitting function

gs <- gls(Employed ~ GNP + Population,
         correlation = corAR1(form = ~Year),
         data = longley)

summary(gs)

# confidence interval
intervals(gs)



# Weighted Least Squares ----------------------------------
#' Errors are uncorrelated, have unequal variance where the form inequality is known.
#' 

data(strongx)

summary(strongx)


g <- lm(crossx ~ energy, data = strongx, weights = sd ^ 2) 

summary(g)

gu <- lm(crossx ~ energy, data = strongx)

summary(gu)

plot(crossx ~ energy, data = strongx)
abline(g)
abline(gu, lty = 2)

#' The unweighted fit appears to fit the data better overall but remember that
#' for lower values of energy the variance in the response is less and so 
#' the weighted fit tries to catch these points better than others.
#' 



