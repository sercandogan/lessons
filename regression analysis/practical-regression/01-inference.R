library(tidyverse)
library(faraway)

# DATA
data("savings") 

g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

summary(g)

# Determine SST
SST <- sum((savings$sr - mean(savings$sr)) ^ 2)
# Determine SSE
SSE <- sum(g$res ^ 2)
# Determine F stats (SST - SSE) / p - 1 / SSE / n - p
f_stat <- ((SST - SSE) / (5 - 1)) / (SSE / (50 - 5))
# Determine p_value
p_value <- 1 - pf(f_stat,4,45)


# ANOVA ----------------------------------
g2 <- lm(sr ~ pop75 + dpi + ddpi, data = savings)

summary(g2)

anova(g2,g)

# Reduced model
gr <- lm(sr ~ I(pop15 + pop75) + dpi + ddpi, data = savings) 

anova(gr,g)

# Another model which says B_ddpi = 1, offset function
gr <- lm(sr ~ pop15 + pop75 + offset(ddpi) + dpi, data = savings) 

anova(gr, g) # We can reject H0.
# Using t_stat = (ß - c) / se(B)
tstat <- (g$coefficients['ddpi'] - 1) / coef(summary(g))["ddpi",2]

2 * pt(tstat,45) # p value

tstat ^ 2 # we find we get the F-value

# Confidence Interval
g <- lm(sr ~ ., data = savings)
summary(g)

# Construct individual 95% confidence intervals for the regression parametes of pop75
qt(0.975,45) 
c(-1.6914 - 2.0114 * 1.08, -1.6914 + 2.0114 * 1.08 ) # Confidence Interval

# ellipse
library(ellipse)
ellipse(g)
plot(ellipse(g,c(2,3)), type = "l", xlim = c(-1,0))
points(0,0)
points(g$coef[2], g$coef[3], pch = 18)
abline(v = c(-0.461 - 2.01 * 0.145,-0.461 + 2.01 * 0.145))
abline(h = c(-1.6914 - 2.0114 * 1.08, -1.6914 + 2.0114 * 1.08))

# Confidence Intervals for predictions

g <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(g)

x0 <- c(1, 0.08, 93, 6.0, 12.0, 0.34) #new observations
y0 <- sum(x0*g$coef) # predicted no. of species

qt(0.975,24) # our t-critical value

x <- cbind(1,gala[,3:7])
x <- as.matrix(x)
xtxi <- solve(t(x) %*% x) # inverse of x^t * x

# mean response CI 
# MSE --> 60.98
bm <- sqrt(x0 %*% xtxi %*% x0) * qt(0.975,24) * 60.98 #the width of bands for mean response 

c(y0 - bm, y0 + bm) # [1]  1.031231 66.808105

# prediction interval

bm <- sqrt(1 + x0 %*% xtxi %*% x0) * qt(0.975,24) * 60.98

c(y0 - bm, y0 + bm) # [1] -96.16306 164.00240


# predict
predict(g, data.frame(Area = 0.08, Elevation = 93, Nearest = 6.0, Scruz = 12, Adjacent = 0.34), 
        se = T)

# Orthogonality
data(odor)
odor

x <- as.matrix(cbind(1,odor[,-1]))

t(x) %*% x # the matrix is diagonal

g <- lm(odor ~ temp + gas + pack, data = odor)
summary(g, cor = T)

g <- lm(odor ~ gas + pack, data = odor) # drop temp variable
summary(g, cor= T)
