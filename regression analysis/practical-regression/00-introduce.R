# Codes from book which is called "Practical Regression and  Anova using R"
# Author of the book : Faraway

# LOAD DATA ---------------------------------------
library(tidyverse)
library(faraway) # has to be installed
data(gala)


# Example 1 

head(gala) 


gfit <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,  data = gala)

summary(gfit) # Model is significcant

# Matrix way

x <- cbind(1,gala[-c(1,2)])
y <- gala$Species

x <- as.matrix(x) # matrix

t(x) %*% x # Matrix Multiplication

# inverses
 xtx_inv <- solve(t(x) %*% x) # same as summary(gfit)$cov.unscaled

 # Determine Beta parameters
 
# AX = B --solve()--> X = A / B
 solve(t(x) %*% x, t(x) %*% y)
 
# Matrix multplication way
 xtx_inv %*% t(x) %*% y

# Variance 
 xtx_inv * var(x)
  
 
# ESTIMATING ROOT MEAN SQUARE ERROR -----------------------------------------------------
sqrt(sum(gfit$residuals ^ 2) / (30 - 6)) # 60.975

 # STANDARD ERRORS FOR THE COEFFICIENTS
 sqrt(diag(xtx_inv)) * 60.957 # rmse
 
 # COMPUTE R2
 
(rsquare <- 1 - sum(gfit$res ^ 2) / sum((y - mean(y)) ^ 2))
 
 
 

 
 
  