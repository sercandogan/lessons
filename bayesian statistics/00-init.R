library(tidyverse)

# Beta Distribution
beta <- rbeta(100, .99, .5)
hist(beta)

curve(dbeta(x, 81, 219)) #curve
