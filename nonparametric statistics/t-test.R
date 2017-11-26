# t-test / # Parametric Test
set.seed(1000)

x <- rnorm(100, mean = 50, sd = 5)

t.test(x, mu = 50)
