library(data.table)
# What does linear mean?

n <- 500
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
b0 <- 2
b1 <- -2
b2 <- 1
b3 <- -5
y <- b0 + b1 * x1 + b2 * x2 + b3 * x3 + rnorm(n)
y <- b0 * b1 * x1 * b2 * x2 * b3 * x3 + rnorm(n)
plot(x1, y)
plot(x2, y)
plot(x3, y)
plot(fitted(lm(y ~ x1 + x2 + x3)), resid(lm(y ~ x1 + x2 + x3)))
plot(lm(y ~ x1 + x2 + x3))




