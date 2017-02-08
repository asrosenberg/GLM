# Why attach() is evil

(y <- sample(1:2, 10, replace = TRUE))
(X <- data.frame(y = rnorm(10), x = rnorm(10)))

coef(lm(y ~ x, data = X))

attach(X)
coef(lm(y ~ x))
