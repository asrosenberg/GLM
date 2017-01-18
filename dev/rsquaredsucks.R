library(ggplot2)
set.seed(17)

n <- 200

x <- rnorm(n = n, mean = 1, sd = 1)
y <- 2 + 5 * x + rnorm(n = n, mean = 0, sd = 20)
df <- data.frame(y = y, x = x)

lmod <- lm(y ~ x, data = df)
summary(lmod)


R2 <- data.frame(p_useless = NULL, R2 = NULL, adjusted = NULL)

for(i in 1:(n-1)) {
  j <- ncol(df)
  df[ , j+1] <- rnorm(n = n, mean = 0, sd = 1)
  tmp <- data.frame(p_useless = i,
                    R2 = c(summary(lm(y ~ ., data = df))$r.squared)
  )
  R2 <- rbind(R2, tmp)
}


ggplot(data = R2, aes(x = p_useless, y = R2)) + 
  geom_point() + 
  ylab(expression(R^2)) + 
  xlab("number of useless covariates") +
  theme_bw() +
  ggtitle(paste("Why", expression(R^2), "Sucks"))