beer_data <- foreign::read.dta("~/Downloads/beer.dta")
mod <- MASS::polr(as.factor(quality) ~ price + alcohol, data = beer_data)
texreg::screenreg(mod)

summary(MASS::polr(as.factor(quality) ~ price + alcohol + class,
  data = beer_data))















bs.sample <- function(dat)
{
  idx <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
  dat[idx,]
}
bs.routine <- function(dat)
{
  sample.dta <- bs.sample(dat)
  coef(MASS::polr(as.factor(quality) ~ price + alcohol,
    data = sample.dta))
}
bs_est <- replicate(1000, bs.routine(dat = beer_data))
hist(bs_est[1,])
abline(v = coef(mod)[1], col = "red", lwd = 2)
abline(v = quantile(bs_est[1,], probs = 0.025), col = "blue", lwd = 2)
abline(v = quantile(bs_est[1,], probs = 0.975), col = "blue", lwd = 2)
hist(rnorm(1000, coef(mod)[1], 0.36))
abline(v = coef(mod)[1], col = "red", lwd = 2)
abline(v = -0.07, col = "orange", lwd = 2)
abline(v = 1.33, col = "orange", lwd = 2)
bs_mean <- rowMeans(bs_est)
bs_ci <- t(apply(bs_est, 1, quantile, probs = c(0.025, 0.975)))
bs_results <- cbind(bs_mean, bs_ci)
colnames(bs_results) <- c("Est", "Low", "High")
mod1 <- MASS::polr(as.factor(quality) ~ price + alcohol, data = beer_data)
texreg::screenreg(mod1,
  override.ci.low = c(bs_results[, 2]),
  override.ci.up = c(bs_results[, 3]))
