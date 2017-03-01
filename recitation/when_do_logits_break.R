test_logit <- function(n, num_betas, which_coef_to_test)
{
  betas <- seq(from = -5, to = 5, by = 0.25)
  betas <- sample(betas, num_betas)
  coef_simulations <- replicate(1000, {
    xs <- cbind(1, replicate(num_betas - 1, rnorm(n)))
    probs <- plogis(xs %*% betas)
    y <- rbinom(n, 1, prob = probs)
    form <- as.formula(
      paste("y ~", paste("xs[,", seq(from = 2, to = num_betas, by = 1), "]", 
        collapse = " + "))
    )
    coef(glm(form, family = "binomial"))
  })
  ggdat <- as.data.frame(t(coef_simulations))
  ggplot(ggdat, aes(ggdat[, which_coef_to_test])) + 
    geom_histogram(bins = 100) + theme_bw() +
    ggtitle("When do Logits Break?") +
    xlab(paste("Coefficient", which_coef_to_test, sep = " ")) +
    # xlim(betas[which_coef_to_test] - 5, betas[which_coef_to_test] + 5) +
    geom_vline(xintercept = betas[which_coef_to_test], colour = "red")
}
test_logit(n = 500, num_betas = 3, which_coef_to_test = 3)
