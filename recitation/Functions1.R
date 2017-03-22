###################################
#### Writing functions in R
###################################

##############################
### Loops are bad ###
##############################

N <- 10000
x1 <- runif(N)
x2 <- runif(N)
d <- as.data.frame(cbind(x1, x2))
system.time(
  for(loop in c(1:length(d[,1]))) {
    d$mean2[loop] <- mean(c( d[loop,1] , d[loop, 2]))
  }
)
system.time(d$mean3 <- rowMeans(d[,c(1,2)]))
system.time(d$mean1 <- apply(d, 1, mean))

##############################
### A few simple functions ###
##############################


# Calculate mean

calc_mean <- function(x){
	sum_x <- sum(x, na.rm=TRUE)
	n_x <- sum(!is.na(x))
	mean_x <- sum_x/n_x
  mean_x
}

z <- 1:6
calc_mean(z)
mean(z)

# Count number of odd integers in x

# for loop first

k <- 0
numbers <- c(1,2,4,5,6,7,11,17)
for(n in numbers)
{
	# n represents an element in x
	# first, n is x[1], next n is x[2], etc.
	if(n %% 2 == 1) k <- k+1 # %% is modulo operator
	# k is incremented each time n is odd
}
k

oddcount <- function(x)
{
  k <- 0
  for(n in x)
  {
    if(n %% 2 == 1)
		  k <- k+1
  }
    return(k)
}

better_oddcount <- function(x)
{
   sum(sapply(x, function(i)
   {
      i %% 2
   }
  )
 )
}

oddcount(c(1,2,4,5,6,7,11,17))
better_oddcount(c(1,2,4,5,6,7,11,17))



##############################
### Robust standard errors ###
##############################

# Discussion: What are heteroskedasticity-robust standard errors?

library(car)
data(Prestige)
mod <- lm(prestige ~ log(income) + education + type, data = Prestige)

## Load the Sandwich package
library(sandwich)

# find the variance-covariance matrix
V <- vcovHC(mod, type = "HC3")

# use V to get standard errors
se <- sqrt(diag(V))

# use standard errors to calculate t-statistic
t <- mod$coef / se

t # any guesses about p-values?

# use t-statistic to calculate p-value
p <- 2 * pt(abs(t), mod$df, lower.tail = FALSE)

# put the results together in a matrix
coefmat <- cbind(mod$coef, se, t, p)

# name the rows and columns of the matrix
rownames(coefmat) <- names(mod$coef)
colnames(coefmat) <- c("Estimate", "SE", "t-stat", "Pr(>|t|)")

# Use one of R's internal functions to print it.
printCoefmat(coefmat)



calc_robust_st_errors <- function(obj, type = "HC4m", two.sided = TRUE)
{
  require(sandwich)
  V <- vcovHC(obj, type = type)
  se <- sqrt(diag(V))
  t <- obj$coef/se
  p <- (2^two.sided) * pt(abs(t), obj$df.residual, lower.tail = FALSE)
  coefmat <- cbind(obj$coef, se, t, p)
  rownames(coefmat) <- names(obj$coef)
  colnames(coefmat) <- c("Estimate", "SE", "t-stat", "Pr(>|t|)")
  cat("\nModel Summary with ", type, " Robust Standard Errors\n\n")
  printCoefmat(coefmat)
}
calc_robust_st_errors(mod, two.sided = FALSE)
calc_robust_st_errors(mod, two.sided = TRUE)
mod2 <- lm(prestige ~ log(income) + education, data = Prestige)
calc_robust_st_errors(mod2, two.sided = TRUE)
# This is cool, but we can run the model and include the robust se
# all IN ONE THING!

better_lm_function <- function(formula, dat, robust = TRUE)
{
  mod <- lm(as.formula(formula), data = dat)
  calc_robust_st_errors <- function(obj, type = "HC4m", two.sided = TRUE)
  {
    require(sandwich)
    require(texreg)
    V <- vcovHC(obj, type = type)
    se <- sqrt(diag(V))
    t <- obj$coef/se
    p <- (2^two.sided) * pt(abs(t), obj$df.residual, lower.tail = FALSE)
    coefmat <- cbind(obj$coef, se, t, p)
    coefmat
  }
  ses <- calc_robust_st_errors(mod)
  ps <- unname(ses[, 4])
  if(robust == TRUE)
  {
    print(screenreg(mod, override.se = ses[, 2], override.pvalues = ps))
  }
  if(robust == FALSE)
  {
    print(screenreg(mod, dcolumn = TRUE, booktabs = TRUE))
  }
}

better_lm_function("prestige ~ log(income) + education + type", dat = Prestige,
  robust = TRUE)
better_lm_function("prestige ~ log(income) + education + type", dat = Prestige,
  robust = FALSE)
## Write a function that takes a vector of numbers and returns the
## cube root divided by the natural log of each number in the vector

## Challenge: write the function in a way that checks that the denominator
## is valid, and outputs an error message if it is not valid
## Hint: don't forget properties of natural logs and different types
## of boolean operators

example <- function(x){
  x^(1/3) / log(x)
}

example(0:5)

example.challenge <- function(x){
  if(1 %in% x){

    return("can't divide by zero")

  } else if (0 %in% x){

    return("can't divide by negative infinity")

  } else {

    x^(1/3) / log(x)
  }
}

example.challenge(1:5)

example.challenge(-5:0)

example.challenge(2:5)


##################
### Cross tabs ###
##################

CrossTable2 <- function(x,y){
  require("gmodels")
  CrossTable(x=x,y=y, prop.r=F, prop.t=F, prop.chisq=F,chisq=T,
             format="SPSS")
}

library(car)
data(Duncan)
Duncan$ed.cat <- cut(Duncan$education, 2)
class(Duncan$ed.cat)
CrossTable2(Duncan$ed.cat, Duncan$type)
