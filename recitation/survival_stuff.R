library(survival)
library(foreign)
library(texreg)
library(survminer)
library(wesanderson)
dat <- read.dta("~/Dropbox/Archive/Teaching/GLM_Spring_2016/Data/warchest.dta")
# te: time until a challenger enters a race against an incumbant 
# wc: warchest of incumbant 
# pvote: incumbant vote share
# democrat
# south
head(dat)
# response variable, then whether it is censored (RC = not everyone dies)
Surv(dat$te, dat$censor)

# simple exponential survival model
# positive coef = increase chance of living 
survreg(Surv(te, censor) ~ wc, data = dat, dist = "exponential")

# more complex
survreg(Surv(te, censor) ~ wc + dem + pvote, data = dat, dist = "exponential")
survreg(Surv(te, censor) ~ wc + dem + pvote + 
  south, data = dat, dist = "exponential")

# test weibull
mod <- survreg(Surv(te, censor) ~ wc + dem + pvote + south, 
  data = dat, dist = "weibull")
summary(mod)
ggsurvplot(mod, data = dat)

# shape matters because it tells you if the hazard is truly constant
# scale = 1/shape
1/0.33 # looks like it's not =/= 1

# stratify by south
coxph(Surv(te, censor) ~ wc + pvote + strata(south) + dem, 
  data = dat, method = "efron")

data(lung)
fit<- survfit(Surv(time, status) ~ strata(sex) + age + 
  wt.loss, data = lung)
# Basic survival curves
ggsurvplot(fit, data = lung)
ggsurvplot(fit, data = lung,
  surv.median.line = "hv", # Add medians survival
  # Change legends: title & labels
  legend.title = "Sex",
  legend.labs = c("Male", "Female"),
  # Add p-value and confidence intervals
  pval = TRUE,
  conf.int = TRUE,
  # Add risk table
  risk.table = TRUE,
  tables.height = 0.2,
  tables.theme = theme_cleantable(),
  # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
  # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
  # I choose the wes anderson film "Grand Budapest Hotel"
  palette = c(wes_palette("GrandBudapest", 2, type = "discrete")[1], 
    wes_palette("GrandBudapest", 2, type = "discrete")[2]),
  ggtheme = theme_bw() # Change ggplot2 theme
) 

