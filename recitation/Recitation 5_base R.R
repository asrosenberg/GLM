###############################
# Recitation 5: Graphics in R #
###############################

library(car)

data(Duncan, package = "car")
head(Duncan)
row.names(Duncan)
# How do we get more details about variables in dataset?
class(Duncan) # returns class of the dataset, not the variables
lapply(Duncan, class) # applies class function to columns in dataset
# can also read about dataset in "car" documentation
?plot # documentation for plot - see options

# 4 ways to produce the same basic scatterplot
plot(prestige ~ income, data = Duncan) # default settings
attach(Duncan)
plot(income, prestige) # same thing
detach(Duncan)
plot(Duncan$income, Duncan$prestige, xlab = "income", ylab = "prestige")
plot(x = Duncan$income, y = Duncan$prestige, 
     log = "", # x or y values are not logged
     type = "p", # "p" for points, "l" for line, "b" for both, "o" for both overlaid
                 # "h" for very tight histograms, "s" for steps
     pch = 1, # plot symbol is an open dot (1), 
              # pch = 19:25 can be filled in with color
              # pch = 32:117 are ASCII characters
     col = "black", # color of symbols
     cex = 1, # character expansion - scales symbol size relative to default
              # (of course, default is 1)
     cex.main = 1, # changes size of title relative to default
     cex.axis = 1, # changes size of numbers on axis tick marks
     xlab = "income", 
     ylab = "prestige",
     main = "", # main title is empty
     sub = "", # subtitle is empty here
     asp = NA) # sets aspect ratio
?pch
par(pin = c(4,3), mai = c(1.5, 1.2, 1, .5)) # plots will be 4 in. wide by 3 in. tall,
# with margins of 1.5 in. on bottom, 1.5 in. on left,1 in. on top, and .2 in. on right 
set.seed(123)
plot(x = seq(2001,2010,1), y = sample(seq(1,10,1), 10, replace = TRUE), 
     xlim = c(2000, 2011), ylim = c(0, 11),
     type = "o", 
     pch = 36, # -0x20ACL yields Euro symbol, but some super-fancy symbols don't work with all fonts
     cex = 1.3, 
     col = hsv(h = .4, s = .9, v = 1, alpha = 0.6),
     # h is color, s is saturation (light/dark), v is lightness, alpha is transparency
     lty = 3, # line type (solid line (1) is default)
     lwd = 2, # line width relative to default
     xlab = "Year", 
     ylab = "Exchange Rate",
     main = "U.S. Dollar Exchange Rates*", 
     sub = "*This is hypothetical data",
     family = "mono", # sets font family
     font.sub = 3, # changes subtitle font to italic
     col.axis = rgb(r = 0.8, g = 0.4, b = 0), # sets color of axis text
     # r is red, g is green, b is blue
     cex.sub = 0.4,
     cex.lab = 0.8) # changes size of title relative to default
par(par(no.readonly = TRUE)) # restores default graphical parameters


# A few brief plots
# R will determine right plot type to use based on type of the variable 

# univariate scatterplot
plot(Duncan$income)
# histogram
plot(Duncan$type)
hist(Duncan$type) # why the error?
unique(Duncan$type)
unique(as.numeric(Duncan$type))
hist(as.numeric(Duncan$type)) # still not great - only three categories here

# bivariate scatterplot
plot(income ~ education, data = Duncan)

# two factors mosaic-plot
Duncan$inc.cat <- cut(Duncan$income, 3) 
# creates new variable that divides income into 3 intervals
par(las = 2, mar = c(5,7,2,3)) # las sets style of axis labels (2 is perpendicular)
# mar sets margins based on lines (alt. measurement)
plot(inc.cat ~ type, data = Duncan, xlab = "", ylab = "")
par(par(no.readonly = TRUE))

# boxplot
plot(income ~ type, data= Duncan, xlab="")




#######################
# Build a scatterplot #
#######################

plot(prestige ~ income, data=Duncan, pch=16, col="blue",
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income\n (Duncan Data)')

# include a third variable
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')

# can denote third variable with colors, as well
cols <- c("blue", "black", "red")
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')

# can include this information with a color palette, too
myRamp <- colorRampPalette(c("gray75", "gray25")) 
# returns function that converts integer into character vector of colors
cols2 <- myRamp(length(unique(Duncan$education))) # returns vector of 36 colors
pchs <- c(15,16,17)
plot(prestige ~ income, data=Duncan, 
     pch=pchs[as.numeric(Duncan$type)],
     col = cols2[order(Duncan$education)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income\n (Duncan Data)')

# combine 4 plots into 1 frame
par(mfrow = c(2,2))
plot(prestige ~ income, data=Duncan, pch=16, col="blue",
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC good or excellent', 
     main = 'Prestige versus Income')
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC good or excellent', 
     main = 'Prestige versus Income')
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC good or excellent', 
     main = 'Prestige versus Income')
plot(prestige ~ income, data=Duncan, 
     pch=pchs[as.numeric(Duncan$type)],
     col = cols2[order(Duncan$education)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC good or excellent', 
     main = 'Prestige versus Income')
par(par(no.readonly = TRUE))

# add a legend
cols <- c("blue", "black", "red")
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')
legend("bottomright", c("Blue-collar", "Professional", "White-collar"),
       pch=1:3, col=cols) 
# inset gives fraction of plot region b/w legend box and plot box

# add a regression line
cols <- c("blue", "black", "red")
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')
legend("bottomright", c("Blue-collar", "Professional", "White-collar"),
       pch=1:3, col=cols, inset=.01)
abline(lm(prestige ~ income, data=Duncan))


# add several regression lines
cols <- c("blue", "black", "red")
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')
legend("bottomright", c("Blue-collar", "Professional", "White-collar"),
       pch=1:3, col=cols, inset=.01)
abline(lm(prestige ~ income, data=Duncan, 
          subset=Duncan$type == "bc"), col=cols[1])
abline(lm(prestige ~ income, data=Duncan, 
          subset=Duncan$type == "prof"), col=cols[2])
abline(lm(prestige ~ income, data=Duncan, 
          subset=Duncan$type == "wc"), col=cols[3])


# add regression lines only for observed values of income
# first, estimate models
mod.bc <- lm(prestige ~ income, data=Duncan, 
             subset=Duncan$type == "bc")
mod.prof <- lm(prestige ~ income, data=Duncan, 
               subset=Duncan$type == "prof")
mod.wc <- lm(prestige ~ income, data=Duncan, 
             subset=Duncan$type == "wc")

# next, create appropriate predictions
# first get range of incomes for each subset
# then get predictions for prestige for lowest and highest income values within each subset
bc.dat <- data.frame(
    income = range(Duncan$income[which(Duncan$type == "bc")])) 
preds.bc <- predict(mod.bc, newdata=bc.dat)
prof.dat <- data.frame(
    income = range(Duncan$income[which(Duncan$type == "prof")]))
preds.prof <- predict(mod.prof, newdata=prof.dat)
wc.dat <- data.frame(
    income = range(Duncan$income[which(Duncan$type == "wc")]))
preds.wc <- predict(mod.wc, newdata=wc.dat )
bc.dat
preds.bc

# combine these vectors into a new matrix
mat <- cbind(rbind(bc.dat$income, prof.dat$income, wc.dat$income), 
             rbind(preds.bc, preds.prof, preds.wc))
mat

# put it all together:
cols <- c("blue", "black", "red")
plot(prestige ~ income, data=Duncan, pch=as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')
legend("bottomright", c("Blue-collar", "Professional", "White-collar"),
       pch = 1:3, col = cols, cex = 0.6)
segments(x0 = mat[,1], y0 = mat[,3], x1 = mat[,2], y1 = mat[,4], col=cols) 

# identify some points
cols <- c("blue", "black", "red")
plot(prestige ~ income, data = Duncan, pch = as.numeric(Duncan$type),
     col = cols[as.numeric(Duncan$type)],
     xlab='% males earnings > $3500/year', 
     ylab='% of NORC raters indicating profession as good or excellent', 
     main = 'Prestige versus Income')
legend("bottomright", c("Blue-collar", "Professional", "White-collar"),
       pch = 1:3, col = cols, cex = 0.6)
segments(mat[,1], mat[,3], mat[,2], mat[,4], col=cols)
identify(x = Duncan$income, y = Duncan$prestige,
         labels = rownames(Duncan), n = 2)
# alternatively, could use code below to identify points
inds <- which(rownames(Duncan) %in% c("minister", "conductor"))
text(x = Duncan$income[inds], y = Duncan$prestige[inds], 
     labels = rownames(Duncan)[inds], pos = 2)

# other types of plots
hist(Duncan$prestige, breaks = 10, xlab = "Prestige", main = "")

hist(Duncan$prestige, breaks = 10, xlab = "Prestige", 
     main = "", freq = F, col = "gray80") 
lines(density(Duncan$prestige), col="red", lwd=1.5)

bp1 <- by(data = Duncan$prestige, INDICES = list(Duncan$type), FUN = mean)
# by splits data frame into subsets based on factor or list of factors, 
# then applies function to each subset
par(mar=c(4,10,2,2))
barplot(height = bp1, horiz = TRUE, las = 1) 
# bp1 is vector of numbers, which are heights of bars
# horiz sets to horizontal
# las = 1 sets axis labels to horizontal
par(par(no.readonly = TRUE))
dpar <- par(no.readonly = TRUE)
par(dpar)

#############################
# Walk Through a Fancy Plot #
#############################

library(RColorBrewer)
par(family = "mono")
plot(prestige ~ income, data = Duncan, 
     cex = (education/50) + 1,
     axes = FALSE, ann = FALSE,
     col = paste(brewer.pal(3, "Set2")[as.numeric(type)], "66", sep=""),
     pch = 19,
     ylim = c(0,100)) 
box(which = "plot", lty = "solid", col = 8)
title(main = "Prestige versus Income",
      ylab = "Prestige",
      xlab = "Income",
      sub = "Observation Size Proportional to Education Level",
      cex.sub = 0.6)
axis(side = 2, at = seq(10,90,20), labels = c("10", "30", "50", "70", "90"), col = 8)
axis(1, at = seq(15, 75, 15), labels = c("15", "30", "45", "60", "75"), col = 8)
inds <- which(rownames(Duncan) %in% c("minister", "conductor"))
text(x = Duncan$income[inds], y = Duncan$prestige[inds], 
     labels = rownames(Duncan)[inds], pos = 2, cex = .6)
legend("bottomright", c("Blue-collar", "Professional", "White-collar"),
       pch = 19, col = paste(brewer.pal(3, "Set2"), "66", sep=""), 
       cex = 0.6, box.col = 8)
