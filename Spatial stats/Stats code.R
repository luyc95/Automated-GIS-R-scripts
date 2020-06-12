# package loading
library(moments)

# Set work path
setwd()

# data loading
data <- read.csv2()
head(data)

# Get descriptive stats
dim(data)
length(data)
summary(data)
mean(data)
var(data)
sd(data)
skewness(data)
kurtosis(data)

# Plotting histograms and relative histograms
hist(data)
hist(data, freq = FALSE)
curve(dnorm(data, mean=mean(data), sd=sd(data)), add=TRUE)

# QQ-plot
qqnorm(data)
qqline(data)

# Scatter plot
plot(data)

# T-test
# Creating a binary (0,1) variable and using it to select a subset of the data.
data$A <- ifelse(conditona)
data$A == 1

#Creating subsets
A <- data[data$A == 1]
B <- data[data$A != 1]

#Unequal Variance Assumed, two-sided test
t.test(A,B)

#Unequal Variance Assumed, one-sided test
t.test(A,B, alternative = "greater")

#Equal Variance Assumed, two-sided test 
t.test(A,B, var.equal = TRUE)

#Unequal Variance Assumed, two-sided test
t.test(A,B)

# ANOVA
model<- lm(data)
anova(model)

# Levene Test
abs_dev <- abs(resid(model))
boxplot(abs_dev)
anova(lm(abs_dev))

# K-S test
data_ECDF <- ecdf(data)
plot(data_ECDF)
curve(pnorm(data, mean=mean(data), sd=sd(data), add=TRUE)
ks.test(data, "pnorm", mean(data), sd(data))

# Kruskal-Wallis 
kruskalWallis <- kruskal.test(data)
kruskalWallis

qchisq(0.05, k-1, lower.tail = FALSE)

# Median Test
median <- median(data)

boxplot(data)
abline(h = median, lwd = 2)

data_sub <- ifelse(data > median, "Greater than median", "Less than or equal")
data_sub

medianTable <- table(median)
medianTable

chisq <- chisq.test(medianTable)
chisq$expected
chisq

# Pearson's correlation test <6>
r <- cor(data1,data2,method = "pearson")
r

n <- length(data)

t <- (r * sqrt(n-2)) / sqrt(1 - r^2) 

tcrit <- qt(p = 0.025, df = n-2, lower.tail = TRUE)

cor.test(data1,data2,method = "pearson")

# Spearman's correlation test <7>
rs <- cor(data1,data2,method = "spearman")
rs

ts <- rs * sqrt(n-1) 

tcrits <- qt(p = 0.025, df = n-1, lower.tail = TRUE)

cor.test(data1,data2,method = "spearman")

# linear regression model
model <- lm(dataA~dataB, data)
summary(model)

# Scatter plot & includes regression line
plot(dataA,dataB)
abline(reg = model)

#Diagnostic Plots
plot(model)

#Retrieving important quantities
coefficients(model)
residuals(model)
fitted.values(model)

#Log Model
hist(log(data))

#Estimate the log model parameters
ModLog <- lm(log(dataA) ~ dataB)
summary(ModLog)

#Plot the estimated model on the scatter plot
plot(dataB, log(dataA))
abline(reg = ModLog)

#Assess the diagnostic plots of the log model
plot(ModLog)

#Log-Log Model
#Estimate the log-log model parameters
ModLogLog <- lm(log(dataA) ~ log(dataB), data = mosquitos)
summary(ModLogLog)

#Plot the estimated model on the scatter plot
plot(log(dataA), log(dtaB))
abline(reg = ModLogLog)

#Assess the diagnostic plots of the log-log model
plot(ModLogLog)

# Multivariate regression
base.model <- lm(dependvariable ~ Fa+Fb+Fc, data)

summary(base.model)

#Perform the diagnostic plots
plot(base.model)

#Independence and right hand side variables
ResidualVsXPlots <- function(mod.in){
  var.names <- names(mod.in$coefficients)
  n.x.vars <- length(var.names)
  mod.e <- residuals(mod.in)
  
  for (i in 2:n.x.vars){
    plot (mod.in$model[,var.names[i]], mod.e, xlab = var.names[i], ylab = "residuals")
    lines(lowess(mod.in$model[,var.names[i]],mod.e, f=3/4), col="red")
    locator(1)
  }
}

ResidualVsXPlots(base.model)

# Boxplots for the categorical variables
boxplot(residuals(base.model) ~ Fa)

#Homoskedasticity and right hand side variables
XScaleLocationPlots <- function(mod.in){
  var.names <- names(mod.in$coefficients)
  n.x.vars <- length(var.names)
  std.residuals <- sqrt(abs(rstandard(mod.in)))
  
  for (i in 2:n.x.vars){
    plot (mod.in$model[,var.names[i]], std.residuals, xlab = var.names[i], 
          ylab = "Square root of Absolute Standardized Residuals")
    lines(lowess(mod.in$model[,var.names[i]], std.residuals, f=3/4), col="red")
    locator(1)
  }
}

XScaleLocationPlots(base.model)

#Homoskedasticity boxplots
boxplot(sqrt(abs(rstandard(base.model))) ~ Fa)

#Calculating VIF
#Load the package
library(car)

#Calculate VIF
vif(base.model)


#Log Transform
log.model <- lm(log(dependvariable) ~ Fa+Fb+Fc, data)

summary(log.model)

#Perform the diagnostic plots
plot(log.model)

#Log-Log Transform
loglog.model <- lm(log(dependvariable) ~ log(Fa)+Fb+Fc, data = tw)

summary(loglog.model)

#Perform the diagnostic plots
plot(loglog.model)

# Forward test
step(lm(log(dependvariable) ~ 1, data = tw), scope = log(dependvariable) ~ Fa+log(Fb),data = tw, direction = "forward")