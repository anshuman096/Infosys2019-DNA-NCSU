# File: Design_Experiments.ANOVA.MANOVA.R
#---------------------------
# Design of Experiments
#   ANOVA, ANCOVA
#   MANOVA, MANCOVA
#---------------------------

#-----------------------------
# One-way Between-Groups ANOVA
#-----------------------------
# install.packages("multcomp")
require (multcomp)
attach (cholesterol)
head (cholesterol)
# Group sample size
# Treatments (trt: Dependent Var):
#   * Same drug but administered differently
#       - 20 mg once per day (1time)
#       - 10 mg twice per day (2times)
#       - 5 mg four times per day (4times)
#   * drugD
#   * drugE
# Response Var: Cholesterol Reduction

table(trt)

# Examine means & sd for each treatment group
aggregate (response, by=list(trt), FUN=mean)
aggregate (response, by=list(trt), FUN=sd)

# Test for group differences
# If significant F-test (p-value < 0.05): 
#    then reject the null hypothesis
#  H0 : all five groups have the same effect
fit <- aov (response ~ trt)
summary(fit)

# Plot group means and confidence intervals
library (gplots)
plotmeans (response ~trt, xlab="Treatment",
           ylab = "Cholesterol Reduction",
           main="Means with 95% CI")

# F-test is significant:
#  Conclusion: five treatments are not equally effective
# But: it does not tell which treatments differ from one another
# Use: TukeyHSD() to test all pairwise differences or
#      glht() for multiple mean comparisons

TukeyHSD(fit)
par(las=2)
par (mar=c(5,8,4,2))
plot (TukeyHSD(fit))

# Use: glht() for multiple mean comparisons
library(multicomp)
par (mar = c(5,4,6,2))
comp <- glht(fit, linfct=mcp(trt="Tukey"))
plot (cld(comp, level=0.05), col="lightgrey")

#-----------------------------------------------
# Assumptions underlying one-way ANOVA tests:
#  Dependent var: is normally distributed
#  Dependent var: has equal variance in each trt group
#  ANOVA can be sensistive to outliers
#-----------------------------------------------
library (car)
lm.fit <- lm(response ~ trt, data=cholesterol)
qqPlot (lm.fit, simulate=TRUE, 
        main="Q-Q Plot", labels=FALSE)

# test equality / homegeneity of variances 
bartlett.test (response ~ trt, data=cholesterol)

# test for outliers: NA is p-value > 1
# Significant test: No indication of outliers
library(car)
outlierTest(fit)

#-----------------------------
# One-way Between-Groups ANCOVA
#  Including Covariates
#-----------------------------

# Treatments: four doses of drugs to pregnant mice
# Response: Post-birth weight
# Covariate: Gestation time
data (litter, package="multcomp")
attach(litter)
head(litter)
table(dose)
aggregate(weight, by=list(dose), mean)
fit <- aov(weight ~ gesttime + dose)
summary(fit)

aggregate(weight, by=list(dose), mean)
# Adjust group means after partialling out
# the effects of the covariate
# 
install.packages("effects")
library (effects)
effect ("dose", fit)

# ANCOVA: Multiple Comparisons
# c(3, -1, -1, -1): comparison of the no-drug group
#  with the average of the other three drug groups
library(multicomp)
contrast <- rbind("no drug vs drug" = c(3, -1,-1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))

#-----------------------------------------------
# Assumptions underlying one-way ANCOVA tests:
#  Response var: is normally distributed
#  Response var: has equal variance in each group
#  ACNOVA can be sensistive to outliers
#  Homegeneity of regression slopes between
#    response and covariate for each group
#-----------------------------------------------

# Test homegeity of regression slopes
# Nonsignificant results support equality
# Otherwise, use nonparametric ANCOVA: sm.ancova 
#   in "sm" package
library (multcomp)
fit2 <- aov (weight ~ gesttime*dose, data=litter)
summary(fit2)

#-----------------------------------
# Visualizing the ANCOVA results
#-----------------------------------
install.packages("HH")
library (HH)
ancova (weight ~ gesttime + dose, data=litter)

#-----------------------------------------
# Two-way Factorial ANOVA
# Subjects are assigned to groups
#  from the cross-classification of factors
#------------------------------------------
attach (ToothGrowth)
table (supp, dose)

aggregate (len, 
           by=list(supp,dose), 
           FUN=mean)
aggregate (len, by=list(supp,dose), FUN=sd)

fit <- aov(len ~ supp*dose)
summary(fit)

#-----------------------------------
# Visualizing the two-way ANOVA results
#-----------------------------------
# way-1
interaction.plot (dose, supp, len, type="b",
          col=c("red","blue"), pch=c(16,18),
          main="Interaction: Dose & Supplement Type")

# way-2
library(gplots)
plotmeans(len~interaction(supp,dose, sep=" "),
          connect=list(c(1,3,5), c(2,4,6)),
          col=c("red","darkgreen"),
          main="Interaction with 95% CI",
          xlab="Treatment & Dose Combination")

# way-3
library (HH)
interaction2wt (len~supp*dose)

#-----------------------------------
# Repeated measures ANOVA
# Same subjects are measure more than once
#-----------------------------------
#Factors (Independent variables):
#  Type: Quebec vs Mississippi (Between-Group)
#  Conc: Ambient CO2 concentration (Within-Group)
# Dependent Variable / Response: 
#  Uptake: CO2  uptake by plants
data (CO2)
head(CO2)
w1b1 <- subset(CO2, Treatment=='chilled')
attach(w1b1)
fit <- aov(uptake ~ conc*Type +
            Error(Plant/(conc), w1b1))
summary(fit)

#-----------------------------------
# Visualizing repeated measures ANOVA 
#-----------------------------------
# way-1
par(las=2)
par(mar=c(10,4,4,2))
with(w1b1, 
     interaction.plot (conc, Type, uptake,
      type="b", col=c("red","blue"), pch=c(16,18),
      main="Interaction: Plant Type & Concentration"))

boxplot(uptake ~ Type*conc, data=w1b1,
        col=c("gold","green"),
        main="Chilled Quebec & Mississippi Plants",
        ylab="CO2 uptake rate (umol/m^2 sec)")


#-----------------------------------
# MANOVA: Multivariate Analysis of Variance
# More than one outcome (dependent) variable
#-----------------------------------
library(MASS)
attach(UScereal)
y <- cbind(calories, fat, sugars)
aggregate (y, by=list(shelf), FUN=mean)

cov(y)
fit <- manova (y ~ shelf)
summary(fit)

summary.aov(fit)

#-----------------------------------
# Assumptions: One-way MANOVA
# Multivariate normality of response
# Homogeneity / equality of variance-covariance matrices
# Multivariate outliers
#-----------------------------------
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis (y, center, cov)
coord <- qqplot (qchisq(ppoints(n), df=p),
        d, main="Q-Q Plot: Multivariate Normality",
        ylab="Mahalanobis D2")
abline(a=0,b=1, col=c("red"))

# Multivariate outliers
install.packages("mvoutlier")
library(mvoutlier)
outliers <- aq.plot(y)
outliers

# Robust MANOVA
install.packages("rrcov")

library(MASS)
attach(UScereal)
y <- cbind(calories, fat, sugars)

library (rrcov)
Wilks.test(y, shelf, method="mcd")


