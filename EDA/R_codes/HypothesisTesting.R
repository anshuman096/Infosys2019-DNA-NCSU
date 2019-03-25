# File: Hypothesis Testing
#---------------------
# HYPOTHESIS  TESTING
#---------------------

#---------------------
# One-sample: T-Test
#---------------------
data(tips, package = "reshape2")
head (tips)
unique (tips$sex)
unique (tips$day)

# Null Hypothesis H0: 
#   The average tip is equal to $2.50
# Assumption: Variance is unknown -> use t-statistic

t.test(tips$tip, alternative="two.sided", mu=2.5)

# Examine the t-statistic visually
## build a t-distribution
randT <- rt(3000, df=NROW(tips)-1)
tipTTest <- t.test(tips$tip, 
                   alternative="two.sided", 
                   mu=2.5) 
require (ggplot2)
ggplot(data.frame(x=randT)) +
  geom_density(aes(x=x), fill="grey", color="grey") +
  geom_vline(xintercept=tipTTest$statistic, color="red") +
  geom_vline(xintercept=mean(randT) +
               c(-2,2)*sd(randT), linetype=2)

## One-sided t-test
t.test(tips$tip, alternative="greater", mu=2.5)

#--------------------------------
# Two-Sample Tests: Independent Populations
#--------------------------------

## Ansari-Bradley Test: Equality of Variances
##    Non-parametric

# H0: The variances in tips between 
#     female and male groups are equal

data(tips, package = "reshape2")

# Examine the variance by sex
aggregate (tip ~ sex, data = tips, var)

# Test for normality of tip distribution
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])

# Use non-parametric test
ansari.test(tip ~ sex, tips)

# H0: The female and male groups are tipped equally
# Assumptions: Variances between the two groups are equal
t.test (tip ~ sex, data = tips, var.equal=TRUE)

# H0: Both independent observations are sampled 
#     from the same probability distribution
# Non-parametric Test: Wilcoxon rank sum tes

library(MASS)
head(UScrime)
# So: Southern vs non-Southern state
# Prob: Probability of incareceration 
# (i.e., being imprisoned if committed a crime)
with (UScrime, by(Prob, So, median))

wilcox.test (Prob ~ So, data = UScrime)

#--------------------------------
# Paired Two-Sample Tests: Dependent Populations
#--------------------------------
install.packages("UsingR")
require(UsingR)
head(father.son)

t.test(father.son$fheight, father.son$sheight, paired=TRUE)

# H0: Both dependent observations are sampled 
#     from the same probability distribution
# Non-parametric Test: Wilcoxon signed rank test

library(MASS)
head(UScrime)
sapply(UScrime[c("U1", "U2")], median)
with (UScrime, wilcox.test(U1, U2, paired = TRUE))

library(MASS)
head(UScrime)
sapply(UScrime[c("U1", "U2")], 
       function(x) c(mean=mean(x), sd=sd(x)))
with (UScrime, t.test(U1, U2, paired = TRUE))

#-----------------------------------
# Hypothesis Testing via Simulation
#-----------------------------------
# Merchant ad intervention produce 8% return rate 
# compared to historical 10%
# Null Hypothesis: Reduction in return is by chance

# Hat: One one and nine zeros, 
# representing the null model of 10% returns
hat <- c(rep(1,1),rep(0,9))

resample.statistic <- function() {
  draw <- sample(hat, 200, replace = TRUE)
  ret <- sum(draw)
  return(ret)
}

n <- 1000
trials <- replicate(n, res())
pval <- sum(ifelse(trials<=16,1,0))
cat("Estimated p-value: ",pval/n,"\n") 
hist(trials)


