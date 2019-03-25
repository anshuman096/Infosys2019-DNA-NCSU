# File: Explore_distribution.R
#------------------------------------
# Sample Statistic vs. Population Parameter
#-------------------------------------

#------------------------------------
# Explore  t-distribution
#-------------------------------------

# Generate n random numbers from the t-distribution
# with n degrees of freedom
n = 1000
degf <- n
tRandom <- rt(n, df=degf)

# Confirm that the empirical mean is zero 
# and the variance of n / (n-2)
mean(tRandom)
var(tRandom)

# Compute and Plot the t-distribution
require(ggplot2)
densityTRandom <- dt(tRandom, df=degf)
df <- data.frame(x=tRandom, y=densityTRandom)
ggplot(df) + aes(x=x, y=y) + geom_line() +
  labs(x="Random t-Distribution Variable", y="Density")

# Use a Shapiro-Wilk test to check 
# whether tRandom is from the normal distribution
# when the p-value is lower than 0.05, 
# conclude that the sample deviates from normality
plot(density(tRandom))
shapiro.test(tRandom)

# Plot using the qqnorm()
qqnorm(tRandom)
qqline(tRandom, col=2)

#------------------------------------------
# Comparative analysis of t-distributions
# for different values  of n, sample size
#------------------------------------------



#------------------------------------
# Explore  Chi-squared-distribution
#-------------------------------------

# Generate n random numbers from the 
# Chi-squared-distribution
# with n degrees of freedom
n = 1000
df <- 7
chiSquaredRandom <- rchisq(n, df=df)

# Confirm that the empirical mean is n 
# and the variance is 2n 
mean(chiSquaredRandom)
var(chiSquaredRandom)

# Compute and Plot the chi-squared-distribution
require(ggplot2)
densityChiSquared <- dchisq(chiSquaredRandom, df=df)
df.data <- data.frame(x=chiSquaredRandom, y=densityChiSquared)
ggplot(df.data) + aes(x=x, y=y) + geom_line() +
  labs(x="Random Chi-Squared-Distribution Variable", y="Density")


