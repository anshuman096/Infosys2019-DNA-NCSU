# File: Sampling_normal_distribution.R
#------------------------------------
# Sample Statistic vs. Population Parameter
#    from the normal (Gaussian) distribution
#-------------------------------------

# Population Parameter: mu
# Sample Statistic: X_bar
# mu : mean
# sigma^2: variance
# sd = sqrt(variance) : standard deviation (S)
rand.normal <- rnorm (n=100, mean = 50, sd = 5)
mean (rand.normal) # sample mean
sd (rand.normal)   # sample standard deviation

# population mean estimates
m.estimates <- sapply (1:1000, 
          FUN=function(iter) { 
            mean(rnorm(100, mean=50, sd=10)) 
          })
hist(m.estimates)

mean (m.estimates)
# S.E. = standard error
var (m.estimates)

#------------------------------------
# Explore unit normal distribution
#-------------------------------------

# Generate 1000 points with the unit normal distribution
randUnitNormal <- rnorm(1000, mean=0, sd = 1)

# Calculate their distribution
densityRandUnitNormal <- dnorm(randUnitNormal)

# Plot the ditribution
require(ggplot2)
df <- data.frame(x=randUnitNormal, y=densityRandUnitNormal)
ggplot(df) + aes(x=x, y=y) + geom_point() +
         labs(x="Random Unit Normal Variable", y="Density")

# Compute the probability that x is less than 1.64
pnorm(1.64)

# Compute the probability that x lies between -1.96 and 1.96
pnorm(1.96) - pnorm(-1.96)

# Compute cumulative probability distribution
probabilityRandUnitNormal <- pnorm(randUnitNormal)
df2 <- data.frame(x=randUnitNormal, y=probabilityRandUnitNormal)
ggplot(df2) + aes(x=x, y=y) + geom_line() +
     labs(x="Random Unit Normal Variable", y="Probability")

1 - pnorm(1.96)

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


