# File: AB_test_compare_proportions.R

# To test whether the proportions of converts 
# are the same in the two groups 

converters <- c(200, 182)
group.sizes <- c(2353, 2240)

prop.test (x=converters, n=group.sizes, 
           alternative = "greater",
           correct = FALSE)

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
