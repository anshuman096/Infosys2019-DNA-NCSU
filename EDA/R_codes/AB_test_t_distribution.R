# File: AB_test_t_distribution.R
#------------------------------------
# Explore  t-distribution
#-------------------------------------

control.group.size = 10
treatment.group.size = 10

t.statistic = 1.777
n = control.group.size + 
    treatment.group.size
degrees.of.freedom = 11.7

# Generate n random numbers from a t-distribution
# with the specified degrees of freedom

tRandom <- rt (n, df=degrees.of.freedom)

# P[X > t.statistic]
pt (t.statistic, 
    df=degrees.of.freedom, 
    lower.tail = FALSE)

hist (tRandom)

# Compute and Plot the t-distribution
require(ggplot2)
densityTRandom <- dt(tRandom, df=degrees.of.freedom)
df <- data.frame(x=tRandom, y=densityTRandom)
ggplot(df) + aes(x=x, y=y) + geom_line() +
  labs(x="Random t-statistic Variable", 
       y="Density") +
  geom_vline(xintercept = t.statistic, linetype="solid", 
                color = "blue", size=2) +
  ggtitle("The t-distribution with the Observed t-statistic")


