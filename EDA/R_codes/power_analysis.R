# File: power_analysis.R

#install.packages("pwr")
library (pwr)

# Cell phone usage and driving reaction time
# d = 0.8 or larger: standarded mean difference
pwr.t.test(d=0.8, sig.level=0.05, power=0.9,
           type="two.sample", 
           alternative="two.sided")

# Cell phone usage and driving reaction time
#Constraints:
#  Assume that you want to detect 0.5 standard deviation difference in population means
#  You limit the chances of falsey declaring population means to be different to 1 out of 100
#  You can only afford to include 40 participants in the study
#Question: 
#  What is the probability to detect a difference between the population means that is that large, given the above constraints?

pwr.t.test(n = 20, d=0.5, sig.level=0.01, 
           type="two.sample", 
           alternative="two.sided")

help(pwr.2p.test)
example (pwr.2p.test)

# h = effect size
# n = sample size
# sig.level = alpha (Type 1 error probability)
# power = probability of detecting the effect size h

results <- pwr.2p.test(h = 0.13, n=25, 
            sig.level = 0.05, 
            alternative = "greater")

print (results$power)

res2 <- pwr.2p.test(h = 0.13, power = 0.75,
                       sig.level = 0.05, 
                       alternative = "greater")

print (res2$n)

pwr.2p.test(h = 0.13, 
            power = 0.60,
            n = 25,
            sig.level = NULL,
            alternative = "greater")





