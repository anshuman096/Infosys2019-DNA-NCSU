import numpy as np
import scipy.stats as stats

albany = np.random.binomial(1, 0.36, 300000)
usa = np.random.binomial(1, 0.36, 300000000)

albany_sample = np.random.choice(albany, 1000, replace = True)
usa_sample = np.random.choice(usa, 1000, replace = True)

num_runs = 0
num_significant = 0
for i in range(1000):
    pval = stats.ttest_ind(albany_sample, usa_sample)
    if pval[1] < 0.05:
        num_significant += 1
    num_runs += 1
    albany_sample = np.random.choice(albany, 100, replace = True)
    usa_sample = np.random.choice(usa, 100, replace = True)
prob = 1 - num_significant/num_runs
print("Proportion of times response rate of Albany and USA are the same: ", prob)
