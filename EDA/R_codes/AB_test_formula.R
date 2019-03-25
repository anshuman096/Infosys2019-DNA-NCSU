# File: AB_test_compare_means_formula.R
# Example: A/B Test: Compare Two Means
# Data: Treat and Control for Pig Blood Test

# Note: Set working directory to the source file
# Session --> Set Working Directory --> To Source File Location

df <- read.table(file = "../data_raw/pigblood.table.txt", 
                 header=TRUE)
control <- df[,1]
treatment <- df [,2]
n.control <- length(control)
n.treatment <- length(treatment)

observed.difference <- mean (control) -
                       mean (treatment)
  
standard.error <- sqrt (var(treatment)/n.treatment + 
                        var(control)/n.control)

t.statistic <- observed.difference / standard.error
t.statistic
