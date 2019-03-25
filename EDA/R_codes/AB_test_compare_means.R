# File: AB_test_compare_means.R
# Example: A/B Test: Compare Two Means
# Data: Treat and Control for Pig Blood Test

# Note: Set working directory to the source file
# Session --> Set Working Directory --> To Source File Location

df <- read.table(file = "../data_raw/pigblood.table.txt", 
                 header=TRUE)
control = df[,1]
treatment = df [,2]

cat("Mean Control: ", mean(control), "\n")
cat("Mean Treatment: ", mean(treatment), "\n")
cat("Difference in means: ", 
    mean(treatment) - mean(control),"\n")

dim (df)
head (df)
df

# alternative = "greater": 
#    x has a larger mean than y
t.test (x=control, 
        y=treatment, 
        alternative="greater")

# Check Arguments, Usage and Values
help(t.test)

# access individual Values
res <- t.test (x=control, 
               y=treatment, 
               alternative="greater")
res$p.value
res$statistic
res$parameter

# double the data
# means will not change
# BUT p-value does
# what else changes?
control = c(df[,1], df[,1])
treatment = c( df [,2], df [,2])

# alternative = "greater": x has a larger mean than y
t.test (x=control, 
        y=treatment, 
        alternative="greater")







