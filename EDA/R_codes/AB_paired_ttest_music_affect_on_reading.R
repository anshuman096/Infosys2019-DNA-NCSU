# File: AB_paired_ttest_music_affect_on_reading.R
# Example: A/B Test: Compare Two Means of Paired Samples
# Data: The same subjects: 
#       reading score with and without background music
# NOTE: Paired Comparisons

# Note: Set working directory to the source file
# Session --> Set Working Directory --> To Source File Location

df <- read.table(file = "../data_raw/reading.table.txt", 
                 header=TRUE)
control = df[,1]
treatment = df [,2]

cat("Mean Control: ", mean(control), "\n")
cat("Mean Treatment: ", mean(treatment), "\n")
cat("Difference in means: ", 
    mean(treatment) - mean(control),"\n")
cat("Difference in means: ", 
    mean(treatment - control),"\n") # same sample size

dim (df)
#head (df)
df

# alternative = "greater": 
#    x has a larger mean than y
# paired = TRUE
t.test (x=treatment,
        y=control, 
        alternative="greater",
        paired = TRUE)
