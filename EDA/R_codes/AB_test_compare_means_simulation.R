# File: AB_test_compare_means_simulation.R
# Example: A/B Test: Compare Two Means via Simulation
# Data: Treat and Control for Pig Blood Test

# Note: Set working directory to the source file
# Session --> Set Working Directory --> To Source File Location
# getwd()

simulation.trial <- function(data.vector,  
                             replace=TRUE, 
                             fun.name) {
  shuffle <- sample(data.vector, 
                    length(data.vector), 
                    replace)
  shuffle.matrix <- matrix(shuffle,ncol=2,
                           nrow=length(data.vector),
                           byrow=TRUE)
  statistic <- fun.name(shuffle.matrix[,2]) - 
               fun.name(shuffle.matrix[,1])
  return(statistic)
}

vector.data <- scan(file = "../data_raw/pigblood.txt")
View (vector.data)
# create a matrix with 10 rows and 2 columns
matrix.data<- matrix(vector.data, 
                     ncol=2, nrow=10, 
                     byrow=TRUE)

write.table(matrix.data, sep=" ", row.names=FALSE, 
            col.names=c( "Control", "Treatment" ))

cat("Mean Control: ", mean(matrix.data[,1]), "\n")
cat("Mean Treatment: ", mean(matrix.data[,2]), "\n")
mean.diff <- mean(matrix.data[,2]) - mean(matrix.data[,1])
cat("Difference in the means of Treatment minus Control: ", 
    mean.diff,"\n")

n.repeats <- 1000
with.replacement <- TRUE
trials <- replicate(n.repeats,
                    simulation.trial(vector.data, 
                                     with.replacement,
                                     mean))
hist(trials)
# The mean.diff is negative, so use <=
pval <- ifelse(trials <= mean.diff, 1, 0)
cat("p-value: ",sum(pval)/n.repeats,"\n")
