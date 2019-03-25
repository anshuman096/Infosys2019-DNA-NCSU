# File: AB_paired_ttest_music_affect_on_reading.R
# Example: A/B Test: Compare Two Means of Paired Samples
# Data: The same subjects: 
#       reading score with and without background music
# NOTE: Paired Comparisons

# Note: Set working directory to the source file
# Session --> Set Working Directory --> To Source File Location

reading.scores <- scan(file="../data_raw/reading.txt")
score.matrix <- matrix(reading.scores,ncol=2,nrow=11,byrow=TRUE)
# verify the matrix is correct
write.table(score.matrix, sep=" ", row.names=FALSE, 
            col.names=c("Without Music", "With Music"))

set.seed (2018)

res <- function() {
  # apply applies the sample function to each *row* in mat
  shuffle.within.rows <- apply(score.matrix, 1, 
                      function(mat) sample(mat,replace=F))
  # must transpose the shuffled matrix to get 2 column format
  mat2 <- t(shuffle.within.rows)
  diffmeans <- mean(mat2[,2])-mean(mat2[,1])
  return(diffmeans)
}

n <- 1000
trials <- replicate(n, res())
hist(trials)
pval <- sum(ifelse(trials>=1.45,1,0))
cat("Paired comparison p-value: ", pval/n,"\n")
