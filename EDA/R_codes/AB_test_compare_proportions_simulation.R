# Example: Obama's handling economy example

resample.statistic <- function(data, size=200, 
                               replace = TRUE, fun.name) {
  draw <- sample(data, size, replace)
  pos.rate <- fun.name(draw)/size
  return(pos.rate)
}

# positive and negative rates
sample.size <- 200
pos.response <- 72
neg.response <- 128
positive.rate <- pos.response / sample.size

hat <- c(rep(1,pos.response), rep(0,neg.response))

with.replacement <- TRUE
n.repeats <- 1000

trials <- replicate(n.repeats, 
                    resample.statistic(hat, sample.size, 
                             with.replacement, sum))
hist(trials)
ci <- quantile(trials, c(0.05, 0.95))
cat("Point Estimate: ", positive.rate, "\n")
cat("90% Confidence Interval: ",ci,"\n") 
