# File: SRS_sampling.R
# Sampling without replacement: srcwor()
# Sampling with replacement: srswr()

library (sampling)

data(belgianmunicipalities)
head(belgianmunicipalities)

population.size <- length (belgianmunicipalities$Tot04)
cat ("Population size: ", length (srs.sample), "\n")

# Note: vector of 0's and 1's of the same size as population
#       1: which observation to select
set.seed(2020)
sample.size <- 20
srs.bitmap <- srswor(n=sample.size, 
                     N=population.size) 
head (srs.bitmap)

# Access the records in the sample
#   using the sample bitmap
name <- belgianmunicipalities$Commune
as.vector ( name[srs.bitmap == 1] )

#---------------------------------------
# sample(): with or without replacement
#---------------------------------------
my.data <- 1:20

# bootstrap sample
sample (my.data, replace = TRUE)
