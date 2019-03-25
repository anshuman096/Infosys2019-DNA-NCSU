# File: Bootstrap_sampling.R
# NOTE: Set Working directory to the source file location
#   Session --> Set Working Directory --> To Source File Location

#install.packages("boot")
library (boot)

loans_income <- read.csv(file = "../data_raw/loans_income.csv")[,1]

head (loans_income)

stat_fun <- function (x, idx) median (x [idx])

boot.obj <- boot (loans_income, R = 1000, statistic = stat_fun)
boot.obj

# estimate confidence interval on the obtained statistic
boot.ci (boot.obj, type="perc")
