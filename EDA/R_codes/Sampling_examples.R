# File: Sampling_examples.R

# install.packages("sampling")
# install.packages("survey")

library (sampling)
library (survey)

cust.sat <- read.csv(file="../data_raw/cust.sat.csv")
head (cust.sat)
summary (cust.sat)

boxplot( score ~ size, data=cust.sat, 
         ylab="Customer Satisfaction Score", 
         xlab="Size of Customer Organization", 
         sub="-- dataset is cust.sat --" )

#---------------------------------
# SRS: Simple Random Sampling
#---------------------------------
set.seed(842)
cust.sat.srs = cust.sat[srswor(n=200,N=2500)==1,]
summary(cust.sat.srs)
dim(cust.sat.srs)
head(cust.sat.srs)

boxplot( score ~ size, data=cust.sat.srs, 
         ylab="Customer Satisfaction Score", 
         xlab="Size of Customer Organization", 
         sub="-- dataset is cust.sat.srs --" )

#---------------------------------
# Stratified SRS
#---------------------------------
set.seed(842)
ind.str = sampling::strata( data=cust.sat, stratanames="size", 
                            size=0.08*NN[unique(cust.sat$size)], 
                            method="srswor",
                            description=TRUE )

cust.sat.str = getdata( cust.sat, ind.str )
summary(cust.sat.str); dim(cust.sat.str); head(cust.sat.str)

boxplot( score ~ size, data=cust.sat.str, 
         ylab="Customer Satisfaction Score", 
         xlab="Size of Customer Organization", 
         sub="-- dataset is cust.sat.str --" )

#---------------------------------
# Cluster 2-stage
#---------------------------------
set.seed(842)
ind.clus = sampling::mstage( data=cust.sat, 
                             stage=list("cluster",""), 
                             varnames="size", 
                             size=list(3,c(100,33,67)), 
                             method=list("srswor","srswor"),
                             description=TRUE )

unique(ind.clus[[1]]$size); table(ind.clus[[1]]$size)
head(ind.clus[[1]]); head(ind.clus[[2]])
cust.sat.clus = getdata( cust.sat, ind.clus )
summary(cust.sat.clus[[2]]) 
dim(cust.sat.clus[[2]])
head(cust.sat.clus[[2]])

table(cust.sat.clus[[2]]$size)
boxplot( score ~ size, data=cust.sat.clus[[2]], 
         ylab="Customer Satisfaction Score", 
         xlab="Size of Customer Organization", 
         sub="-- dataset is cust.sat.clus --" )

#---------------------------------
# Systematic
#---------------------------------
set.seed(842)
kk = floor(2500/200)
ind.sys = sample(kk,1)
ind.sys = ind.sys + (0:199)*kk
cust.sat.sys = cust.sat[ind.sys,]

summary(cust.sat.sys)
dim(cust.sat.sys)
head(cust.sat.sys)
table(cust.sat.sys$size)

boxplot( score ~ size, data=cust.sat.sys, 
         ylab="Customer Satisfaction Score", 
         xlab="Size of Customer Organization", 
         sub="-- dataset is cust.sat.sys --" )
