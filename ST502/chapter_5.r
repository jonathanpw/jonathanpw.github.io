# This R script file provides simple code for illustrating concepts related to
# the law of large numbers and the central limit theorem.


# Law of large numbers for a sum of independent Bernoulli random variables ----
# Begin with a sample of size 10 with a specified probability of success, p
p = .673

# Construct a matrix with columns corresponding to the realization of each data
# set. We want to observe the sampling distribution of the empirical mean
num_data_sets = 1000
n = 10
x = matrix( 0, n, num_data_sets)
# Sample Bernoulli(p) data
for(i in 1:num_data_sets)  x[, i] = rbinom( n=n, size=1, prob=p)
# Next, construct the sample of empirical means
x_bar = colMeans(x)
# and then plot the empirical density function for the empirical mean as a 
# random variable.  Note that the expected value of this random variable is p
par(mfrow=c(2,3))
hist( x_bar, xlim=c(0,1), main=paste0('n = ',n), freq=F)
abline( v=p, col='green', lwd=3)

# Now observe the behavior of this distribution for larger values of n
n = 30
x = matrix( 0, n, num_data_sets)
for(i in 1:num_data_sets)  x[, i] = rbinom( n=n, size=1, prob=p)
x_bar = colMeans(x)
hist( x_bar, xlim=c(0,1), main=paste0('n = ',n), freq=F)
abline( v=p, col='green', lwd=3)
# And larger...
n = 100
x = matrix( 0, n, num_data_sets)
for(i in 1:num_data_sets)  x[, i] = rbinom( n=n, size=1, prob=p)
x_bar = colMeans(x)
hist( x_bar, xlim=c(0,1), main=paste0('n = ',n), freq=F)
abline( v=p, col='green', lwd=3)
# And larger...
n = 1000
x = matrix( 0, n, num_data_sets)
for(i in 1:num_data_sets)  x[, i] = rbinom( n=n, size=1, prob=p)
x_bar = colMeans(x)
hist( x_bar, xlim=c(0,1), main=paste0('n = ',n), freq=F)
abline( v=p, col='green', lwd=3)
# And larger...
n = 10000
x = matrix( 0, n, num_data_sets)
for(i in 1:num_data_sets)  x[, i] = rbinom( n=n, size=1, prob=p)
x_bar = colMeans(x)
hist( x_bar, xlim=c(0,1), main=paste0('n = ',n), freq=F)
abline( v=p, col='green', lwd=3)
# And larger still...
n = 100000
x = matrix( 0, n, num_data_sets)
for(i in 1:num_data_sets)  x[, i] = rbinom( n=n, size=1, prob=p)
x_bar = colMeans(x)
hist( x_bar, xlim=c(0,1), main=paste0('n = ',n), freq=F)
abline( v=p, col='green', lwd=3)
# Recall that by the law of large numbers, this distribution should converge to
# a point mass at p.
# What is the empirical mean of the empirical mean for this last sample?  And
# how close is it to the value of p?
cat('empirical mean of the empirical mean = ',mean(x_bar),'\n')
# Does this suggest that the sample mean is a good estimator for p¸

dev.off()
# -----------------------------------------------------------------------------