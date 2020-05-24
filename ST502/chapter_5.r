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



# Convergence in distribution of Poisson(lambda_n) to N(0,1) with 
# lambda_n --> infinity -------------------------------------------------------
sample_size = 10000
lambda_seq = c(1,3,10,100,10000,100000)
par(mfrow=c(2,3))
for(i in 1:length(lambda_seq)){
	
	lambda = lambda_seq[i]
	z = ( rpois( n=1000, lambda=lambda) - lambda ) / sqrt(lambda)
	hist( z, xlim=c(-4,4), ylim=c(0,.8), main=paste0('lambda = ', lambda), freq=F)
	x_grid = seq(-4,4,by=.1)
	lines( x_grid, dnorm( x=x_grid, mean=0, sd=1, log=F))
}
# -----------------------------------------------------------------------------



# Example Monte Carlo integration code for the example from lecture -----------
indicator_A = function( x, y) return(as.integer(x^2 + y^2 < 1))

n = 1000000
sample_mean = 0
for(i in 1:n){
	
	x_i = runif( n=1, min=-2, max=2)
	y_i = runif( n=1, min=-2, max=2)
	
	sample_mean = sample_mean + indicator_A( x_i, y_i) / n
}

cat('Estimated P(A) = ', sample_mean, ' versus true P(A) = ', pi/16, '\n')
# -----------------------------------------------------------------------------

# Seth's elegant solution to the Monte Carlo integration example from lecture -
n<-1000000
x<-runif(n,-2,2)
y<-runif(n,-2,2)
mean( x^2 + y^2 <= 1 )
# -----------------------------------------------------------------------------



