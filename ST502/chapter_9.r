# Code for illustrating Q-Q plots

# Sample data from the standard uniform distribution
n = 10000
x = runif( n=n, min=0, max=1)
# Next, order the sample
order_stats = sort(x)
# Then compute the quantiles of the standard uniform distribution for each
# order statistic.  Recall that the expected value of the kth order statistic is
# k/(n+1)
u_q = (1:n)/(n+1)
# Finally, plot the observed order statistics against their expected values, 
# assuming the standard uniform distribution.  Since the data were actually
# generated from the standard uniform distribution the fit should be good, and
# we should observe roughly a 45 degree line from the origin.
plot( u_q, order_stats)
grid = seq(0,1,by=.001)
lines( grid, grid, col='green', lwd=3)

# Now consider what happens when we sample data from the beta(2,2) distribution
x = rbeta( n=n, shape1=2, shape2=2)
# First lets look at how the empirical density function compares to the standard
# uniform density function
hist( x, breaks=sqrt(n), freq=F, 
      main='Histogram of beta(2,2) data against unif(0,1) density')
lines( grid, dunif( grid, min=0, max=1), col='green', lwd=2)
# Notice that the tails of these two distributions are very different, and this
# is what shows up on the Q-Q plot
order_stats = sort(x)
plot( u_q, order_stats)
grid = seq(0,1,by=.001)
lines( grid, grid, col='green', lwd=3)



# Repeat this analysis for standard normal data via inverse CDF transforms of 
# the expected quantiles of the standard uniform distribution
x = rnorm( n=n, mean=0, sd=1)
order_stats = sort(x)
plot( u_q, pnorm(order_stats, mean=0, sd=1))
grid = seq(min(x),max(x),by=.001)
lines( grid, grid, col='green', lwd=3)

# How similar does the normal distribution look to a distribution with heavier
# tails?  Say, the t_5 distribution?
x = rt( n=n, df=3)
# First plot the histogram of the data set from the t_5 distribution against 
# the standard normal density function
hist( x, breaks=sqrt(n), freq=F,
      main='Histogram of t_5 data against normal(0,1) density')
grid = seq(min(x),max(x),by=.001)
lines( grid, dnorm( grid, mean=0, sd=1), col='green', lwd=2)
# Again, observe the difference in the tails, and notice how this shows up in 
# the Q-Q plots
order_stats = sort(x)
plot( u_q, pnorm( order_stats, mean=0, sd=1))
lines( grid, grid, col='green', lwd=3)
