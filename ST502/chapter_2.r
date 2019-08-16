# This R script file provides simple code for simulating from various common
# probability distributions.  The aim is to help students understand how a
# random variable is described by its mass or density function, and how 
# asymptotics plays a role in uncertainty quantification.


# Bernoulli random variable ---------------------------------------------------
# First simulate a sample of realizations with prob(success)=.7
n = 100
p = .7
x = rbinom( n=n, size=1, prob=p)
# Note that 'rbinom()' corresponds to the binomial distribution, but a binomial
# random variable with 1 trial (size=1) is a Bernoulli random variable
# Next look at the empirical mass function of the sample
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='Bernoulli empirical pmf')

# Observe how the empirical density converges to the true density as the sample
# size gets larger
n = 1000
x = rbinom( n=n, size=1, prob=p)
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='Bernoulli empirical pmf')

n = 1000000
x = rbinom( n=n, size=1, prob=p)
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='Bernoulli empirical pmf')

# Lastly compute and plot the emirical cumulative distribution function
cdf = cumsum(pmf)
plot( x=0:1, y=cdf, ylim=0:1, pch=16, main='Bernoulli empirical cdf')
lines( x=0:1, y=rep( cdf[1], 2))
dev.off()
# -----------------------------------------------------------------------------


# Binomial random variable ----------------------------------------------------
# First simulate a sample of realizations with prob(success)=.7 and 12 trials
n = 100
p = .7
num_trials = 12
x = rbinom( n=n, size=num_trials, prob=p)
# Next look at the empirical mass function of the sample
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='binomial empirical pmf')

# Observe how the empirical density converges to the true density as the sample
# size gets larger
n = 1000
x = rbinom( n=n, size=num_trials, prob=p)
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='binomial empirical pmf')

n = 1000000
x = rbinom( n=n, size=num_trials, prob=p)
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='binomial empirical pmf')

# Lastly compute and plot the emirical cumulative distribution function
cdf = cumsum(pmf)
plot( x=0:num_trials, cdf, ylim=0:1, pch=16, main='binomial empirical cdf')
for(i in 1:num_trials) lines( x=(i-1):i, y=rep( cdf[i], 2))
dev.off()
# -----------------------------------------------------------------------------


# Poisson random variable -----------------------------------------------------
# First simulate a sample of realizations with shape parameter 4
n = 100
lambda = 4
x = rpois( n=n, lambda=lambda)
# Next look at the empirical mass function of the sample
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='poisson empirical pmf')

# Observe how the empirical density converges to the true density as the sample
# size gets larger
n = 1000
x = rpois( n=n, lambda=lambda)
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='poisson empirical pmf')

n = 1000000
x = rpois( n=n, lambda=lambda)
pmf = table(x) / n
barplot( height=pmf, ylim=0:1, main='poisson empirical pmf')

# Lastly compute and plot the emirical cumulative distribution function
cdf = cumsum(pmf)
plot( x=0:max(x), cdf, ylim=0:1, pch=16, main='poisson empirical cdf')
for(i in 1:max(x)) lines( x=(i-1):i, y=rep( cdf[i], 2))
dev.off()
# -----------------------------------------------------------------------------


# (Continuous) uniform random variable ----------------------------------------
# First simulate a sample of realizations with interval (0,1)
n = 100
a = 0
b = 1
x = runif( n=n, min=a, max=b)
# Next look at the empirical mass function of the sample
hist( x=x, freq=F, main='uniform empirical pdf')

# Observe how the empirical density converges to the true density as the sample
# size gets larger
n = 1000
x = runif( n=n, min=a, max=b)
hist( x=x, freq=F, main='uniform empirical pdf')

n = 1000000
x = runif( n=n, min=a, max=b)
hist( x=x, freq=F, main='uniform empirical pdf')

# Lastly compute and plot the emirical cumulative distribution function
pdf = hist( x=x, plot=F)$counts / n
x_grid = hist( x=x, plot=F)$mids
cdf = cumsum(pdf)
plot( x=x_grid, cdf, ylim=0:1, type='l', main='poisson empirical cdf')
dev.off()
# -----------------------------------------------------------------------------