# Random number and variable generation


# First, consider generating random instances from the uniform(0,1) distribution
n = 10000
u = runif(n)
hist( u, breaks=floor(sqrt(n)), freq=F)
lines( c(0,1), c(1,1) , lwd=3)	


# Next, how to generate instances from a uniform(a,b) distribution?
a = 1
b = 4
x = a + u*(b - a)
hist( x, breaks=floor(sqrt(n)), freq=F)
lines( c( a, b), c( 1/(b-a), 1/(b-a)), lwd=3)	


# Now use the probability integral transform to use uniform(0,1) draws to 
# emulate random instances from the normal(0,1) distribution
n = 10000
u = runif(n)
x = qnorm(u)
hist( x, breaks=floor(sqrt(n)), freq=F)
grid = seq( -4, 4, by=.01)
lines( grid, dnorm( grid, mean=0, sd=1), lwd=3)	


# Recall from lecture that x ~ iid Bernoulli(p) data with prior p ~ beta(a,b) 
# results in the posterior distribution beta( a + sum(x), b + n - sum(x))

# Data generating model
p = .7
n = 30
x = (runif(n) < p)

# Prior and posterior specificatoin
a = 2
b = 5
n_post = 100
p_post = rbeta( n_post, shape1=a+sum(x), shape2=b+n-sum(x))


# Plot a histogram of random sample
hist( p_post, breaks=floor(sqrt(n_post)), freq=F, xlim=c(0,1),
      main="posterior distribution: Bernoulli data, beta prior")

grid = seq( 0, 1, by=.01)
lines( grid, dbeta( grid, shape1=a+sum(x), shape2=b+n-sum(x)), lwd=3)	


# What sort of things can random samples be used for?
# Monte Carol integration
n = 10000
alpha = 1
beta = 3
x = rgamma(n=n, shape=alpha, rate =beta)

# Approximate E(X)
x_bar = mean(x); x_bar
# Compare to the true mean of E(X) = alpha / beta
alpha / beta

# Approximate Var(X)
x2_bar = mean(x^2)
var_x = x2_bar - x_bar^2; var_x
# Compare to the true mean of Var(X) = alpha / beta^2
alpha / beta^2

# Approximate the probability that 0 < x < 6
b = .8
a = 0
prob = pgamma(b, shape=alpha, rate=beta) - pgamma(a, shape=alpha, rate=beta)
prob

MC_integral = mean( a < x & x < b ); MC_integral