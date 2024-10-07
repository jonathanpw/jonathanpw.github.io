# Likelihood inference


# Bernoulli (i.e., "coin toss") likelihood function
L = function( p, x)  return( p^sum(x) * (1-p)^sum(1 - x) )
L = Vectorize( L, vectorize.args="p")

# Input data from the coin toss experiment
x_pseudo = c( 1, 1, 0, 1, 0, 0, 0, 1, 0, 1,
              1, 1, 1, 1, 1, 1, 0, 0, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 0, 0, 1,
              1, 0, 1, 1, 1, 1, 0, 1, 0, 0,
              1, 0, 0, 1, 0, 0, 0, 1, 1, 0,
              1, 1, 0, 1, 1, 1, 0, 0, 1, 0,
              0, 1, 1, 0, 1, 1, 1, 1, 0, 1,
					    1, 1, 1, 0, 1, 0, 1, 0, 0, 0,
					    1, 1, 1, 1, 1, 0, 1, 0, 1, 0,
					    0, 1, 1, 1, 0, 1, 0, 1, 0, 0)

x_real = c( 0, 1, 1, 0, 0, 0, 1, 0, 0, 0,
            1, 0, 0, 1, 1, 1, 0, 1, 1, 0,
            1, 1, 1, 0, 0, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 1, 0, 0,
            1, 0, 1, 0, 1, 1, 0, 1, 1, 1,
            0, 0, 0, 0, 1, 0, 1, 1, 1, 1,
            0, 0, 0, 1, 0, 1, 0, 1, 1, 1,
            1, 0, 1, 1, 1, 1, 0, 1, 1, 0,
            0, 1, 1, 1, 0, 1, 1, 1, 0, 0,
            1, 0, 1, 0, 1, 0, 0, 1, 0, 0)

# Which observed data has a higher likelihood consistent with p=.5?
L( .5, x_pseudo)
L( .5, x_real)

# Plot the likelihood for both data sets over a grid of values of p to determine
# value most consistent with the data
p_grid = seq( 0, 1, by=.01)
plot( p_grid, L( p_grid, x_real), xlab="p", ylab="likelihood", type="l",lwd=3)
lines( p_grid, L( p_grid, x_pseudo), col="green", lwd=3)

# Next, generate synthetic coin tosses for different values of p
n = 100
p_true = .7
x = (runif(n) < p_true)

p_grid = seq( 0, 1, by=.01)
plot( p_grid, L( p_grid, x), xlab="p", ylab="likelihood", type="l", lwd=3)
p_hat = mean(x)
abline( v=p_hat, col="green", lwd=3)
abline( v=p_true, col="red", lwd=3)


# How about the likelihood function for Gaussian data?
n = 30
mu = 0
sigma_sq = 2
x = rnorm( n=n, mean=mu, sd=sqrt(sigma_sq))

L = function( mu, sigma_sq, x){
	return( prod(exp(-.5*(x - mu)^2/sigma_sq) / sqrt(2 * pi * sigma_sq)) )
	# Note that this is equivalent to:
	# prod( dnorm( x, mean=mu, sd=sqrt(sigma_sq))
}


# Assume that sigma^2 is fixed, and plot over a grid of mu values
L_mu_vec = Vectorize( L, vectorize.args="mu")

x = rnorm( n=n, mean=mu, sd=sqrt(sigma_sq))
mu_grid = seq( -5, 5, by=.01)
plot( mu_grid, L_mu_vec( mu=mu_grid, sigma=sigma_sq, x), xlab="mu", 
      ylab="likelihood", type="l", lwd=3)
mu_hat = mean(x)
abline( v=mu_hat, col="green", lwd=3)


# Assume that mu is fixed, and plot over a grid of sigma^2 values
L_sigma_sq_vec = Vectorize( L, vectorize.args="sigma_sq")
sigma_sq_grid = seq( 0, 10, by=.01)

x = rnorm( n=n, mean=mu, sd=sqrt(sigma_sq))
plot( sigma_sq_grid, L_sigma_sq_vec( mu=mu_hat, sigma_sq=sigma_sq_grid, x), 
      xlab="sigma_sq", ylab="likelihood", type="l", lwd=3)
sigma_sq_hat = mean((x - mean(x))^2)
abline( v=sigma_sq_hat, col="green", lwd=3)







