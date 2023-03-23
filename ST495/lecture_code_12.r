# Illustration of a simple permutation test

# First recall how a hypothesis test is constructed for the difference of two 
# means of iid Gaussian data

sigma = 1

n_x = 31
x = rnorm( n=n_x, mean=3, sd=sigma)
n_y = 26
y = rnorm( n=n_y, mean=3.5, sd=sigma)

z = abs( (mean(x) - mean(y)) / (sigma * sqrt((1/n_x) + (1/n_y))) )
p_value = 2 * pnorm(z, lower.tail=F); p_value

# If H_0 : mu_x = mu_y is false, the what is the distribution of the p-value?
num_sims = 10000 # Number of simulations
p_vals = rep( NA, num_sims)
for(i in 1:num_sims){
	
	x = rnorm( n=n_x, mean=3, sd=sigma)
	y = rnorm( n=n_y, mean=3.5, sd=sigma)

	z = abs( (mean(x) - mean(y)) / (sigma * sqrt((1/n_x) + (1/n_y))) )
	p_vals[i] = 2 * pnorm(z, lower.tail=F)
}
hist( p_vals, freq=F, breaks=floor(sqrt(num_sims)), main="H_0 false")



# If H_0 : mu_x = mu_y is true, then the p_value should be uniformly distributed
num_sims = 10000 # Number of simulations
p_vals = rep( NA, num_sims)
for(i in 1:num_sims){
	
	x = rnorm( n=n_x, mean=3, sd=sigma)
	y = rnorm( n=n_y, mean=3, sd=sigma)

	z = abs( (mean(x) - mean(y)) / (sigma * sqrt((1/n_x) + (1/n_y))) )
	p_vals[i] = 2 * pnorm(z, lower.tail=F)
}
hist( p_vals, freq=F, breaks=floor(sqrt(num_sims)), main="H_0 true")

# In this case of H_0 true how many type 1 errors are observed at level alpha?
alpha = seq( 0, 1, by=.01)
prop_reject = rep( NA, length(alpha))
for(k in 1:length(alpha)) prop_reject[k] = mean(p_vals < alpha[k])
plot( alpha, prop_reject, ylab="empirical coverage", xlab="alpha", 
			main="type 1 errors: z test, Gaussian data")
lines( alpha, alpha, col="green")


# How about if the Gaussian assumption is violated?
num_sims = 10000 # Number of simulations
p_vals = rep( NA, num_sims)
for(i in 1:num_sims){
	
	x = rcauchy( n=n_x, location=3, scale=sigma)
	y = rcauchy( n=n_y, location=3, scale=sigma)

	z = abs( (mean(x) - mean(y)) / (sigma * sqrt((1/n_x) + (1/n_y))) )
	p_vals[i] = 2 * pnorm(z, lower.tail=F)
}
hist( p_vals, freq=F, breaks=floor(sqrt(num_sims)), main="H_0 true")

# In this case of H_0 true how many type 1 errors are observed at level alpha?
alpha = seq( 0, 1, by=.01)
prop_reject = rep( NA, length(alpha))
for(k in 1:length(alpha)) prop_reject[k] = mean(p_vals < alpha[k])
plot( alpha, prop_reject, ylab="propotion rejected", xlab="alpha",
      xlim=c(0,1), ylim=c(0,1), 
			main="type 1 errors: z test, model misspecification")
lines( alpha, alpha, col="green")



# Now consider the permutation test
num_sims = 10000 # Number of simulations
p_vals = rep( NA, num_sims)
for(i in 1:num_sims){
	
	x = rnorm( n=n_x, mean=3, sd=sigma)
	y = rnorm( n=n_y, mean=3, sd=sigma)
	t_n = abs(mean(x) - mean(y))

	w = c( x, y)
	num_perms = 100
	t = rep( NA, num_perms)
	for(k in 1:num_perms){
	
		index = sample( 1:length(w), size=n_x, replace=F)
		x_tilde = w[index]
		y_tilde = w[-index]
	
		t[k] = abs(mean(x_tilde) - mean(y_tilde))
	}
	p_vals[i] = mean(t_n <= t)
	
	print(i)
}

# In this case of H_0 true how many type 1 errors are observed at level alpha?
alpha = seq( 0, 1, by=.01)
prop_reject = rep( NA, length(alpha))
for(k in 1:length(alpha)) prop_reject[k] = mean(p_vals < alpha[k])
plot( alpha, prop_reject, ylab="propotion rejected", xlab="alpha",
      xlim=c(0,1), ylim=c(0,1),
			main="type 1 errors: permutation test, Gaussian data")
lines( alpha, alpha, col="green")


# And how about if we generate Cauchy data, again?
num_sims = 10000 # Number of simulations
p_vals = rep( NA, num_sims)
for(i in 1:num_sims){
	
	x = rcauchy( n=n_x, location=3, scale=sigma)
	y = rcauchy( n=n_y, location=3, scale=sigma)
	t_n = abs(mean(x) - mean(y))

	w = c( x, y)
	num_perms = 100
	t = rep( NA, num_perms)
	for(k in 1:num_perms){
	
		index = sample( 1:length(w), size=n_x, replace=F)
		x_tilde = w[index]
		y_tilde = w[-index]
	
		t[k] = abs(mean(x_tilde) - mean(y_tilde))
	}
	p_vals[i] = mean(t_n <= t)
	
	print(i)
}

# In this case of H_0 true how many type 1 errors are observed at level alpha?
alpha = seq( 0, 1, by=.01)
prop_reject = rep( NA, length(alpha))
for(k in 1:length(alpha)) prop_reject[k] = mean(p_vals < alpha[k])
plot( alpha, prop_reject, ylab="propotion rejected", xlab="alpha",
      xlim=c(0,1), ylim=c(0,1),
			main="type 1 errors: permutation test, Cauchy data")
lines( alpha, alpha, col="green")























