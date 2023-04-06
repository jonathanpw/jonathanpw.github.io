# EM algorithm examples




# Multinomial data example
p = .3
probs = c( .5, .25*p, .25*(1-p), .25*(1-p), .25*p)
n = 1000

# Generate a full vector of 5 components
y = rmultinom(n=1, size=n, prob=probs)

# However, we only observe
x = c( y[1]+y[2], y[3], y[4], y[5])

# EM algorithm to learn theta
p = 1
p_old = 0
iter = 0
while( abs(p - p_old) > 10^-4){
	
	p_old = p
	 
	# E step
	q_old = p_old / (2 + p_old)
	
	# M step
	p = (x[4] + q_old*x[1])	/ (x[2] + x[3] + x[4] + q_old*x[1])
	
	iter = iter +1
	cat(paste0("iteration ",iter," p = ",p,"\n"))
}





# Gaussian mixture model example
mu = c( 1, 2, 3)
sigma = c(.1, .25, .5)
p = c( .7, .2, .1)
m = length(mu)

n = 10000
y = sample( 1:m, size=n, prob=p, replace=T) 

x = rep( NA, n)
for(i in 1:n) x[i] = rnorm( n=1, mean=mu[y[i]], sd=sigma[y[i]])
hist(x, freq=F, breaks=floor(sqrt(n)))



# EM algorithm to learn theta

# First we need a helper function
f_yi_given_xi = function( j, xi, theta){
	
	f = rep( NA, m)
	for(k in 1:m){
		mu_k = theta[mu_index[k]] 
		sigma_k = theta[sigma_index[k]]
		p_k = theta[p_index[k]] 
		f[k] = p_k * exp(-(xi - mu_k)^2 / (2*sigma_k^2)) / (sigma_k * sqrt(2*pi))
	}
	
	return(f[j] / sum(f))
}
f_yi_given_xi = Vectorize( f_yi_given_xi, vectorize.args="xi")


mu_index = 1:m
sigma_index = (m+1):(2*m)
p_index = (2*m+1):(3*m)

# Look at the histogram of x to determine reasonable initial values
theta = c( 0, 2, 4, 1, 1, 1, 1/3, 1/3, 1/3)
theta_old = rep( 0, 3*m)
iter = 0
while( sum((theta - theta_old)^2) > 10^-6){
	
	theta_old = theta
	
	for(j in 1:3){
		
		total = sum(f_yi_given_xi( j, x, theta_old))
		
		theta[mu_index[j]] = sum(x * f_yi_given_xi(j,x,theta_old)) / total
		
		ss = (x - theta_old[mu_index[j]])^2
		theta[sigma_index[j]] = sqrt(sum(ss * f_yi_given_xi(j,x,theta_old)) / total)
		
		theta[p_index[j]] = total / n
	}
	
	iter = iter +1
	cat(paste0("iteration ",iter," theta = ",theta,"\n"))
}


theta[mu_index]
theta[sigma_index]
theta[p_index]



















