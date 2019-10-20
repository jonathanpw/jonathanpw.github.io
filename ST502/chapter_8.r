# Code for illustrating the Monte Carlo and bootstrap approaches to simulating
# the sampling distribution of a statistic.
# Generate data from a Gamma(alpha,lambda) distribution
alpha = .3
lambda = 1.7
n = 100
x = rgamma( n=n, shape=alpha, rate=lambda)
hist(x, xlim=c(0,1.25), freq=F, breaks=sqrt(n))

# Compute the method of moments (MoM) estimates of alpha and lambda
alpha_MoM = mean(x)^2 / (mean(x^2) - mean(x)^2)
lambda_MoM = mean(x) / (mean(x^2) - mean(x)^2)
cat('alpha_MoM = ',alpha_MoM,'\n')
cat('lambda_MoM = ',lambda_MoM,'\n')

# Monte Carlo simulation of the sampling distributions of alpha and lambda
m = 10000
sample_alpha_MoM = rep( 0, m)
sample_lambda_MoM = rep( 0, m)
for(k in 1:m){
	
	x = rgamma( n=n, shape=alpha, rate=lambda)

	sample_alpha_MoM[k] = mean(x)^2 / (mean(x^2) - mean(x)^2)
	sample_lambda_MoM[k] = mean(x) / (mean(x^2) - mean(x)^2)	
}
par(mfrow=c(2,2))
hist( sample_alpha_MoM, freq=F, breaks=sqrt(m), xlab=NA, xlim=c(0,1), 
      ylim=c(0,7), main='sampling dist alpha_MoM')
abline( v=alpha, col='green', lwd=3)
hist( sample_lambda_MoM, freq=F, breaks=sqrt(m), xlab=NA, xlim=c(0,5), 
      ylim=c(0,1), main='sampling dist lambda_MoM')
abline( v=lambda, col='green', lwd=3)

# Bootstrap simulation of the sampling distributions of alpha and lambda
# (when the true values of alpha and lambda are unknown)
sample_alpha_MoM_tilde = rep( 0, m)
sample_lambda_MoM_tilde = rep( 0, m)
for(k in 1:m){
	
	x = rgamma( n=n, shape=alpha_MoM, rate=lambda_MoM)

	sample_alpha_MoM_tilde[k] = mean(x)^2 / (mean(x^2) - mean(x)^2)
	sample_lambda_MoM_tilde[k] = mean(x) / (mean(x^2) - mean(x)^2)	
}
hist( sample_alpha_MoM_tilde, freq=F, breaks=sqrt(m), xlab=NA, xlim=c(0,1), 
      ylim=c(0,7), main='bootstrap dist alpha_MoM')
abline( v=alpha, col='green', lwd=3)
abline( v=alpha_MoM, col='purple', lwd=3)
hist( sample_lambda_MoM_tilde, freq=F, breaks=sqrt(m), xlab=NA, xlim=c(0,5), 
      ylim=c(0,1), main='bootstrap dist lambda_MoM')
abline( v=lambda, col='green', lwd=3)
abline( v=lambda_MoM, col='purple', lwd=3)
dev.off()
# -----------------------------------------------------------------------------





# Code to demonstrate that the exact 1 - alpha confidence interval for mu for a
# normal distribution has 1 - alpha coverage.
n = 10 # sample size
mu = 4.7
sigma = .9
N = 100000 # number of samples/CIs

coverage_mu = 0
for(k in 1:N){
	# Simulate data from a normal distribution with mean mu and variance sigma^2
	x = rnorm( n=n, mean=mu, sd=sigma)
	# Construct the lower and upper limits of a .95 level CI for mu
	CI_lower = mean(x) - qt( p=.975, df=n-1) * sqrt((n-1)/n)*sd(x)/sqrt(n)
	CI_upper = mean(x) + qt( p=.975, df=n-1) * sqrt((n-1)/n)*sd(x)/sqrt(n)
	# Check if mu is in the CI
	if( CI_lower < mu & mu < CI_upper ) coverage_mu = coverage_mu +1/N 
}

# Now repeat for the exact 1 - alpha confidence interval for sigma^2
coverage_sigma_sq = 0
for(k in 1:N){
	# Simulate data from a normal distribution with mean mu and variance sigma^2
	x = rnorm( n=n, mean=mu, sd=sigma)
	# Construct the lower and upper limits of a .95 level CI for mu
	CI_lower = n * ((n-1)/n) * sd(x)^2 / qchisq( p=.975, df=n-1)
	CI_upper = n * ((n-1)/n) * sd(x)^2 / qchisq( p=.025, df=n-1)
	# Check if sigma^2 is in the CI
	if( CI_lower < sigma^2 & sigma^2 < CI_upper ){
		coverage_sigma_sq = coverage_sigma_sq +1/N
	}
}

cat('Coverage of .95 CI for alpha= ', coverage_mu, '\n')
cat('\n')
cat('Coverage of .95 CI for sigma^2 = ', coverage_sigma_sq, '\n')
# Note that since we know the exact CIs of the MLEs, the coverage should be .95
# for every sample size.  May just need to observe enough CIs (i.e., large 
# enough N) 
# -----------------------------------------------------------------------------


















