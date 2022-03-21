


# Confidence interal for the slope coefficient in simple linear regression
n = 100
p = 2
beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
sigma = 1

x = rnorm(n)
y = beta[1] + beta[2]*x + rnorm( n, sd=sigma)

# Construct a level 1-alpha confidence interval
alpha = .05
beta_1_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
se_beta_1_hat = sigma / sqrt(sum((x - mean(x))^2))
critical_z = qnorm(1 - alpha/2)  # qnorm gives the inverse Gaussian CDF
lower = beta_1_hat - critical_z * se_beta_1_hat; lower
upper = beta_1_hat + critical_z * se_beta_1_hat; upper
beta[2]

# Verify that the 1-alpha confidence interval achieves its nominal 1-alpha level
# of coverage by simulating sythetic data
N = 1000  # Number of simulated data sets
se_beta_1_hat = sigma / sqrt(sum((x - mean(x))^2))
critical_z = qnorm(1 - alpha/2)
coverage = 0
for(k in 1:N){
	
	y = beta[1] + beta[2]*x + rnorm( n, sd=sigma)
	beta_1_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
	lower = beta_1_hat - critical_z * se_beta_1_hat
	upper = beta_1_hat + critical_z * se_beta_1_hat
	
	if( lower < beta[2] & beta[2] < upper)  coverage = coverage +1
}
coverage = coverage / N; coverage

# Repeat the above simulation study over the grid of 1-alpha {.01,.02, ..., .99}
nominal_coverage = seq( .01, .99, by=.01)
coverage = rep( 0, length(nominal_coverage))
se_beta_1_hat = sigma / sqrt(sum((x - mean(x))^2))
for(k in 1:N){
	
	y = beta[1] + beta[2]*x + rnorm( n, sd=sigma)
	beta_1_hat = sum((x - mean(x))*y) / sum((x - mean(x))^2)
	
	for(r in 1:length(nominal_coverage)){
		alpha = 1-nominal_coverage[r]
		critical_z = qnorm(1 - alpha/2)
		lower = beta_1_hat - critical_z * se_beta_1_hat
		upper = beta_1_hat + critical_z * se_beta_1_hat
	
		if( lower < beta[2] & beta[2] < upper)  coverage[r] = coverage[r] +1
	}
}
coverage = coverage / N

plot( nominal_coverage, coverage, xlab="nominal coverage", 
      ylab="empirical coverage")
lines( c(0,1), c(0,1), col="green")
