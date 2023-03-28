# Introduction to bootstrapping

# Consider bootstrapping to approximatet the sampling distribution of the
# sample mean of an observed sample of data

n = 1000
mu = 30
sigma = 3
x = rnorm(n, mean=mu, sd=sigma)

m = 100
num_boot_samples = 300
bootstrap_means = rep( NA, num_boot_samples)
for(k in 1:num_boot_samples){
	sub_x = sample( x, size=m, replace=T)
	bootstrap_means[k] = mean(sub_x)
}

# Plot the histogram of the subsampled sample means
hist( bootstrap_means, breaks=floor(sqrt(num_boot_samples)), freq=F,
      main=paste0("size of subsamples = ",m))
abline( v=mu, col="green", lwd=3)

# Compare the histogram to the actual sampling distribution of the sample mean 
# for Gaussian data
grid = seq( mu-3, mu+3, by=.01)
lines( grid, dnorm( grid, mean=mu, sd=sigma/sqrt(n)), lwd=3)

# Repeat the above analysis for varying sample sizes n and subsample sizes m


# Next, consider the sampling distribution of the sample standard deviation
x = rnorm(n, mean=mu, sd=sigma)

m = 100
num_boot_samples = 300
bootstrap_sd = rep( NA, num_boot_samples)
for(k in 1:num_boot_samples){
	sub_x = sample( x, size=m, replace=T)
	bootstrap_sd[k] = sd(sub_x)
}

# Plot the histogram of the subsampled sample variances
hist( bootstrap_sd^2, breaks=floor(sqrt(num_boot_samples)), freq=F,
      main=paste0("size of subsamples = ",m))
abline( v=sigma^2, col="green", lwd=3)

# Compare the histogram to the actual sampling distribution of the sample 
# standard deviation for Gaussian data
grid = seq( sigma^2-3, sigma^2+3, by=.01)
lines( grid, (n-1)*dchisq( (n-1)*grid/sigma^2, df=n-1)/sigma^2, lwd=3)


# A bootstrapped confidence interval can be constructed for both mu and sigma by
# computing the empirical alpha/2 and 1-alpha/2 quantiles.  This approach is 
# called the percentile bootstrap confidence interval

# Percentile bootstrap confidence interval for the mean
alpha = .05
lower = quantile( bootstrap_means, probs=alpha/2); lower
upper = quantile( bootstrap_means, probs=1-alpha/2); upper
mu

# Percentile bootstrap confidence interval for the standard deviation
alpha = .05
lower = quantile( bootstrap_sd, probs=alpha/2); lower
upper = quantile( bootstrap_sd, probs=1-alpha/2); upper
sigma

# How about the coverage of these confidence intervals?

# Confidence interval for mu
num_sims = 300
m = 1000
num_boot_samples = 1000
nominal_coverage = seq( .01, .99, by=.01)
coverage = rep( 0, length(nominal_coverage))
for(j in 1:num_sims){
	
	x = rnorm(n, mean=mu, sd=sigma)

	bootstrap_means = rep( NA, num_boot_samples)
	for(k in 1:num_boot_samples){
		sub_x = sample( x, size=m, replace=T)
		bootstrap_means[k] = mean(sub_x)
	}
	
	for(r in 1:length(nominal_coverage)){
		alpha = 1-nominal_coverage[r]
		lower = quantile( bootstrap_means, probs=alpha/2)
		upper = quantile( bootstrap_means, probs=1-alpha/2)
		if(lower < mu & mu < upper) coverage[r] = coverage[r] + 1
	}
}
coverage = coverage / num_sims

plot( nominal_coverage, coverage, xlab="nominal coverage", main="CI for mean",
      ylab="empirical coverage")
lines( c(0,1), c(0,1), col="green")

# Confidence interval for sigma
num_sims = 300
m = 700
num_boot_samples = 300
nominal_coverage = seq( .01, .99, by=.01)
coverage = rep( 0, length(nominal_coverage))
for(j in 1:num_sims){
	
	x = rnorm(n, mean=mu, sd=sigma)

	bootstrap_sd = rep( NA, num_boot_samples)
	for(k in 1:num_boot_samples){
		sub_x = sample( x, size=m, replace=T)
		bootstrap_sd[k] = sd(sub_x)
	}
	
	for(r in 1:length(nominal_coverage)){
		alpha = 1-nominal_coverage[r]
		lower = quantile( bootstrap_sd, probs=alpha/2)
		upper = quantile( bootstrap_sd, probs=1-alpha/2)
		if(lower < sigma & sigma < upper) coverage[r] = coverage[r] + 1
	}
}
coverage = coverage / num_sims

plot( nominal_coverage, coverage, xlab="nominal coverage", main="CI for mean",
      ylab="empirical coverage")
lines( c(0,1), c(0,1), col="green")

















