
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
