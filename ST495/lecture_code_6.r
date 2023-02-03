# Linear models and the sampling distribution of the least squares estimator
# assuming Gaussian errors


#n = 100
#n = 300
n = 1000
p = 4
sigma = .25
#sigma = 1
#sigma = 2

beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( runif(n*(p-1)), ncol=p-1))
XtX_inv = solve(t(X) %*% X)


# Simulate a large number of data sets and least squares estimators 
#num_sims = 100
#num_sims = 300
num_sims = 10000
beta_hat_mat = matrix( NA, nrow=num_sims, ncol=p)
for(k in 1:num_sims){
	y = X %*% beta + rnorm( n, mean=0, sd=sigma)
	beta_hat_mat[k,] = XtX_inv %*% t(X) %*% y
}


# Plot the sampling distributions of the least squares estimates

pdf(paste0("sampling_dist_n",n,"_num_sims",num_sims,".pdf"))
par(mfrow=c(2,2))
for(j in 1:p){
	
	grid = seq( beta[j]-.5, beta[j]+.5, by=.01)
	
	hist( beta_hat_mat[,j], freq=F, main=paste0("beta_hat_",j), xlab=NULL,
	      xlim=c(beta[j]-.5,beta[j]+.5), breaks=floor(sqrt(n)))
	abline( v=beta[j], col="green", lwd=3)
	
	sd_beta_j = sigma * sqrt(XtX_inv[j,j])
	lines(grid, dnorm( grid, mean=beta[j], sd=sd_beta_j), lwd=3)
}
dev.off()


# Check whether confidence intervals for the least squares estimates have the
# correct coverage
level = .99
coverage = rep( 0, p)
for(k in 1:num_sims){
	for(j in 1:p){
		sd_beta_j = sigma * sqrt(XtX_inv[j,j])
		lower = beta_hat_mat[k,j] - sd_beta_j * qnorm( (1-level)/2, lower.tail=F)
		upper = beta_hat_mat[k,j] + sd_beta_j * qnorm( (1-level)/2, lower.tail=F)
	
		coverage[j] = coverage[j] + (lower < beta[j] & beta[j] < upper)
	}
}
coverage = coverage / num_sims; coverage



