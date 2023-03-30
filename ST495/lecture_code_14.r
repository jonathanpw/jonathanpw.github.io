
# CONSTRUCT REGRESSION DATA TO ILLUSTRATE THE DIFFERENCES IN THESE APPROACHES
# THAT MIGHT BE OBSERVED FROM DATA.  APPROACH 1 SHOULD DO BETTER THAN APPROACH 2
# FOR MAYBE HETEROSCEDASTIC DATA.  APPROACH 2B SHOULD BE BETTER THAN APPROACH 2A

# Bootstrapping for linear regression

n = 100
p = 2
sigma = 1

beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( runif(n*(p-1)), ncol=p-1))
y = X %*% beta + rnorm( n, mean=0, sd=sigma)

# First we will sub-sample from the joint distribution of (x_i, y_i)
m = 70 # size of each bootstrap sample
num_boot_samples = 300
bootstrap_beta = matrix( NA, num_boot_samples, p)
for(k in 1:num_boot_samples){
	sub_index = sample( 1:n, size=m, replace=T)
	X_star = X[sub_index,]
	y_star = y[sub_index]
	bootstrap_beta[k,] = solve(t(X_star)%*%X_star) %*% t(X_star) %*% y_star	
}

# Histogram of the bootstrapped sampling distribution for the estimated beta_0
hist( bootstrap_beta[,1], breaks=floor(sqrt(num_boot_samples)), freq=F,
      main="bootstrapped sampling distribution for beta_0_hat")
abline( v=beta[1], col="green", lwd=3)
	
# Since we generated the regression data with Gaussian errors, we know the true
# sampling distribution.  So overlay the true density on the histogram
grid = seq( beta[1]-3, beta[1]+3, by=.01)
lines( grid, dnorm( grid, mean=beta[1], sd=sqrt(solve(t(X)%*%X)[1,1])*sigma), 
       lwd=3)

# Histogram of the bootstrapped sampling distribution for the estimated beta_1
hist( bootstrap_beta[,2], breaks=floor(sqrt(num_boot_samples)), freq=F,
     main="bootstrapped sampling distribution for beta_1_hat")
abline( v=beta[2], col="green", lwd=3)

# Since we generated the regression data with Gaussian errors, we know the true
# sampling distribution.  So overlay the true density on the histogram
grid = seq( beta[2]-3, beta[2]+3, by=.01)
lines( grid, dnorm( grid, mean=beta[2], sd=sqrt(solve(t(X)%*%X)[2,2])*sigma), 
      lwd=3)
			

# -----------------------------------------------------------------------------
# Alternatively, try bootstrapping by subsampling residuals from the empirical
# residuals from the observed data set
n = 100
p = 2
sigma = 1

beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( runif(n*(p-1)), ncol=p-1))
y = X %*% beta + rnorm( n, mean=0, sd=sigma)

beta_hat = solve(t(X)%*%X) %*% t(X) %*% y
u_hat = y - X%*%beta_hat

num_boot_samples = 300
bootstrap_beta = matrix( NA, num_boot_samples, p)
for(k in 1:num_boot_samples){
	u_star = matrix( sample( u_hat, size=n, replace=T), ncol=1)	
	y_star = X %*% beta_hat + u_star
	bootstrap_beta[k,] = solve(t(X)%*%X) %*% t(X) %*% y_star	
}


# Histogram of the bootstrapped sampling distribution for the estimated beta_0
hist( bootstrap_beta[,1], breaks=floor(sqrt(num_boot_samples)), freq=F,
      main="bootstrapped sampling distribution for beta_0_hat")
abline( v=beta[1], col="green", lwd=3)
	
# Since we generated the regression data with Gaussian errors, we know the true
# sampling distribution.  So overlay the true density on the histogram
grid = seq( beta[1]-3, beta[1]+3, by=.01)
lines( grid, dnorm( grid, mean=beta[1], sd=sqrt(solve(t(X)%*%X)[1,1])*sigma), 
       lwd=3)

# Histogram of the bootstrapped sampling distribution for the estimated beta_1
hist( bootstrap_beta[,2], breaks=floor(sqrt(num_boot_samples)), freq=F,
     main="bootstrapped sampling distribution for beta_1_hat")
abline( v=beta[2], col="green", lwd=3)

# Since we generated the regression data with Gaussian errors, we know the true
# sampling distribution.  So overlay the true density on the histogram
grid = seq( beta[2]-3, beta[2]+3, by=.01)
lines( grid, dnorm( grid, mean=beta[2], sd=sqrt(solve(t(X)%*%X)[2,2])*sigma), 
      lwd=3)



# -----------------------------------------------------------------------------
# Next, try bootstrapping by subsampling residuals from the standarized
# empirical residuals from the observed data set
n = 100
p = 2
sigma = 1

beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( runif(n*(p-1)), ncol=p-1))
y = X %*% beta + rnorm( n, mean=0, sd=sigma)

beta_hat = solve(t(X)%*%X) %*% t(X) %*% y
u_hat = y - X%*%beta_hat

Px = X %*% solve(t(X)%*%X) %*% t(X)
h = diag(Px)

r = u_hat / sqrt(1 - h)
r_bar = mean(r)


num_boot_samples = 300
bootstrap_beta = matrix( NA, num_boot_samples, p)
for(k in 1:num_boot_samples){
	u_star = matrix( sample( r - r_bar, size=n, replace=T), ncol=1)	
	y_star = X %*% beta_hat + u_star
	bootstrap_beta[k,] = solve(t(X)%*%X) %*% t(X) %*% y_star	
}

# Histogram of the bootstrapped sampling distribution for the estimated beta_0
hist( bootstrap_beta[,1], breaks=floor(sqrt(num_boot_samples)), freq=F,
      main="bootstrapped sampling distribution for beta_0_hat")
abline( v=beta[1], col="green", lwd=3)
	
# Since we generated the regression data with Gaussian errors, we know the true
# sampling distribution.  So overlay the true density on the histogram
grid = seq( beta[1]-3, beta[1]+3, by=.01)
lines( grid, dnorm( grid, mean=beta[1], sd=sqrt(solve(t(X)%*%X)[1,1])*sigma), 
       lwd=3)

# Histogram of the bootstrapped sampling distribution for the estimated beta_1
hist( bootstrap_beta[,2], breaks=floor(sqrt(num_boot_samples)), freq=F,
     main="bootstrapped sampling distribution for beta_1_hat")
abline( v=beta[2], col="green", lwd=3)

# Since we generated the regression data with Gaussian errors, we know the true
# sampling distribution.  So overlay the true density on the histogram
grid = seq( beta[2]-3, beta[2]+3, by=.01)
lines( grid, dnorm( grid, mean=beta[2], sd=sqrt(solve(t(X)%*%X)[2,2])*sigma), 
      lwd=3)







