
# Gradient descent with some adaptive tuning and max iterations
GD = function( gradient, f, eta=.01, x0, epsilon=10^-6, max_iter=10^4){

	t = 2
	trace = matrix( NA, ncol=length(x0), nrow=max_iter)
	trace[1:2,] =	rbind( x0, x0 + 2*epsilon)
	neg_l = f(trace[t,])
	# Begin the gradient descent algorithm
	while( sum((trace[t,] - trace[t-1,])^2)^.5 >= epsilon & t < max_iter ){
		
		# Update the parameters
		trace[t+1,] = trace[t,] - eta * gradient(trace[t,])
	  #print(trace[t+1,])		
		t = t +1
		
		# If negative log-likelihood increases, then reduce learning rate
		neg_l_new = f(trace[t,]) 
		#cat(neg_l_new, "  ", neg_l, "\n")
		if(neg_l_new >= neg_l) eta = eta * .9 
		neg_l = neg_l_new
	}
	cat("final learning rate = ", eta, "\n")
	
	if(t == max_iter){
		print("Warning: max iterations exceeded")
		trace[max_iter,] = NA
	}
		
	return(trace[1:t,])
}


# Generate synthetic logistic regrerssion data and implement a simulation study 
# to observe the sampling distribution of the MLE of the coefficients computed
# via gradient descent
n = 1000
p = 2
beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( rnorm(n*(p-1)), ncol=p-1))

num_sims = 300
sampling_dist = matrix( NA, num_sims, p);
for(k in 1:num_sims){
	
	# Generate a synthetic data set
	py = 1 / ( 1 + exp(-X %*% beta))
	y = c(runif(n) < py)
	
	df = function(b){
		sigmoid = 1 / (1 + exp(-c(X %*% b)))
		return( colSums((sigmoid - y) * X) )
	}
	
	neg_l = function(b){
		sigmoid = 1 / (1 + exp(-c(X %*% b)) +10^-6) # 10^-6 for numerical stability
		return( -sum(y * log(sigmoid) + (1 - y) * log(1 - sigmoid)) )
	}
	
	b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
	trace = GD( gradient=df, f=neg_l, x0=b0)
	sampling_dist[k,] = tail( trace, 1)
	
	print(k)
}


# Plot the sampling distribution of the components of the MLEs
par(mfrow=c(2,2))
for(j in 1:p){
	hist( sampling_dist[,j], breaks=floor(sqrt(num_sims)), main=paste0("beta_",j),
	      xlab=NA, freq=F)
	abline( v=beta[j], col="green", lwd=3)
} 




# -----------------------------------------------------------------------------
# Stochastic gradient descent
SGD = function( r, n, gradient, eta=.01, x0, epsilon=10^-3, max_iter=10^4){

	t = 2
	trace = matrix( NA, ncol=length(x0), nrow=max_iter)
	trace[1:2,] =	rbind( x0, x0 + 2*epsilon)
	# Begin the gradient descent algorithm
	while( sum((trace[t,] - trace[t-1,])^2)^.5 >= epsilon & t < max_iter ){
		
		index = sample( 1:n, size=r, replace=F)
		
		# Update the parameters
		trace[t+1,] = trace[t,] - eta * gradient( trace[t,], index)
	  #print(trace[t+1,])		
		t = t +1
	}
	cat("final learning rate = ", eta, "\n")
	
	if(t == max_iter){
		print("Warning: max iterations exceeded")
		trace[max_iter,] = NA
	}
		
	return(trace[1:t,])
}


# Test on sythetic logistic regrerssion data
n = 2000
p = 2
beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( rnorm(n*(p-1)), ncol=p-1))

py = 1 / ( 1 + exp(-X %*% beta))
y = c(runif(n) < py)

df_r = function( b, index){
	sigmoid = 1 / (1 + exp(-c(X[index,] %*% b)))
	return( colSums((sigmoid - y[index]) * X[index,]) )
}

b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
trace = SGD( r=100, n=n, gradient=df_r, x0=b0); tail( trace, 2); beta

# Plot the trace of the negative log-likelihood
neg_l_r = function( b, index){
	sigmoid = 1 / (1 + exp(-c(X[index,] %*% b)) +10^-6) 
	return( -sum(y[index] * log(sigmoid) + (1 - y[index]) * log(1 - sigmoid)) )
}
profile = rep( NA, nrow(trace))
for(t in 1:nrow(trace)) profile[t] = neg_l_r( b=trace[t,], index=1:n)
plot( profile[-c(1:10)], main="Trace plot", xlab="iteration", ylab=NA, type="l")


# Generate synthetic logistic regrerssion data and implement a simulation study 
# to observe the sampling distribution of the MLE of the coefficients computed
# via stochastic gradient descent
num_sims = 300
sampling_dist = matrix( NA, num_sims, p);
for(k in 1:num_sims){
	
	# Generate a synthetic data set
	py = 1 / ( 1 + exp(-X %*% beta))
	y = c(runif(n) < py)

	b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
	trace = SGD( r=1000, n=n, gradient=df_r, x0=b0)
	sampling_dist[k,] = tail( trace, 1)
	
	print(k)
}


# Plot the sampling distribution of the components of the MLEs
par(mfrow=c(2,2))
for(j in 1:p){
	hist( sampling_dist[,j], breaks=floor(sqrt(num_sims)), main=paste0("beta_",j),
	      xlab=NA, freq=F)
	abline( v=beta[j], col="green", lwd=3)
} 




# -----------------------------------------------------------------------------
# Mini batch gradient descent
MBGD = function( r, n, gradient, eta=.01, x0, epsilon=10^-3, max_iter=10^4){

	t = 2
	trace = matrix( NA, ncol=length(x0), nrow=max_iter*n)
	trace[1:2,] =	rbind( x0, x0 + 2*epsilon)
	# Begin the gradient descent algorithm
	while( sum((trace[t,] - trace[t-1,])^2)^.5 >= epsilon & t < max_iter ){
		
		for(s in 0:(floor(n/r)-1)){
			index = (s*r +1):(s*r +r)
		
			# Update the parameters
			trace[t+1,] = trace[t,] - eta * gradient( trace[t,], index)
		  #print(trace[t+1,])		
			t = t +1
		}
	}
	cat("final learning rate = ", eta, "\n")
	
	if(t == max_iter){
		print("Warning: max iterations exceeded")
		trace[max_iter,] = NA
	}
		
	return(trace[1:t,])
}


# Test on sythetic logistic regrerssion data
n = 1000
p = 2
beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( rnorm(n*(p-1)), ncol=p-1))

py = 1 / ( 1 + exp(-X %*% beta))
y = c(runif(n) < py)

b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
trace = MBGD( r=100, n=n, gradient=df_r, x0=b0); tail( trace, 2); beta

# Plot the trace of the negative log-likelihood
profile = rep( NA, nrow(trace))
for(t in 1:nrow(trace)) profile[t] = neg_l_r( b=trace[t,], index=1:n)
plot( profile[-c(1:1000)], main="Trace plot", xlab="iteration", ylab=NA, type="l")


# Generate synthetic logistic regrerssion data and implement a simulation study 
# to observe the sampling distribution of the MLE of the coefficients computed
# via stochastic gradient descent
num_sims = 300
sampling_dist = matrix( NA, num_sims, p);
for(k in 1:num_sims){
	
	# Generate a synthetic data set
	py = 1 / ( 1 + exp(-X %*% beta))
	y = c(runif(n) < py)

	b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
	trace = MBGD( r=100, n=n, gradient=df_r, x0=b0)
	sampling_dist[k,] = tail( trace, 1)
	
	print(k)
}


# Plot the sampling distribution of the components of the MLEs
par(mfrow=c(2,2))
for(j in 1:p){
	hist( sampling_dist[,j], breaks=floor(sqrt(num_sims)), main=paste0("beta_",j),
	      xlab=NA, freq=F)
	abline( v=beta[j], col="green", lwd=3)
} 































