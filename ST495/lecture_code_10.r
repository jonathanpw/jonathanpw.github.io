
# Gradient descent from the previous lecture code
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



# Generate synthetic training data from a logistic regression model
n = 100
p = 2
beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
X = cbind( 1, matrix( rnorm(n*(p-1)), ncol=p-1))
py = 1 / ( 1 + exp(-X %*% beta))
y = c(runif(n) < py)

# Train the model
df = function(b){
	sigmoid = 1 / (1 + exp(-c(X %*% b)))
	return( colSums((sigmoid - y) * X) )
}

neg_l = function(b){
	sigmoid = 1 / (1 + exp(-c(X %*% b)) +10^-6) # 10^-6 for numerical stability
	return( -sum(y * log(sigmoid) + (1 - y) * log(1 - sigmoid)) )
}

b0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
beta_MLE = c(tail( GD( gradient=df, f=neg_l, x0=b0), 1))

# Compute the predicted values on the training data
py_hat = 1 / (1 + exp(-c(X %*% beta_MLE)))
y_hat = (py_hat > .5) # Using a threshold of .5

# False positive rate
FPR = sum(y_hat[y == 0] == 1) / sum(y == 0); FPR

# True postive rate (i.e., sensitivity)
TPR = sum(y_hat[y == 1] == 1) / sum(y == 1); TPR

# True negative rate (i.e., specificity)
TNR = sum(y_hat[y == 0] == 0) / sum(y == 0); TNR

# How do these values change for different thresholds?
grid = seq( 0, 1, by=.001)
FPR = rep( NA, length(grid))
TPR = rep( NA, length(grid))
for(k in 1:length(grid)){
	y_hat = (py_hat > grid[k])
	FPR[k] = sum(y_hat[y == 0] == 1) / sum(y == 0)
	TPR[k] = sum(y_hat[y == 1] == 1) / sum(y == 1)
}

# Plot the ROC curve
plot( FPR, TPR, lwd=2, type="l")
lines( grid, grid, lty="dotted", lwd=2)

# This ROC curve shows how well the logistic regression is able to classify the
# training data.  How well does it generalize to data it was not trained on?
# Generate synthetic testing data from the logistic regression model
X_test = cbind( 1, matrix( rnorm(n*(p-1)), ncol=p-1))
py = 1 / ( 1 + exp(-X_test %*% beta))
y_test = c(runif(n) < py)

py_test_hat = 1 / (1 + exp(-c(X_test %*% beta_MLE)))

FPR_test = rep( NA, length(grid))
TPR_test = rep( NA, length(grid))
for(k in 1:length(grid)){
	y_test_hat = (py_test_hat > grid[k])
	FPR_test[k] = sum(y_test_hat[y_test == 0] == 1) / sum(y_test == 0)
	TPR_test[k] = sum(y_test_hat[y_test == 1] == 1) / sum(y_test == 1)
}

# Add this ROC curve to compare with the ROC on the training data
lines( FPR_test, TPR_test, lwd=2, type="l", col="blue")


# Let's verify that random guessing corresponds to the 45 degree line
FPR_coin = rep( NA, length(grid))
TPR_coin = rep( NA, length(grid))
for(k in 1:length(grid)){
	y_coin = (rep( .5, n) > grid[k])
	FPR_coin[k] = sum(y_coin[y_test == 0] == 1) / sum(y_test == 0)
	TPR_coin[k] = sum(y_coin[y_test == 1] == 1) / sum(y_test == 1)
}
lines( FPR_coin, TPR_coin, lwd=2, type="l", col="red")









