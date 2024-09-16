# Least squares 


# Verify that the least squares solution is consistent with what you have 
# previously learned about least squares estimates for simple linear regression
# Generate synthetic data  
beta = c( 1, -2)
n = 100
x = runif(n)
y = beta[1] + beta[2] * x + rnorm( n, mean=0, sd=.25)

plot( x, y)

# Calculate the coefficient estimates from you linear regression course
beta_1_hat = sum( (x - mean(x))*(y - mean(y)) ) / sum( (x - mean(x))^2 )
beta_0_hat = mean(y) - beta_1_hat * mean(x)

beta_0_hat
beta_1_hat

# Calculate the least squares matrix expression
X = cbind( 1, x)
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y; row.names(beta_hat) = NULL

beta_hat

# Add the least squares line to the scatter plot
lines( x, X %*% beta_hat)

# Alternatively, we could construct this line from the orthogonal projection 
# onto the col(X)
P_x = X %*% solve(t(X) %*% X) %*% t(X) 
y_hat = P_x %*% y

# Add the projection of y as green points to the scatter plot
lines( x, y_hat, type="p", col="green")

# Verify that P_x is idempotent and symmetric
as.logical(prod(round(P_x, 6) == round( P_x %*% P_x, 6)))
as.logical(prod(round(P_x, 6) == round( t(P_x), 6)))


# Note as a simple example of orthogonal projections, the orthogonal projection
# onto the horizontal axis
P = matrix( c( 1, 0,
	             0, 0), ncol=2)

# Plot a random vector in R^2
v = runif(2)
plot( x=c( 0, v[1]), y=c( 0, v[2]), xlim=c(0,1), ylim=c(0,1), type="l")
lines( x=v[1], y=v[2], type="p")

# Plot the orthogonal projection onto the horizontal axis
v_proj = P %*% v
lines( x=c( 0, v_proj[1]), y=c( 0, v_proj[2]), col="green")
lines( x=v_proj[1], y=v_proj[2], type="p", col="green")
