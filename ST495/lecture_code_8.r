

# Function to implement the gradient descent algorithm
gradient_descent = function( gradient, eta=.1, x0, epsilon=10^-10){

	t = 2
	trace = rbind( x0, x0 + 2*epsilon)
	# Begin the gradient descent algorithm
	while( sum((trace[t,] - trace[t-1,])^2)^.5 >= epsilon ){
		
		# Update the parameters
		trace = rbind( trace, trace[t,] - eta * gradient(trace[t,]))
	  #print(trace[t,])		 
		t = t +1
	}
	
	return(trace)
}


# Test the gradient descent on a simple quadratic function
f = function(x)  x^2
f = Vectorize(f)
gradient_f = function(x)  2*x

trace = gradient_descent( gradient=gradient_f, x0=2)

grid = seq( -4, 4, by=.1)

# Plot the function and the trace in side-by-side panels
par(mfrow=c(1,2))
plot( grid, f(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, f(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(f(trace), main="Trace plot", xlab="iteration", ylab=NA)

# Consider the effect of changing the learning rate
trace = gradient_descent( gradient=gradient_f, x0=2, eta=.5)
par(mfrow=c(1,2))
plot( grid, f(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, f(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(f(trace), main="Trace plot", xlab="iteration", ylab=NA)

# How about a learning rate of eta = 1?
trace = gradient_descent( gradient=gradient_f, x0=2, eta=1)

# How about initializing the algorihtm at a different value?
p = 1
x0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
trace = gradient_descent( gradient=gradient_f, x0=x0)

par(mfrow=c(1,2))
plot( grid, f(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, f(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(f(trace), main="Trace plot", xlab="iteration", ylab=NA)

dev.off()

g = function(x)  6*x^3 + x^4 
g = Vectorize(g)
gradient_g = function(x) 18*x^2 + 4*x^3

grid = seq( -7, 3, by=.1)
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)

# Fist initialize at x0 = -6
trace = gradient_descent( gradient=gradient_g, x0=-6, eta=.01)
par(mfrow=c(1,2))
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, g(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(g(trace), main="Trace plot", xlab="iteration", ylab=NA)

# Next initialize at x0 = -2
trace = gradient_descent( gradient=gradient_g, x0=-2, eta=.01)
par(mfrow=c(1,2))
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, g(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(g(trace), main="Trace plot", xlab="iteration", ylab=NA)

# What if we initialize at the saddle point x = 0?
trace = gradient_descent( gradient=gradient_g, x0=0, eta=.01)
par(mfrow=c(1,2))
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, g(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(g(trace), main="Trace plot", xlab="iteration", ylab=NA)

# How about initializing at x0 = 2?
trace = gradient_descent( gradient=gradient_g, x0=2, eta=.01, epsilon=10^-4)
par(mfrow=c(1,2))
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, g(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(g(trace), main="Trace plot", xlab="iteration", ylab=NA)

# How about random initialization?
p = 1
x0 = runif(p) * 2*sqrt(6/p) + (-sqrt(6/p))
trace = gradient_descent( gradient=gradient_g, x0=x0, eta=.01, epsilon=10^-4)
par(mfrow=c(1,2))
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, g(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(g(trace), main="Trace plot", xlab="iteration", ylab=NA)



# How about adding some momentum to the gradient descent algorithm?
# https://en.wikipedia.org/wiki/Stochastic_gradient_descent

gd_momentum = function( gradient, eta=.01, alpha=.01, x0, epsilon=10^-6){

	t = 2
	trace = c( x0, x0 + 2*epsilon)
	dx = 2*epsilon
	
	# Begin the gradient descent algorithm
	while( sum((trace[t] - trace[t-1])^2)^.5 >= epsilon ){
				
		# Update the parameters
		trace = c( trace, trace[t] - eta * gradient(trace[t]) + alpha * dx)
	  print(trace[t])		 
		t = t +1
		dx = trace[t] - trace[t-1]
	}
	
	return(trace)
}

trace = gd_momentum( gradient=gradient_g, x0=2, eta=.01, alpha=.4)
par(mfrow=c(1,2))
plot( grid, g(grid), type="l", main="Objective function", xlab="x", ylab=NA)
lines( trace, g(trace), col="green", type="p")
abline( v=trace[-1], col="green")
abline( v=trace[1], col="red")
plot(g(trace), main="Trace plot", xlab="iteration", ylab=NA)









