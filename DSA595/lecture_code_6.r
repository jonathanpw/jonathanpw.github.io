# Monte Carol integration

n = 10000
alpha = 1
beta = 3
x = rgamma(n=n, shape=alpha, rate =beta)

grid = seq(0, 4, by=.01)
#plot( grid, dgamma(grid, shape=alpha, rate =beta))

# Approximate E(X)
x_bar = mean(x); x_bar
# Compare to the true mean of E(X) = alpha / beta
alpha / beta

# Approximate Var(X)
x2_bar = mean(x^2)
var_x = x2_bar - x_bar^2; var_x
# Compare to the true mean of Var(X) = alpha / beta^2
alpha / beta^2

# Approximate the probability that 0 < x < 6
b = .8
a = 0
prob = pgamma(b, shape=alpha, rate=beta) - pgamma(a, shape=alpha, rate=beta)
prob

MC_integral = mean( a < x & x < b ); MC_integral



# Importance sampling

n = 10000
x = rnorm(n=n, mean=0, sd=4)

g = function(x) x^2
g = Vectorize(g)

grid = seq( -1, 2, by=.01)
plot( grid, g(grid), type="l")

hist(x, freq=F, breaks=floor(sqrt(n)))

mean( (-1 < x & x < 2) * g(x) / dnorm( x, mean=0, sd=4))













