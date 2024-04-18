# Simple power analysis

n = 1000000000
alpha = .01
critical_z = qnorm(1-alpha)

num_sims = 300
mu_grid = seq( .0001, .01, by=.001)
prop_reject = rep( 0, length(mu_grid))
power = rep( NA, length(mu_grid))
for(i in 1:length(mu_grid)){
	#for(k in 1:num_sims){
	
		#x = rnorm( n=n, mean=mu_grid[i], sd=1)
		#z_n = mean(x)*sqrt(n)

		#prop_reject[i] = prop_reject[i] + (z_n > critical_z)
		#}
	
	power[i] = pnorm( critical_z, mean=mu_grid[i]*sqrt(n), sd=1, lower.tail=F)
}
#prop_reject = prop_reject / num_sims


plot( mu_grid, power, type="l", col="green", lwd=4)

plot( mu_grid, prop_reject)
lines( mu_grid, power, col="green", lwd=4)
