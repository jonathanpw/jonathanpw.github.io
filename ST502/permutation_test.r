# Generate two data sets
x = rnorm( n=33, mean=79, sd=5)
y = rnorm( n=41, mean=79, sd=1)

# Calculate the test statistic
test_stat = mean(x) - mean(y)

N = 10000
dist_test_stat = rep( 0, N)
for(i in 1:N){
	combined_data = c( x, y)
	permuted_x = sample( combined_data, size=length(x), replace=F)
	permuted_y = setdiff( combined_data, permuted_x)
	dist_test_stat[i] = mean(permuted_x) - mean(permuted_y)
}

p_value_lower = mean(dist_test_stat <= test_stat)
p_value_upper = 1 - p_value_lower
p_value_2sided = mean( dist_test_stat <= -abs(test_stat) |  
                       dist_test_stat >= abs(test_stat))

hist( dist_test_stat, main='Permutation distribution of test statistic',
      xlim=c( min(dist_test_stat,test_stat), max(dist_test_stat,test_stat)), 
      xlab=paste0('empirical p-value = ',round(p_value_lower,4),' (lower), ',
																         round(p_value_upper,4),' (upper), ',
																         round(p_value_2sided,4),' (two-sided)'))
abline( v=test_stat, col='red', lwd=3)
