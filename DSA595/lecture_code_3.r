source("lecture_code_3_helper.r")


p = 2
#beta = runif( n=p, min=1, max=5) * (-1)^(runif(p) < .5)
beta = rep(3, p)

n = 100
X = cbind( 1, matrix( runif(n*(p-1)), ncol=p-1))
#X[,2] = 60*X[,2]

y = X %*% beta + rnorm( n, mean=0, sd=1)




init_par = rep( 0, p)
prior_par = list( prior_mean=rep( 0, p), prior_sd=rep( 5, p))
steps = 20000
burnin = 5000


mcmc_out = mcmc_routine( y, X, init_par, prior_par, steps, burnin)



# ----------------------------------------------------------------------------
# Create mcmc trace plots and histograms
# ----------------------------------------------------------------------------
n_post = 15000
index_post = (steps - burnin - n_post + 1):(steps - burnin)
chain = mcmc_out$chain[index_post,]

# Plot and save the mcmc trace plots and histograms.
par_mean = par_median = upper = lower = rep( NA, ncol(chain))
pdf('trace_plot_adaptive_scaling_only.pdf')
par(mfrow=c(3, 2))
for(r in 1:ncol(chain)){

	plot( NULL, xlab=NA, ylab=NA, xlim=c(1,length(index_post)), 
	      main=paste0( "par_", r), ylim=range(chain[,r]) )

	lines( chain[,r], type='p', col='black')

	par_mean[r] = round( mean(chain[,r]), 4)
	par_median[r] = round( median(chain[,r]), 4)
	upper[r] = round( quantile( chain[,r], prob=.975), 4)
	lower[r] = round( quantile( chain[,r], prob=.025), 4)

	hist( chain[,r], ylab=NA, main=NA, freq=F,
		    breaks=sqrt(nrow(chain)),
	      xlab=paste0('Mean = ',toString(par_mean[r]),
				             ' Median = ',toString(par_median[r])))
	abline( v=upper[r], col='red', lwd=2, lty=2)
	abline( v=lower[r], col='purple', lwd=2, lty=2)
	
	abline( v=beta[r], col='green', lwd=2, lty=2)
}
dev.off()

# ----------------------------------------------------------------------------