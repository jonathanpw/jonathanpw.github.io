# This script contains the code for the mcmc and its helper functions

library(mvtnorm, quietly=T)




# -----------------------------------------------------------------------------
# Function to compute the log-likelihood of the data
# -----------------------------------------------------------------------------
log_density = function( par, prior_par, y, X){
	
	coeffs = matrix( par, ncol=1)
	
	log_fn = dmvnorm( x=c(y), mean=c(X%*%coeffs), sigma=diag(length(y)), log=T)

	mean = prior_par$prior_mean
	sd = diag(prior_par$prior_sd)
	log_prior_dens = dmvnorm( x=par, mean=mean, sigma=sd, log=T)
	
	return( log_fn + log_prior_dens )
}
# -----------------------------------------------------------------------------





# -----------------------------------------------------------------------------
# The mcmc routine for sampling the parameters
# -----------------------------------------------------------------------------
mcmc_routine = function( y, X, init_par, prior_par, steps, burnin){

	par = init_par 
	n = length(y)
	n_par = length(par)
	chain = matrix( 0, steps, n_par)
	
	pscale = .0001
	accept = 0
	
	# Evaluate the log_post of the initial par 
	log_post_prev = log_density( par, prior_par, y, X)
	if(!is.finite(log_post_prev)){
		print("Infinite log-posterior; choose better initial parameters")
		break
	}
	
	# Begin the MCMC algorithm --------------------------------------------------
	chain[1,] = par
	for(ttt in 2:steps){
		
		# Propose an update
		proposal = rmvnorm( n=1, mean=par, sigma=diag(n_par)*pscale)
		
		# Compute the log density for the proposal
		log_post = log_density( proposal, prior_par, y, X)

		# Only propose valid parameters during the burnin period
		if(ttt < burnin){
			while(!is.finite(log_post)){
				print('bad proposal')
				proposal = rmvnorm( n=1, mean=par, sigma=diag(n_par)*pscale)
			  log_post = log_density( proposal, prior_par, y, X)
			}		
		}
		
		# Evaluate the Metropolis-Hastings ratio
		if( log_post - log_post_prev > log(runif(1,0,1)) ){
			log_post_prev = log_post
			par = proposal
			accept = accept +1
		}
		chain[ttt,] = par

		# Proposal tuning scheme ------------------------------------------------
		if(ttt < burnin){
			# During the burnin period, update the proposal scale in each step 
			# to target a desired acceptance rate.  This helps with mixing.
			if(ttt == 100)  pscale = 1
			
			if(ttt %% 30 == 0){ 
				if(ttt %% 480 == 0){  
					accept = 0  

				} else if( accept / (ttt %% 480) < .4 ){ 
					pscale = (.75^2)*pscale
	
				} else if( accept / (ttt %% 480) > .5 ){ 
					pscale = (1.25^2)*pscale
				} 
			}
		}
		# -----------------------------------------------------------------------
		# Restart the acceptance ratio at burnin.
		if(ttt == burnin)  accept = 0

		if(ttt%%1==0)  cat('--->',ttt,'\n')
	}
	# ---------------------------------------------------------------------------
	
	print(accept/(steps-burnin))
	return(list( chain=chain[burnin:steps,], accept=accept/(steps-burnin),
	             pscale=pscale))
}
# -----------------------------------------------------------------------------





