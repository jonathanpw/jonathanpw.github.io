# This script contains the code for the mcmc and its helper functions

library(mvtnorm, quietly=T)




# -----------------------------------------------------------------------------
# Function to compute the log-likelihood of the data
# -----------------------------------------------------------------------------
log_density = function( par, prior_par, par_index, y, X){
	
	n = length(y)
	beta = matrix( par[par_index$beta], ncol=1)
	sigma = sqrt(exp(par[par_index$log_sigma_sq]))
	
	log_fn = dmvnorm( x=c(y), mean=c(X%*%beta), sigma=diag(n)*sigma, log=T)

	beta_mean = prior_par$prior_mean[par_index$beta]
	beta_sd = diag(prior_par$prior_sd[par_index$beta])
	
	log_pd_beta = dmvnorm( x=c(beta), mean=beta_mean, sigma=beta_sd, log=T)
	log_pd_var = -par[par_index$log_sigma_sq]
	
	return( log_fn + log_pd_beta + log_pd_var)
}
# -----------------------------------------------------------------------------





# -----------------------------------------------------------------------------
# The mcmc routine for sampling the parameters
# -----------------------------------------------------------------------------
mcmc_routine = function( y, X, init_par, prior_par, par_index, steps, burnin){

	par = init_par 
	n = length(y)
	n_par = length(par)
	chain = matrix( 0, steps, n_par)
	
	pcov = diag(n_par)
	pscale = .0001
	accept = 0
	
	# Evaluate the log_post of the initial par 
	log_post_prev = log_density( par, prior_par, par_index, y, X)
	if(!is.finite(log_post_prev)){
		print("Infinite log-posterior; choose better initial parameters")
		break
	}
	
	# Begin the MCMC algorithm --------------------------------------------------
	chain[1,] = par
	for(ttt in 2:steps){
		
		# Propose an update
		proposal = rmvnorm( n=1, mean=par, sigma=pcov*pscale)
		
		# Compute the log density for the proposal
		log_post = log_density( proposal, prior_par, par_index, y, X)

		# Only propose valid parameters during the burnin period
		if(ttt < burnin){
			while(!is.finite(log_post)){
				print('bad proposal')
				proposal = rmvnorm( n=1, mean=par, sigma=pcov*pscale)
			  log_post = log_density( proposal, prior_par, par_index, y, X)
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
			# During the burnin period, update the proposal covariance in each step 
			# to capture the relationships within the parameters vectors for each 
			# transition.  This helps with mixing.
			if(ttt == 100)  pscale = 1
				
			if(100 <= ttt & ttt <= 2000){  
				temp_chain = chain[1:ttt,]
				pcov = cov(temp_chain[ !duplicated(temp_chain),, drop=F])
				
			} else if(2000 < ttt){  
				temp_chain = chain[(ttt-2000):ttt,]
				pcov = cov(temp_chain[ !duplicated(temp_chain),, drop=F])
			}
			if( sum( is.na(pcov) ) > 0)  pcov = diag(n_par)

			# Tune the proposal covariance for each transition to achieve 
			# reasonable acceptance ratios.
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

		if(ttt%%100==0)  cat('--->',ttt,'\n')
	}
	# ---------------------------------------------------------------------------
	
	print(accept/(steps-burnin))
	cat("pscale = ",pscale,"\n")
	cat("pcov = ","\n")
	print(pcov)
	return(list( chain=chain[burnin:steps,], accept=accept/(steps-burnin),
	             pscale=pscale))
}
# -----------------------------------------------------------------------------





