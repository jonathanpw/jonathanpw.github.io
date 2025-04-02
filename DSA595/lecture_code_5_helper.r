# This script contains the code for the mcmc and its helper functions

library(mvtnorm, quietly=T)




# -----------------------------------------------------------------------------
# Function to compute the log-likelihood of the data
# -----------------------------------------------------------------------------
log_density = function( par, prior_par, par_index, y, X){
	
	n = length(y)
	beta = matrix( par[par_index$beta], ncol=1)
	sigma_sq = exp(par[par_index$log_sigma_sq])
	
	log_fn = dmvnorm( x=c(y), mean=c(X%*%beta), sigma=diag(n)*sigma_sq, log=T)

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
	
	group = list( par_index$beta, 
		            par_index$log_sigma_sq)
	n_group = length(group)
	
	pcov = list();	for(j in 1:n_group)  pcov[[j]] = diag(length(group[[j]]))
	pscale = rep( .0001, n_group)
	accept = rep( 0, n_group)
	
	# Evaluate the log_post of the initial par 
	log_post_prev = log_density( par, prior_par, par_index, y, X)
	if(!is.finite(log_post_prev)){
		print("Infinite log-posterior; choose better initial parameters")
		break
	}
	
	# Begin the MCMC algorithm --------------------------------------------------
	chain[1,] = par
	for(ttt in 2:steps){
		for(j in 1:n_group){
			
			# Propose an update
			ind_j = group[[j]]
			proposal = par
			proposal[ind_j] = rmvnorm(n=1, mean=par[ind_j], sigma=pcov[[j]]*pscale[j])
		
			# Compute the log density for the proposal
			log_post = log_density( proposal, prior_par, par_index, y, X)

			# Only propose valid parameters during the burnin period
			if(ttt < burnin){
				while(!is.finite(log_post)){
					print('bad proposal')
					proposal = par
					proposal[ind_j] = rmvnorm( n=1, mean=par[ind_j],
						                              sigma=pcov[[j]]*pscale[j])
				  log_post = log_density( proposal, prior_par, par_index, y, X)
				}		
			}
		
			# Evaluate the Metropolis-Hastings ratio
			if( log_post - log_post_prev > log(runif(1,0,1)) ){
				log_post_prev = log_post
				par[ind_j] = proposal[ind_j]
				accept[j] = accept[j] +1
			}
			chain[ttt,ind_j] = par[ind_j]

			# Proposal tuning scheme ------------------------------------------------
			if(ttt < burnin){
				# During the burnin period, update the proposal covariance in each step 
				# to capture the relationships within the parameters vectors for each 
				# transition.  This helps with mixing.
				if(ttt == 100)  pscale[j] = 1
					
				if(length(ind_j) > 1){
					if(100 <= ttt & ttt <= 2000){  
						temp_chain = chain[1:ttt,ind_j]
						pcov[[j]] = cov(temp_chain[ !duplicated(temp_chain),, drop=F])
					
					} else if(2000 < ttt){  
						temp_chain = chain[(ttt-2000):ttt,ind_j]
						pcov[[j]] = cov(temp_chain[ !duplicated(temp_chain),, drop=F])
					}
					if( sum( is.na(pcov[[j]]) ) > 0)  pcov[[j]] = diag( length(ind_j) )
				}

				# Tune the proposal covariance for each transition to achieve 
				# reasonable acceptance ratios.
				if(ttt %% 30 == 0){ 
					if(ttt %% 480 == 0){  
						accept[j] = 0  

					} else if( accept[j] / (ttt %% 480) < .4 ){ 
						pscale[j] = (.75^2)*pscale[j] 
		
					} else if( accept[j] / (ttt %% 480) > .5 ){ 
						pscale[j] = (1.25^2)*pscale[j]
					} 
				}
			}
			# -----------------------------------------------------------------------
		}
		# Restart the acceptance ratio at burnin.
		if(ttt == burnin)  accept[j] = 0

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





