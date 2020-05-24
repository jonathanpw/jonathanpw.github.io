X = matrix( c(1,1,0,
	            1,0,1,
							1,1,1,
							1,1,0,
							1,0,1), ncol=3, byrow=T)

# Gram-Schmidt process for constucting an orthogonal set of vectors from a 
# linearly independent set of vectors
GS = function(X){
	
	n = nrow(X)
	p = ncol(X)
	
	U = matrix( 0, n, p)
	U[,1] = X[,1]
	null_proj = diag(n)
	
	for(j in 2:p){
	
		U_prev = U[,j-1, drop=F]
	
		null_proj = null_proj - U_prev %*% t(U_prev) / sum(U_prev^2)
	
		U[,j] = null_proj %*% X[,j]
	}
	
	return(U)
}

U = GS(X)

# To verify that the columns of U are orthogonal, check that the off-diagonal
# components of U'U are zero
round( t(U) %*% U, 12)

# Next, normalize the columns of U
Q = U %*% diag( 1 / sqrt( diag(t(U) %*% U) ))
round( t(Q) %*% Q, 12)

# Finally, to reconstruct X, construct the upper triangular matrix R in the 
# QR decomposition
R_mat = function( U, X){
	
	p = ncol(X)
	
	S = diag(p)
	for(i in 1:(p-1)){
		
		U_i = U[,i, drop=F]
		
		for(j in (i+1):p)  S[i,j] = t(U_i) %*% X[,j, drop=F] / sum(U_i^2)
	}
	
	R = diag(sqrt( diag(t(U) %*% U) )) %*% S
	
	return(R)
}

R = R_mat( U, X)
round( Q %*% R, 12)  # Should be equal to X


