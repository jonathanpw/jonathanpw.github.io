# Linear algebra topics



# -----------------------------------------------------------------------------
# Matrix multiplication

matrix_multiply = function( A, B){
	# Function to return the matrix produc AB 
	
	q = dim(A)[2]
	
	# The number of columns of A must match the number of rows of B
	if( q == dim(B)[1] ){
	
		# Denote A as a p by q matrix
		p = dim(A)[1]
	
		# Denote B as a q by m matrix
		m = dim(B)[2]
		
		# Initialize a zero p by m matrix
		C = matrix( 0, nrow=p, ncol=m)
		for(i in 1:p){
			for(j in 1:m){
				for(k in 1:q)  C[i,j] = C[i,j] + A[i,k] * B[k,j]
			}
		}
		return(C)
		
	} else return("ERROR: matrix dimensions are not compatible")
}


# Test the function on an example that is easily worked out analytically 
A = matrix( c(1, 2,
	            3, 4), ncol=2, byrow=T)

B = matrix( c( 1, 1), ncol=1)

AB = matrix_multiply( A, B); AB


# How does this compare to the base R matrix multiply function?
A %*% B
as.logical(prod( AB == A %*% B ))


# Next, perform a Monte Carlo simulation experiment to stress test the code
# First, set the seed so that if the code fails, you can easily reproduce the
# example for which the code fails.
set.seed(1)
for(k in 1:100){
	p = sample( x=1:100, size=1)
	q = sample( x=1:100, size=1)
	m = sample( x=1:100, size=1)
	A = matrix( runif(p*q), nrow=p, ncol=q)
	B = matrix( runif(q*m), nrow=q, ncol=m)
	AB = matrix_multiply( A, B)
	
	print(k)
  if( !as.logical(prod( round( AB, 6) == round( A %*% B,6) )) ){ print("code fail"); break }	
}



# -----------------------------------------------------------------------------
# Systems of linear equations

# Write a function to perform back substitution on an upper triangular matrix
back_substitution = function( M, b){
	
	p = dim(M)[1]
	x = rep( NA, p)
	
	x[p] = b[p] / M[p,p]
	for(i in (p-1):1){
		
		x[i] = b[i] / M[i,i]
		for(j in (i+1):p)  x[i] = x[i] - M[i,j] * x[j] / M[i,i]
	}
	
	return(matrix( x, p))
	
}



# Next, perform a Monte Carlo simulation experiment to stress test the code
# First, set the seed so that if the code fails, you can easily reproduce the
# example for which the code fails.
set.seed(1)
for(k in 1:100){
	p = sample( x=2:30, size=1)
	A = matrix( runif(p*p), ncol=p)
	A[lower.tri(A)] = 0	
	b = matrix( runif(p), ncol=1)
	x = back_substitution( A, b)
	print(k)
  if(!as.logical(prod(round(A %*% x - b, 4) == 0))){ print("code fail"); break }	
}



# Write a function to compute an eigenvector of a triangular matrix, for a given
# eigenvalue
eigenvector = function( A, lambda){
	
	p = dim(A)[1]
	
	if( p != dim(A)[2]){
		
		return("ERROR: matrix is not square")
		
	} else{ 
		
		is_upper = T
		is_lower = T
		B = t(A)
		for(i in 2:p){ 
			# Check if the matrix is upper triangular
			if( !prod(A[i,1:(i-1)] == 0) )  is_upper=F  
			# Check if the matrix is lower triangular
			if( !prod(B[i,1:(i-1)] == 0) )  is_lower=F
			if( !is_upper | !is_lower ) break 
		}

		# The matrix must be either upper or lower triangular
		if( is_upper & is_lower){

			# If the matrix is both upper and lower triangular, then it is diagonal
			v = rep( 0, p)
			v[ which(diag(A) == lambda) ] = 1
			return(v)
		
		} else if( !is_upper & !is_lower){
		
			return("ERROR: matrix is not triangular")
	
		} else{

			# If triangular, then WLOG convert it to upper triangular
			if(is_upper)  M = A  else  M = B
			
			# Homework problem to modify the back_substituion function to work here:
			#v = back_substitution( M, rep(0,p)) 
		
			return(v)
		}
	}				
}



# Test the function on a simple example
A = diag(c(1,2,3,4,5))

eigenvector( A, 1)
eigenvector( A, 2)
eigenvector( A, 3)
eigenvector( A, 4)
eigenvector( A, 5)


# Note that base R also has a built in function for determining eigenvalues and
# eigenvectors.  In the future, use this function.
A = matrix( c( 1, 2, 3,
	             0, 4, 5,
							 0, 0, 6), ncol=3, byrow=T)
eigen(A)

# There is also a built in function for solving systems of linear equations
# Ax = b
b = c( 0, 0, 0)
solve( a=A, b=b)

# If the argument for b is missing, then the matrix inverse is returned
A_inverse = solve(A)
A %*% A_inverse
A_inverse %*% A









