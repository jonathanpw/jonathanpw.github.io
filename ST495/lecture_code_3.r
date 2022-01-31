# Spectral theorem and singular value decomposition (SVD)

A = matrix( c( 1, 2, 3,
	             2, 4, 5,
							 3, 5, 6), ncol=3, byrow=T)

# The spectral theorem gaurantes because A is symmetric, A can be decomposed as
# Q %*% D %*% t(Q), where Q and D are constructed as follows
eigen_decomp = eigen(A); eigen_decomp
D = diag(eigen_decomp$values)
Q = eigen_decomp$vectors

# Now let's verify that we can recover A via the orthogonal decomposition
Q %*% D %*% t(Q)

# Also, observe that the columns (and rows) of Q are orthonormal 
t(Q) %*% Q
Q %*% t(Q)

# Next, verify that the columns of Q are eigenvectors of A with corresponding
# eigenvalues given by the diagonal components of D
A %*% Q[,1]; D[1,1] * Q[,1, drop=F]
A %*% Q[,2]; D[2,2] * Q[,2, drop=F]
A %*% Q[,3]; D[3,3] * Q[,3, drop=F]

# Verify that the trace is equal to the sum of the eigevalues
trace = function(M)  sum(diag(M))
trace(A)
sum(eigen_decomp$values)

# Verify that the determinant is equal to the sum of the eigevalues
det(A)
prod(eigen_decomp$values)

# Now let's take the SVD of A
svd(A)

# How does this compare to the eigen decomposition?
eigen_decom

# How about the comparison to the eigen decomposition of t(A) %*% A?
eigen(A%*%A)

# The square root of the eigenvalues of t(A) %*% A should be the same as the 
# singular values of 
sqrt(eigen(A%*%A)$values)

# For a non-square matrix X, let's see that X can be reconstructed from its SVD
X = matrix( c( 1, 2, 3, 4,
	             5, 6, 7, 8), ncol=4, byrow=T)

sv_decomp = svd(X)
U = sv_decomp$u
D = diag(sv_decomp$d)
V = sv_decomp$v

U %*% D %*% t(V); X
