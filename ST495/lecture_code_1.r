# Comments in R begin with the hash symbol

# R can be run interactively or in batch mode
# R is open source (i.e., it is free and anyone can contribute packages)
# RStudio is an integrated development environment (IDE) for using R
# Note that R and RStudio are not the same thing

# You should become comfortable running R in the Unix command line
# - This will force you to write organized/documented code in script files,
#   rather than a series of commands in an interactive session console
# - Running code in batch mode forces you to refresh the environment every time
#   you run code.  NEVER save your workspace!
# - Running code in a cluster environment is typically done in batch mode.



# The first thing to be aware of whenever opening R is the present working
# directory ("pwd" is the Unix command):
getwd()

# If this pwd is not where your R code is located, then change it:
setwd("/Users/jwilli27/Documents/School/2022Spring/ST495")

# And verify the new pwd:
getwd()

# Practice writing an R script file with the above three commands, and run your
# script file from the command line.  If you name your script file "practice.r",
# then the command line syntax is: Rscript practice.r



# -----------------------------------------------------------------------------
# R classes to be familiar with:

# character
typeof("0"); typeof(as.character(0))

# numeric
typeof(0)

# integer
typeof(as.integer(0))

# logical
typeof(FALSE); typeof(T); typeof(as.logical(0))

# complex
typeof(0+0i)



# -----------------------------------------------------------------------------
# R data structures to be familiar with:

# vector
x = vector("numeric", length=4); x[1] = 1; x
x = numeric(4); x
x = logical(4); x
x = c( F, F, F, F); x
length(x)
c(x, T)
1:10
seq( 1, 10, by=.5)
rep( 1, 5)

# list
y = list(); y[[1]] = c( 1, 1, 1); y
y[[2]] = x; y
y[[3]] = "okay, I get it"; y
z = list( a=1:3, b=4:8); z
z["a"]
z[["a"]]
z$a

# matrix
M = matrix( NA, nrow=4, ncol=3); M
M[,1] = x; M
M[2,] = y[[1]]; M
M[4,3] = runif(1); M
dim(M)
cbind( M, x)
rbind( M, y[[1]])
diag(3); diag(rep( 1, 3))
t(x)
M[, 3, drop=F]



# -----------------------------------------------------------------------------
# R objects to be familiar with:

# infinity
Inf
-Inf
is.finite(Inf)

# Not a Number and NA
NaN
NA
is.na(NaN); is.nan(NaN)
is.na(NA); is.nan(NA)



# -----------------------------------------------------------------------------
# R control structures to be familiar with:

# if, else statements
if(0 == 1) print("condition is true") else print("condition is false")
if(0 == 0){
	print("condition is true")
} else{
	print("condition is false")
}

# for statements
for(k in 1:10) print(k)
for(k in 1:10){
 print(k)
}

# break statements
for(k in 1:10){
 print(k)
 if(k == 4) break
}

# while statements
epsilon = .001
k = 0
while(1/k > epsilon){
	k = k +1
	print(1/k)
}



# -----------------------------------------------------------------------------
# R functions
f = function(x) abs(x)
g = function(x){
	y = abs(x)
	return(c( x, y))
}
h = Vectorize(g, vectorize.args="x")
points = h(seq( -4, 4, by=.1))



# -----------------------------------------------------------------------------
# Ploting

# basic plot
plot( points[1,], points[2,])
plot( points[1,], points[2,], type="l", col="blue")
lines( points[1,], 4-points[2,], col="green")

# histogram
s = rnorm(n=10000)
hist(s, freq=T)
hist(s, freq=F)

# box plot
boxplot(s)

# save a plot as a .pdf file
pdf("example.pdf")
boxplot(s)
dev.off()



# -----------------------------------------------------------------------------
# Matrix arithmetic 
A = matrix( c( 1, 2,
	             3, 4), nrow=2, byrow=T)
B = diag(2)
a = c( 1, 1)

# matrix addition
A + B

# component-wise multiplication
A * 2
A * B

# matrix multiplication
A %*% B
A %*% a; A %*% t(t(a))
t(a) %*% A

# column-oriented matrix-vector addition
A + c( 1, 2); c( 1, 2) + A

# column-oriented matrix-vector component-wise multiplication
A * c( 1, 2); c( 1, 2) * A



# -----------------------------------------------------------------------------
# Random numbers

# setting seeds
c = sample( 1:1000, 1, replace=F)
set.seed(c[1])

# uniform distribution
runif( 0, 1, n=10)

# Bernoulli distribution
rbinom( 1, .5, n=10); runif( 0, 1, n=10) < .5

# Gaussian distribution
rnorm( 0, 1, n=10)

# exponential distribution
data = rexp( 1, n=10000)
hist(data)

# Next, re-run the above lines starting with setting the seed, and observe that
# the exact same numbers are generated.  Finally, change the seed to c[2], and
# observe that new numbers are generated



# -----------------------------------------------------------------------------
# Miscellaneous

# saving and loading data
write.table( data, file="data.csv", sep=",", row.names=F, col.names=F)
data = read.table( "data.csv", sep=",", header=F)

save( data, file="data.rda")
load("data.rda")

# source an R script file while in an interactive session
source("practice.r")

# installing a package while in an interactive session
install.packages("MASS")

# loading a package while in an interactive session
library(MASS)

# clear the workspace
ls()
rm(list = ls())
ls()

# exit an interactive session
q()


