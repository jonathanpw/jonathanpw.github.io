

args = commandArgs(TRUE)

cat('You passed me the integer, ', args[1])

# Let's generate some data!
data = runif(100)

# Let's save the data!
save( data, file=paste0('data_set_',args[1],'.rda'))