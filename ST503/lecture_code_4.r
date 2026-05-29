# Visual illustration of SVD dimension reduction using a grayscale image


# Need a package to read .jpeg files in R
install.packages("jpeg")
library(jpeg)

# Load a .jpeg file
picture = readJPEG("raw_pic.jpg")
dim(picture)

# Plot the picture in R
plot( NULL, xlim=c(0,1), ylim=c(0,1), type='n')  # 'n' is for a blank plot
rasterImage( image=picture, xleft=0, ybottom=0, xright=1, ytop=1)

# Notice that the picture is distorted.  This is because we have coerced the 
# native shape of the image.  To fix this, we need to use the native ratio for 
# height to width, as measured in number of pixels, when we plot it 
height = dim(picture)[1]; height
width = dim(picture)[2]; width
ratio = height / width; ratio

# The plot is a square, so scale it to be as large as its largest dimension
plot_dim = c( 0, max( ratio, 1/ratio))
plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=picture, xleft=0, ybottom=0, xright=1, ytop=ratio)



# Most likely, the .jpeg is a 3-dimensional array.  The first two dimensions are
# the pixel coordinte location, and the third dimension is the RGB value for 
# that pixel.  To illustrate, let's change the color of a few pixels near the
# center of the image:
y = floor(height/2)
x = floor(width/2)

temp_picture = picture
for( i in (x-100):(x+100)){
	for(j in (y-100):(y+100)){
		temp_picture[ j, i,] = c( 0, 1, 0)  # c( red, green, blue)
		# note that the the first component is actually the vertical position, and 
		# the second is the horizontal location
	}
}
plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=temp_picture, xleft=0, ybottom=0, xright=1, ytop=ratio)



# A grayscale imange can be constructed from any of the RBG slices of the 
# 3-dimensional array
plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=picture[,,1], xleft=0, ybottom=0, xright=1, ytop=ratio)

plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=picture[,,2], xleft=0, ybottom=0, xright=1, ytop=ratio)

plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=picture[,,3], xleft=0, ybottom=0, xright=1, ytop=ratio)

# Let's take the green slice
grayscale_picture = picture[,,2]
dim(grayscale_picture)



# Now take the SVD of the grayscale picture 
spectral_decomp = svd(grayscale_picture)
names(spectral_decomp)
U = spectral_decomp$u; dim(U)
D = diag(spectral_decomp$d); dim(D)
V = spectral_decomp$v; dim(V)
# Reconstruct the grayscale picture from the SVD
M = U %*% D %*% t(V); M[M<0] = 0; M[M>1] = 1
plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=M, xleft=0, ybottom=0, xright=1, ytop=ratio)



# Reconstruct the grayscale picture from a dimension-reduced SVD
U = spectral_decomp$u[,1:2]; dim(U)
D = diag(spectral_decomp$d[1:2]); dim(D)
V = spectral_decomp$v[,1:2]; dim(V)
M = U %*% D %*% t(V); M[M<0] = 0; M[M>1] = 1
plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
rasterImage( image=M, xleft=0, ybottom=0, xright=1, ytop=ratio)



# Reconstruct the grayscale picture using successively more singular values
# Save as a .pdf file
pdf("dim_reduction.pdf")
for(num_sigma in 2:40){ 
	U = spectral_decomp$u[,1:num_sigma]; dim(U)
	D = diag(spectral_decomp$d[1:num_sigma]); dim(D)
	V = spectral_decomp$v[,1:num_sigma]; dim(V)
	M = U %*% D %*% t(V); M[M<0] = 0; M[M>1] = 1
	plot( NULL, xlim=plot_dim, ylim=plot_dim, type='n')  
	rasterImage( image=M, xleft=0, ybottom=0, xright=1, ytop=ratio)
	print(num_sigma)
}
dev.off()


