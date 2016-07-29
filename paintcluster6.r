#! /usr/bin/Rscript

#set.seed(2001)

#install.packages("tiff")
#install.packages("jpeg")

require(tiff)
require(jpeg)



require(tiff)
require(jpeg)
step_vertical=2
step_horizontal=1
small_step=1

image1 = readJPEG("/Users/xiayangzhou/Desktop/test/4.JPG")


image1 = (image1[,,1] + image1[,,2] + image1[,,3]) / 3

rowsnb=NROW(image1)
colsnb=NCOL(image1)


size = c(step_vertical,step_horizontal) 

start = c(0,0) 



n1=floor(rowsnb/step_vertical)
n2=floor((colsnb-step_horizontal) /small_step)+1



n = n1*n2

x = vector(mode = "list", length = n)
corner = start 
image_number = 1
for (i in 1:n1) { 
	for ( j in 1:n2) {
		print (corner)
		x[[image_number]] = as.matrix(image1[(corner[1]+1):(corner[1]+size[1]),(corner[2]+1):(corner[2]+size[2])] )
		image_number = image_number + 1
		corner[2] = corner[2] + small_step
	}
corner[1] = corner[1] + step; corner[2] = start[2] 
}

writeTIFF(x, "pavement.tiff")
####################################
# begin to cluster
#################################


dimA = 10 
ratio = 1 
quantile = 0.02 
traceValue = 0.02 
finalTraceValue = 0.05
iterNewton = 10 

eigenratio = 0.001
class_number = 20
cat("maximum assumed number of classes :", class_number, "\n")
class_number = class_number + 1
gamma = 0.001 
max_classes = 40
threshold = 0.1

trace.newton <- function(d2, k, iter) {
	beta = 0
	for ( i in 1:iter) { 
		beta = beta + (sum(exp(- beta * d2)) - k)/sum(d2 * exp( - beta * d2)) 
	}
	return (beta / 2)
}

x = readTIFF("pavement.tiff", all = TRUE)
n = length(x)
cat("n = ", n, "\n")

A = matrix(0,n,n)
for (i in 1:(n-1)) {
	for (j in (i+1):n) {
		A[i,j] = mean(x[[i]] * x[[j]])
		A[j,i] = A[i,j]
	}
	A[i,i] = mean(x[[i]]*x[[i]])
} 
A[n,n] = mean(x[[n]]*x[[n]])

ones = matrix(1,n,n)
A = A - (ones %*% A + A %*% ones) / n + mean(A) 
rm(ones)

sA = svd(A, nv = 0)
firstrep = sA$u[,1:4] %*% diag(sqrt(sA$d[1:4]))
rm(sA, A)

A = matrix(0,n,n)

for (i in 1:(n-1)) {
	for (j in (i+1):n) {
		A[i,j] = mean((x[[i]] - x[[j]])^2)
		A[j,i] = A[i,j]
	}
} 

rm(x) 

beta1 = trace.newton(A, traceValue * n * n + n , iterNewton)
A = exp ( - beta1 * A )
cat("trace of A^2 = ", mean(A*A) - 1/n, "\n")
A = 1 - A

beta2 = trace.newton(A, traceValue * n * n + n , iterNewton)
A = exp ( - beta2 * A )
cat("trace of A^2 = ", mean(A*A) - 1/n, "\n")

sA = svd(A, nv=0) 
secondrep = sA$u[,2:dimA] %*% diag(sqrt(sA$d[2:dimA]))
secondrep = diag(1/sqrt(rowSums(secondrep^2))) %*% secondrep
rm(sA)


pcolor = vector(length = n)
pcolor[1:n] = "blue"
#pcolor[1:(n/2)] = "blue"
#pcolor[(n/2+1):n] = "red"
#print(pcolor)

A = 1 - secondrep %*% t(secondrep)
#print (A[1:15,1:30])

beta = trace.newton(A, n * n * finalTraceValue + n , iterNewton)
A = exp ( - beta * A )
cat("trace of A^2 = ", mean(A*A) - 1/n, "\n")
#print (A[1:15,1:30])


D = diag(1 / sqrt(pmax(rowSums(A), n*gamma)))
A = D %*% A %*% D
#print (A[1:15,1:30])
rm(D)

s = svd(A, nv=0)

#print (s$u)

iter = ceiling(log(1/eigenratio)/log(s$d[2]/s$d[class_number]))

print (iter)

A = s$u %*% diag(s$d^iter) %*% t(s$u)
#print (A[1:10,3:30])

# project on the sphere
gamma = 0.0000000001
N = diag(1 / sqrt(pmax(diag(A),gamma)))
A = N %*% A %*% N
ones = matrix(1,n,n)
A = A - (ones %*% A + A %*% ones)/n +  mean(A)
rm(ones)
N = diag(1 / sqrt(pmax(diag(A), gamma)))
A = N %*% A %*% N
rm(N)

B = A
C = A
R = C[,1]
i = (B[,1] < threshold)
#print (B[,1])
#print (i)
if (max(i) > 0) { 
C = C[,i]
B = B[i,i]
for (j in 1:max_classes) { 
	if ( max(i) == 0 ) break
	if (sum(i) == 1) { 
		dim(C) = c(n,1)
		dim(B) = c(1,1)
	}
	R = cbind(R, C[,1])
	i = (B[,1] < threshold)
	C = C[,i]
	B = B[i,i]
}
}
rm(i,C,B)

if (is.null(dim(R)) ) { 
	dim(R) = c(n,1)	
} 

cat("dim(R) = \n")
print(dim(R))
color_number = dim(R)[2]
cat("color_number = ", color_number, "\n")
color_value = rainbow(color_number)
color_classif = rep(color_value[1], n)
for (i in 1:color_number) { 
	color_classif[R[,i] >= threshold] =color_value[i]} 
rm( R)

print(sprintf("number of colors = %i", color_number))

#######################################
#begin to paint
######################################


image2 = array(0,dim=c(dim(image1)[1],dim(image1)[2],3))
#image2 = array(0,dim=c(dim(image1),3))
image2[,,1] = image2[,,2] = image2[,,3] = image1

color_rgb=as.matrix(col2rgb(color_classif))/255.0
label_ul = floor(size/2) - small_step/2.0  
label_lr = floor(size/2) + small_step/2.0
image_number = 1 
corner = start 
for (i in 1:n1) {

  for ( j in 1:n2) {
    #print (corner)
    #print(color_classif[image_number])
    image2[corner[1]+(label_ul[1]:label_lr[1]),corner[2]+(label_ul[2]:label_lr[2]),1] =color_rgb[1,image_number]*image1[corner[1]+(label_ul[1]:label_lr[1]),corner[2]+(label_ul[2]:label_lr[2])]    
    image2[corner[1]+(label_ul[1]:label_lr[1]),corner[2]+(label_ul[2]:label_lr[2]),2] =color_rgb[2,image_number]*image1[corner[1]+(label_ul[1]:label_lr[1]),corner[2]+(label_ul[2]:label_lr[2])]
    image2[corner[1]+(label_ul[1]:label_lr[1]),corner[2]+(label_ul[2]:label_lr[2]),3] =color_rgb[3,image_number]*image1[corner[1]+(label_ul[1]:label_lr[1]),corner[2]+(label_ul[2]:label_lr[2])]    
    image_number = image_number + 1
    corner[2] = corner[2] + small_step
  }
corner[1] = corner[1] + step; corner[2] = start[2] 
}

#for (k in 1:n){
#	colorN=color_classif[k]
#	l=trunc((k-1)/n2)
#	c=k-l*n2
#	corner=c(l*step,(c-1)*step)
#	print (l*step+size[1])
#	print ((c-1)*step +size[2])
#	image2[(l*step+1):(l*step+size[1]),((c-1)*step +1):((c-1)*step +size[2]),1]= image1[(l*step+1):(l*step+size[1]),((c-1)*step +1):((c-1)*step +size[2])]*(1-colorN/#color_number)*1.5
#	image2[(l*step+1):(l*step+size[1]),((c-1)*step +1):((c-1)*step +size[2]),2]= image1[(l*step+1):(l*step+size[1]),((c-1)*step +1):((c-1)*step +size[2])]*(colorN/#color_number)
#	image2[(l*step+1):(l*step+size[1]),((c-1)*step +1):((c-1)*step +size[2]),3]= image1[(l*step+1):(l*step+size[1]),((c-1)*step +1):((c-1)*step +size[2])]*sqrt(colorN/#color_number)*(colorN%%3==0)

	
#}

writeJPEG(image2, "ss.jpg")

#displays data points in file colors.pdf 
pdf("colors.pdf")
plot(1:n, col = color_classif, xlab=sprintf("Number of classes = %d, \n trace = %.4f, iter = %f",color_number, finalTraceValue, iter)
)
#, xlab=sprintf("Number of classes = %d, \n trace = %.4f, iter = %d",color_number, finalTraceValue, iter)
grid(col="blue")
dev.off()
