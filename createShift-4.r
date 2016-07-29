#! /usr/bin/Rscript

#set.seed(2001)

#install.packages("tiff")

require(tiff)
require(jpeg)

#image1 = readJPEG("/Users/xiayangzhou/Desktop/test/1.JPG")
#image2 = readJPEG("/Users/xiayangzhou/Desktop/test/2.JPG")
image3 = readJPEG("/Users/xiayangzhou/Desktop/test/3.JPG")

#image1 = (image1[,,1] + image1[,,2] + image1[,,3]) / 3
#image2 = (image2[,,1] + image2[,,2] + image2[,,3]) / 3
image3 = (image3[,,1] + image3[,,2] + image3[,,3]) / 3


size = c(400,400) 

#start1 = c(400,550) 
#step1 = c(10,0) 
#n1 = 50

#start2 = c(300,100)
#step2 = c(10,10)
#n2 = 50

start3 = c(200,900)
step3 = c(20,0)
nx = 30
ny= 15
n = nx*ny

x = vector(mode = "list", length = n)

for (j in 1:nx){

	corner = start3+c(0,30*j)
	for ( i in (ny*(j-1)+1):(ny*j)) {
		x[[i]] = image3[corner[1]:(corner[1]+size[1]),corner[2]:(corner[2]+size[2])] 
		corner = corner + step3
	}
}


writeTIFF(x, "sequence4.tiff")
