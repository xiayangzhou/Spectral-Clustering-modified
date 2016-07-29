require(tiff)
require(jpeg)
step=100

image1 = readJPEG("/Users/xiayangzhou/Desktop/test/3.JPG")


image1 = (image1[,,1] + image1[,,2] + image1[,,3]) / 3
image2 = array(0,dim=c(dim(image1)[1],dim(image1)[2],3))

dim(image2)
image2[,,1]=image1
image2[,,2]=image1

image2[,,3]=image1*0.3


writeJPEG(image2, "ss.jpg")
