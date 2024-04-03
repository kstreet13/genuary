
# add texture?

require(png)
pic  <- readPNG('~/Desktop/temp/1.png')

# channels: R, G, B, alpha
#pic <- pic[,,1:3]
#pic <- (pic[,,1]+pic[,,2]+pic[,,3])/3

hsvmat <- rgb2hsv(rbind(as.numeric(pic[,,1]),
                        as.numeric(pic[,,2]),
                        as.numeric(pic[,,3])),
                  maxColorValue = 1)
picHSV <- pic
picHSV[,,1] <- hsvmat[1,]
picHSV[,,2] <- hsvmat[2,]
picHSV[,,3] <- hsvmat[3,]
rm(hsvmat)


N <- nrow(pic)*ncol(pic)
x <- runif(N)
y <- runif(N)
bumpup <- which(abs(x-.5) > abs(y-.5) & x < .5)
bumpdown <- which(abs(x-.5) > abs(y-.5) & x > .5)
x[bumpup] <- x[bumpup] + .5
x[bumpdown] <- x[bumpdown] - .5
filt <- matrix(x, nrow=nrow(pic), ncol=ncol(pic))
filt <- scaleAB(filt, a=-.05, b=.05)
rm(N,x,y,bumpup,bumpdown)

# fudge colors
pic0 <- picHSV

cut <- quantile(filt, .9)
r <- which(filt < -cut) %% nrow(filt)
r[r == 0] <- nrow(filt)
c <- which(filt < -cut) %/% nrow(filt) + 1
# take from previous column
for(p.i in 1:length(r)){
    if(c[p.i] > 1){
        picHSV[r[p.i],c[p.i],1:2] <- pic0[r[p.i],c[p.i-1],1:2]
    }
}
r <- which(filt > cut) %% nrow(filt)
r[r == 0] <- nrow(filt)
c <- which(filt > cut) %/% nrow(filt) + 1
# take from next column
for(p.i in 1:length(r)){
    if(c[p.i] < ncol(filt)){
        picHSV[r[p.i],c[p.i],1:2] <- pic0[r[p.i],c[p.i+1],1:2]
    }
}



picHSV[,,3] <- picHSV[,,3] + filt
picHSV[,,3][picHSV[,,3] < 0] <- 0
picHSV[,,3][picHSV[,,3] > 1] <- 1



rgbmat <- col2rgb(hsv(h = as.numeric(picHSV[,,1]),
                      s = as.numeric(picHSV[,,2]),
                      v = as.numeric(picHSV[,,3]))) / 255
pic2 <- pic
pic2[,,1] <- rgbmat[1,]
pic2[,,2] <- rgbmat[2,]
pic2[,,3] <- rgbmat[3,]
rm(rgbmat)


writePNG(pic2, target = '~/Desktop/1.png')







