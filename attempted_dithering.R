


require(png)
# black and white picture
pic <- readPNG('~/Desktop/pic.png')
pic <- readPNG('~/Projects/slingshot/inst/slingshot_sticker.png')
# channels: R, G, B, alpha
pic <- (pic[,,1]+pic[,,2]+pic[,,3])/3
pic <- mazing::condense_matrix(pic, fun=mean)


pic <- matrix(seq(0,1,length.out=100), nrow=100,ncol=100)
pic <- mazing::expand_matrix(pic)
pic <- mazing::expand_matrix(pic)


{
    N <- nrow(pic)*ncol(pic)
    x <- runif(N)
    y <- runif(N)
    bumpup <- which(abs(x-.5) > abs(y-.5) & x < .5)
    bumpdown <- which(abs(x-.5) > abs(y-.5) & x > .5)
    x[bumpup] <- x[bumpup] + .5
    x[bumpdown] <- x[bumpdown] - .5
    filt <- matrix(x, nrow=nrow(pic), ncol=ncol(pic))
    rm(N,x,y,bumpup,bumpdown)
} # filt

dith <- pic > filt
dith[,] <- as.numeric(dith)

image(pic)
image(dith)

writePNG(dith, target = '~/Desktop/dith.png')
















shrinkBy2 <- function (m, fun = median) {
    m2 <- matrix(NA, nrow = ceiling(nrow(m)/2), ncol = ceiling(ncol(m)/2))
    for (i in 1:(nrow(m2))) {
        for (j in 1:(ncol(m2))) {
            i.2 <- (2 * i - 1):(2 * i)
            j.2 <- (2 * j - 1):(2 * j)
            if (i == nrow(m2)) {
                i.2 <- (nrow(m) - 1):nrow(m)
            }
            if (j == ncol(m2)) {
                j.2 <- (ncol(m) - 1):ncol(m)
            }
            m2[i, j] <- fun(m[i.2, j.2])
        }
    }
    return(m2)
}

s2lin <- function(x){
    ind <- which(x<=.04045)
    x[ind] <- x[ind]/12.92
    x[-ind] <- ((x[-ind]+.055) / 1.055)^2.4
    return(x)
}

lin2s <- function(x){
    ind <- which(x<=.0031308)
    x[ind] <- x[ind] * 12.92
    x[-ind] <- 1.055*x[-ind]^(1/2.4)-.055
    return(x)
}

require(png)
pic <- readPNG('~/Desktop/angel.png')
pic <- pic[,,1:3]
newpic <- pic[1:ceiling(nrow(pic)/2), 1:ceiling(ncol(pic)/2),]
for(i in 1:3){
    newpic[,,i] <- s2lin(shrinkBy2(pic[,,i]))
}
pic <- newpic

filt <- ambient::noise_blue(c(64,64))
while(nrow(filt) < nrow(pic)){
    filt <- rbind(filt, filt)
}
while(ncol(filt) < ncol(pic)){
    filt <- cbind(filt, filt)
}
filt <- filt[1:nrow(pic), 1:ncol(pic)]

updn <- pic
for(i in 1:3){
    updn[,,i] <- pic[,,i] > filt
}

writePNG(updn, target = '~/Desktop/pic2.png')










###########

pic <- readPNG('~/Desktop/angel.png')
pic[,,1] <- pic[,,2] <- pic[,,3] <- 0.299*pic[,,1] + 0.587*pic[,,2] + 0.114*pic[,,3]
pic <- pic[,,1:3]

pic[,,1] <- pic[,,2] <- pic[,,3] <- s2lin(pic[,,1])

filt <- ambient::noise_blue(c(64,64))
while(nrow(filt) < nrow(pic)){
    filt <- rbind(filt, filt)
}
while(ncol(filt) < ncol(pic)){
    filt <- cbind(filt, filt)
}
filt <- filt[1:nrow(pic), 1:ncol(pic)]

updn <- pic
updn[,,1] <- updn[,,2] <- updn[,,3] <- pic[,,1] > filt


writePNG(updn, target = '~/Desktop/picBWbig.png')










