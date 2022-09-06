
require(png)
pic  <- readPNG('~/Desktop/trojan.png')
#pic <- readPNG('~/Projects/slingshot/inst/slingshot_sticker.png')
#pic <- readPNG('~/Desktop/angel.png')
# channels: R, G, B, alpha
pic <- pic[,,1:3]
pic <- (pic[,,1]+pic[,,2]+pic[,,3])/3


N <- nrow(pic)*ncol(pic)
#filt <- matrix(runif(N, nrow=nrow(pic), ncol=ncol(pic))
#filt <- matrix(sample(1:25) / 25 - .5/25, nrow=5, ncol=5)
#filt <- ambient::noise_blue(c(64,64))
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
} # filt (this one looks the best)


while(nrow(filt) < nrow(pic)){
    filt <- rbind(filt, filt)
}
while(ncol(filt) < ncol(pic)){
    filt <- cbind(filt, filt)
}
filt <- filt[1:nrow(pic), 1:ncol(pic)]





dith <- pic > filt
dith[,] <- as.numeric(dith)

image(pic)
image(dith)

writePNG(dith, target = '~/Desktop/dith.png')
writePNG(pic, target = '~/Desktop/orig.png')







p1 <- pic
p2 <- pic*(7/8) + updn*(1/8)
p3 <- pic*(6/8) + updn*(2/8)
p4 <- pic*(5/8) + updn*(3/8)
p5 <- pic*(4/8) + updn*(4/8)
p6 <- pic*(3/8) + updn*(5/8)
p7 <- pic*(2/8) + updn*(6/8)
p8 <- pic*(1/8) + updn*(7/8)
p9 <- updn

l <- lapply(1:3, function(i){
    rbind(cbind(p1[,,i],p2[,,i],p3[,,i]),
          cbind(p4[,,i],p5[,,i],p6[,,i]),
          cbind(p7[,,i],p8[,,i],p9[,,i]))
})

out <- array(1, dim = c(nrow(pic)*3,ncol(pic)*3,4))
out[,,1] <- l[[1]]
out[,,2] <- l[[2]]
out[,,3] <- l[[3]]

writePNG(updn, target = '~/Desktop/pic.png')
















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
    x[-ind] <- 1.055*x^(1/2.4)-.055
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










