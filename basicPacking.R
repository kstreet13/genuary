
# packing

width <- height <- 100


# circles

circles <- NULL
avg.rad <- 7
while(is.null(circles)){
    newcirc <- c(runif(1,max=100), runif(1,max=100), rpois(1,avg.rad))
    if(newcirc[3]==0){ newcirc[3] <- 1 }
    if(newcirc[1] - newcirc[3] > 0 &
       newcirc[1] + newcirc[3] < width &
       newcirc[2] - newcirc[3] > 0 &
       newcirc[2] + newcirc[3] < height){
        circles <- matrix(newcirc, nrow=1)
    }
}

log <- c(circles, 1)

for(i in 1:10000){
    newcirc <- c(runif(1,max=100), runif(1,max=100), rpois(1,avg.rad))
    if(newcirc[3]==0){ newcirc[3] <- 1 }
    l <- c(newcirc, 0)
    
    if(newcirc[1] - newcirc[3] > 0 &
       newcirc[1] + newcirc[3] < width &
       newcirc[2] - newcirc[3] > 0 &
       newcirc[2] + newcirc[3] < height){
        sqdists <- colSums((newcirc[1:2] - t(circles[,1:2,drop=FALSE]))^2)
        if(all((newcirc[3] + circles[,3])^2 < sqdists)){
            circles <- rbind(circles, newcirc)
            l[4] <- 1
        }
    }
    log <-  rbind(log,l)
    
    if(i %% 100 == 0){
        if(avg.rad > 1){
            if(sum(log[(i-100):i,4]) <= 5){
                avg.rad <- avg.rad - 1
            }
        }
    }
}

plot(c(0,width), c(0,height), col='white', asp=1)
angleseq <- seq(0,2*pi, length.out = 100)[-1]
for(i in 1:nrow(circles)){
    polygon(circles[i,1]+cos(angleseq)*circles[i,3], 
            circles[i,2]+sin(angleseq)*circles[i,3])
}



plot(1:nrow(log), cumsum(log[,4])/1:nrow(log), type='l', ylim=0:1)
abline(h=0)
plot(log(1:nrow(log)), cumsum(log[,4])/1:nrow(log), type='l', ylim=0:1)


# squares

angleseq <- seq(pi/4, 7*pi/4, length.out = 4) + pi/4
squares <- NULL
while(is.null(squares)){
    newsq <- c(runif(1,max=100), runif(1,max=100), rpois(1,5))
    if(newsq[3]==0){ newsq[3] <- 1 }
    poly <- cbind(newsq[1]+cos(angleseq)*newsq[3], 
                  newsq[2]+sin(angleseq)*newsq[3])
    if(min(poly[,1]) > 0 &
       max(poly[,1]) < width &
       min(poly[,2]) > 0 &
       max(poly[,2]) < height){
        squares <- matrix(newsq, nrow=1)
    }
}

log <- c(squares, 1)

require(sf)
for(i in 1:10000){
    newsq <- c(runif(1,max=100), runif(1,max=100), rpois(1,avg.rad))
    if(newsq[3]==0){ newsq[3] <- 1 }
    poly <- cbind(newsq[1]+cos(angleseq)*newsq[3], 
                  newsq[2]+sin(angleseq)*newsq[3])
    
    l <- c(newsq, 0)
    
    if(min(poly[,1]) > 0 &
       max(poly[,1]) < width &
       min(poly[,2]) > 0 &
       max(poly[,2]) < height){
        sqdists <- colSums((newsq[1:2] - t(squares[,1:2,drop=FALSE]))^2)
        sqradii <- (newsq[3] + squares[,3])^2
        if(all(sqradii < sqdists)){
            squares <- rbind(squares, newsq)
            l[4] <- 1
        }else{
            idx <- which(sqradii > sqdists)
            ns <- rbind(poly, poly[1,])
            ns <- st_polygon(list(ns))
            ok <- sapply(idx, function(id){
                sq.i <- cbind(squares[id,1]+cos(angleseq)*squares[id,3], 
                              squares[id,2]+sin(angleseq)*squares[id,3])
                sq.i <- rbind(sq.i, sq.i[1,])
                sq.i <- st_polygon(list(sq.i))
                return(!st_intersects(ns, sq.i, sparse = FALSE))
            })
            if(all(ok)){
                squares <- rbind(squares, newsq)
                l[4] <- 1
            }
        }
    }
    log <-  rbind(log,l)
}

plot(c(0,width), c(0,height), col='white', asp=1)
for(i in 1:nrow(squares)){
    polygon(squares[i,1]+cos(angleseq)*squares[i,3], 
            squares[i,2]+sin(angleseq)*squares[i,3])
}



# triangles

angleseq <- seq(0, 4*pi/3, length.out = 3) + pi/2
tringls <- NULL
while(is.null(tringls)){
    newtri <- c(runif(1,max=100), runif(1,max=100), rpois(1,5))
    if(newtri[3]==0){ newtri[3] <- 1 }
    poly <- cbind(newtri[1]+cos(angleseq)*newtri[3], 
                  newtri[2]+sin(angleseq)*newtri[3])
    if(min(poly[,1]) > 0 &
       max(poly[,1]) < width &
       min(poly[,2]) > 0 &
       max(poly[,2]) < height){
        tringls <- matrix(newtri, nrow=1)
    }
}

log <- c(tringls, 1)

require(sf)
for(i in 1:10000){
    newtri <- c(runif(1,max=100), runif(1,max=100), rpois(1,avg.rad))
    if(newtri[3]==0){ newtri[3] <- 1 }
    poly <- cbind(newtri[1]+cos(angleseq)*newtri[3], 
                  newtri[2]+sin(angleseq)*newtri[3])
    
    l <- c(newtri, 0)
    
    if(min(poly[,1]) > 0 &
       max(poly[,1]) < width &
       min(poly[,2]) > 0 &
       max(poly[,2]) < height){
        sqdists <- colSums((newtri[1:2] - t(tringls[,1:2,drop=FALSE]))^2)
        sqradii <- (newtri[3] + tringls[,3])^2
        if(all(sqradii < sqdists)){
            tringls <- rbind(tringls, newtri)
            l[4] <- 1
        }else{
            idx <- which(sqradii > sqdists)
            nt <- rbind(poly, poly[1,])
            nt <- st_polygon(list(nt))
            ok <- sapply(idx, function(id){
                tri.i <- cbind(tringls[id,1]+cos(angleseq)*tringls[id,3], 
                              tringls[id,2]+sin(angleseq)*tringls[id,3])
                tri.i <- rbind(tri.i, tri.i[1,])
                tri.i <- st_polygon(list(tri.i))
                return(!st_intersects(nt, tri.i, sparse = FALSE))
            })
            if(all(ok)){
                tringls <- rbind(tringls, newtri)
                l[4] <- 1
            }
        }
    }
    log <-  rbind(log,l)
}

plot(c(0,width), c(0,height), col='white', asp=1)
for(i in 1:nrow(tringls)){
    polygon(tringls[i,1]+cos(angleseq)*tringls[i,3], 
            tringls[i,2]+sin(angleseq)*tringls[i,3])
}





# hexes

angleseq <- seq(0, 2*pi, length.out = 7)[-1]
tringls <- NULL
while(is.null(tringls)){
    newtri <- c(runif(1,max=100), runif(1,max=100), rpois(1,5))
    if(newtri[3]==0){ newtri[3] <- 1 }
    poly <- cbind(newtri[1]+cos(angleseq)*newtri[3], 
                  newtri[2]+sin(angleseq)*newtri[3])
    if(min(poly[,1]) > 0 &
       max(poly[,1]) < width &
       min(poly[,2]) > 0 &
       max(poly[,2]) < height){
        tringls <- matrix(newtri, nrow=1)
    }
}

log <- c(tringls, 1)

require(sf)
for(i in 1:10000){
    newtri <- c(runif(1,max=100), runif(1,max=100), rpois(1,avg.rad))
    if(newtri[3]==0){ newtri[3] <- 1 }
    poly <- cbind(newtri[1]+cos(angleseq)*newtri[3], 
                  newtri[2]+sin(angleseq)*newtri[3])
    
    l <- c(newtri, 0)
    
    if(min(poly[,1]) > 0 &
       max(poly[,1]) < width &
       min(poly[,2]) > 0 &
       max(poly[,2]) < height){
        sqdists <- colSums((newtri[1:2] - t(tringls[,1:2,drop=FALSE]))^2)
        sqradii <- (newtri[3] + tringls[,3])^2
        if(all(sqradii < sqdists)){
            tringls <- rbind(tringls, newtri)
            l[4] <- 1
        }else{
            idx <- which(sqradii > sqdists)
            nt <- rbind(poly, poly[1,])
            nt <- st_polygon(list(nt))
            ok <- sapply(idx, function(id){
                tri.i <- cbind(tringls[id,1]+cos(angleseq)*tringls[id,3], 
                               tringls[id,2]+sin(angleseq)*tringls[id,3])
                tri.i <- rbind(tri.i, tri.i[1,])
                tri.i <- st_polygon(list(tri.i))
                return(!st_intersects(nt, tri.i, sparse = FALSE))
            })
            if(all(ok)){
                tringls <- rbind(tringls, newtri)
                l[4] <- 1
            }
        }
    }
    log <-  rbind(log,l)
}

plot(c(0,width), c(0,height), col='white', asp=1)
for(i in 1:nrow(tringls)){
    polygon(tringls[i,1]+cos(angleseq)*tringls[i,3], 
            tringls[i,2]+sin(angleseq)*tringls[i,3])
}

