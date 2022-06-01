

# few big circles with single shape packed inside
# mix of shapes outside the circles


# 0 = circle, 3 = triangle, 4 = square, 6 = hex
makePoly <- function(x, y, r, type = 0, offset=0){
    stopifnot(type %in% c(0,3,4,6))
    if(type == 0){ type <- 100 }
    angleseq <- seq(0, 2*pi, length.out = type+1) + offset
    poly <- cbind(x+cos(angleseq)*r, 
                  y+sin(angleseq)*r)
    return(poly)
}





width <- 80
height <- 120

colorRange1 <- c(.05,.13) # orange ~ (.05 - .13)
colorwidth <- colorRange1[2] - colorRange1[1]

if(runif(1)>.5){
    # contrasting (.49 - .88)
    st <- runif(1, min=.49, max=.8)
    colorRange2 <- c(st, st + colorwidth)
}else{
    # similar (.93 - 1.2) need to %% later
    st <- runif(1, min=.93, max=1.2)
    while(abs(st - 1.05) < .03){
        st <- runif(1, min=.93, max=1.2)
    }
    colorRange2 <- c(st, st + colorwidth)
}



bigcircles <- NULL
nbig <- 1 + rpois(1, 1.4)
if(nbig == 2){ nbig <- 1 + rpois(1, 1.4) }
while(is.null(bigcircles)){
    newcirc <- c(runif(1,max=100), runif(1,max=100), rpois(1,30))
    if(newcirc[3]==0){ newcirc[3] <- 1 }
    if(newcirc[1] - newcirc[3] > 0 &
       newcirc[1] + newcirc[3] < width &
       newcirc[2] - newcirc[3] > 0 &
       newcirc[2] + newcirc[3] < height){
        bigcircles <- matrix(newcirc, nrow=1)
    }
}
while(nrow(bigcircles) < nbig){
    newcirc <- c(runif(1,max=100), runif(1,max=100), rpois(1,30))
    if(newcirc[3]==0){ newcirc[3] <- 1 }
    if(newcirc[1] - newcirc[3] > 0 &
       newcirc[1] + newcirc[3] < width &
       newcirc[2] - newcirc[3] > 0 &
       newcirc[2] + newcirc[3] < height){
        sqdists <- colSums((newcirc[1:2] - t(bigcircles[,1:2,drop=FALSE]))^2)
        if(all((newcirc[3] + bigcircles[,3])^2 < sqdists)){
            bigcircles <- rbind(bigcircles, newcirc)
        }
    }
}
bigcircles <- cbind(bigcircles, type = sample(c(3,4,6), nrow(bigcircles), replace = TRUE))
bigcircles <- cbind(bigcircles, offset = -pi*bigcircles[,2]/200)
bigcircles <- cbind(bigcircles, color = colorwidth*bigcircles[,2]/100+colorRange1[1])
bigcircles[,6] <- bigcircles[,6] %% 1

# plot(c(0,width), c(0,height), col='white', asp=1)
# for(i in 1:nrow(bigcircles)){
#     polygon(makePoly(bigcircles[i,1],bigcircles[i,2],bigcircles[i,3]))
# }


shapes <- NULL
numshapes <- 1
avg.rad <- 10
# first shape
while(is.null(shapes)){
    newshp <- c(runif(1,max=width), runif(1,max=height), rpois(1,avg.rad))
    if(newshp[3]==0){ newshp[3] <- 1 }
    bigCdists <- colSums((newshp[1:2] - t(bigcircles[,1:2,drop=FALSE]))^2)
    if(any(bigCdists < bigcircles[,3]^2)){
        # if it's in a big circle, it's that circle's shape, needs to not touch the circle
        wh <- which(bigCdists < bigcircles[,3]^2)
        newshp <- c(newshp, bigcircles[wh,4:6])
        poly <- makePoly(newshp[1], newshp[2], newshp[3], newshp[4], newshp[5])
        if(all(colSums((bigcircles[wh,1:2] - t(poly))^2) < bigcircles[wh,3]^2)){
            shapes <- matrix(newshp, nrow=1)
        }
    }else{
        # else it's a ~random shape, needs to not touch any circle
        if(nbig==1){
            ty <- bigcircles[,4]
        }else{
            ty <- sample(bigcircles[,4], 1, prob = 1/bigCdists)
        }
        #if(runif(1) < .5){ ty <- 0 }
        newshp <- c(newshp, ty, pi*newshp[2]/200, colorwidth*newshp[2]/100+colorRange2[1]) # offset and color tied to y coord
        poly <- makePoly(newshp[1], newshp[2], newshp[3], newshp[4], newshp[5])
        if(min(poly[,1]) > 0 &
           max(poly[,1]) < width &
           min(poly[,2]) > 0 &
           max(poly[,2]) < height){
            ok <- sapply(1:nrow(bigcircles), function(id){
                bc.i <- makePoly(bigcircles[id,1],bigcircles[id,2],bigcircles[id,3])
                bc.i[nrow(bc.i),] <- bc.i[1,]
                bc.i <- st_polygon(list(bc.i))
                poly[nrow(poly),] <- poly[1,]
                ns <- st_polygon(list(poly))
                return(!st_intersects(ns, bc.i, sparse = FALSE))
            })
            if(all(ok)){
                shapes <- matrix(newshp, nrow=1)
            }
        }
    }
}

# all subsequent shapes
for(i in 1:50000){
    newshp <- c(runif(1,max=width), runif(1,max=height), rpois(1,avg.rad))
    if(newshp[3]==0){ newshp[3] <- 1 }
    bigCdists <- colSums((newshp[1:2] - t(bigcircles[,1:2,drop=FALSE]))^2)
    check1 <- FALSE
    if(any(bigCdists < bigcircles[,3]^2)){
        # if it's in a big circle, it's that circle's shape, needs to not touch the circle
        wh <- which(bigCdists < bigcircles[,3]^2)
        newshp <- c(newshp, bigcircles[wh,4:6])
        #if(newshp[4] == 3){ if(runif(1)<.5) {newshp[5] <- newshp[5]+pi/3} } # triangles can be flipped
        poly <- makePoly(newshp[1], newshp[2], newshp[3], newshp[4], newshp[5])
        if(all(colSums((bigcircles[wh,1:2] - t(poly))^2) < bigcircles[wh,3]^2)){
            check1 <- TRUE
        }
    }else{
        # else it's a ~random shape, needs to not touch any big circle
        if(nbig==1){
            ty <- bigcircles[,4]
        }else{
            ty <- sample(bigcircles[,4], 1, prob = 1/bigCdists)
        }
        #if(runif(1) < .5){ ty <- 0 }
        newshp <- c(newshp, ty, pi*newshp[2]/200, colorwidth*newshp[2]/100+colorRange2[1]) # offset tied to y coord
        #if(newshp[4] == 3){ if(runif(1)<.5) {newshp[5] <- newshp[5]+pi/3} } # triangles can be flipped
        poly <- makePoly(newshp[1], newshp[2], newshp[3], newshp[4], newshp[5])
        if(min(poly[,1]) > 0 &
           max(poly[,1]) < width &
           min(poly[,2]) > 0 &
           max(poly[,2]) < height){
            ok1 <- sapply(1:nrow(bigcircles), function(id){
                bc.i <- makePoly(bigcircles[id,1],bigcircles[id,2],bigcircles[id,3])
                bc.i[nrow(bc.i),] <- bc.i[1,]
                bc.i <- st_polygon(list(bc.i))
                poly[nrow(poly),] <- poly[1,]
                ns <- st_polygon(list(poly))
                return(!st_intersects(ns, bc.i, sparse = FALSE))
            })
            if(all(ok1)){
                check1 <- TRUE
            }
        }
    }
    
    if(check1){
        # check doesn't touch other shapes
        sqdists <- colSums((newshp[1:2] - t(shapes[,1:2,drop=FALSE]))^2)
        sqradii <- (newshp[3] + shapes[,3])^2
        if(all(sqradii < sqdists)){
            shapes <- rbind(shapes, newshp)
        }else{
            idx <- which(sqradii > sqdists)
            poly[nrow(poly),] <- poly[1,]
            ns <- st_polygon(list(poly))
            ok <- sapply(idx, function(id){
                sh.i <- makePoly(shapes[id,1],shapes[id,2],shapes[id,3],shapes[id,4],shapes[id,5])
                sh.i[nrow(sh.i),] <- sh.i[1,]
                sh.i <- st_polygon(list(sh.i))
                return(!st_intersects(ns, sh.i, sparse = FALSE))
            })
            if(all(ok)){
                shapes <- rbind(shapes, newshp)
            }
        }
    }
    
    if(i %% 100 == 0){
        # check if we're still accepting things, if not turn down avg.rad
        if(nrow(shapes) - numshapes < 2){
            if(avg.rad > 1){
                avg.rad <- avg.rad - 1
                print(c(i, avg.rad))
            }
        }
        numshapes <- nrow(shapes)
    }
}
shapes[,6] <- shapes[,6] %% 1

# for(i in 1:nrow(shapes)){
#     polygon(makePoly(shapes[i,1],shapes[i,2],shapes[i,3],shapes[i,4],shapes[i,5]))
# }

iii='new'
png(paste0('~/Desktop/',iii,'.png'), width = 800, height = 1200, res = 150)
old.par <- par(mar = c(0,0,0,0))
plot(c(0,width), c(0,height), col='white', asp=1)
rect(-9999,-9999,9999,9999, col=hsv(.2, .03, .95))
# for(i in 1:nrow(bigcircles)){
#     polygon(makePoly(bigcircles[i,1],bigcircles[i,2],bigcircles[i,3]))
# }
for(i in 1:nrow(shapes)){ #nrow(shapes)
    poly <- makePoly(shapes[i,1],shapes[i,2],shapes[i,3],shapes[i,4],shapes[i,5])
    #wiggle <- max(.01, .02 - shapes[i,4] / 500)
    polygon(poly, border = NA,
            col = hsv(shapes[i,6],
                      runif(1, min=.5, max=1), 
                      runif(1, min=.90, max=.95)))
}
dev.off()




