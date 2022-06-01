

# spline (curve lines through fixed points)
myspline <- function(x,y, sort = FALSE, circular = FALSE, ...){
    if(sort){
        y <- y[order(x)]
        x <- sort(x)
    }
    if(circular){
        x <- c(x[(length(x)-1):length(x)], x, x[1:3])
        y <- c(y[(length(y)-1):length(y)], y, y[1:3])
    }
    sx <- spline(1:length(x),x, ...)
    sy <- spline(1:length(y),y, ...)
    if(circular){
        ind <- which(sx$x >= 3 & sx$x <= length(x)-2)
        sx$y <- sx$y[ind]
        sy$y <- sy$y[ind]
    }
    return(list(x=sx$y, y=sy$y))
}

mycirularspline <- function(x,y, sort = FALSE, ...){
    if(sort){
        y <- y[order(x)]
        x <- sort(x)
    }
    x <- c(x[(length(x)-1):length(x)], x, x[1:3])
    y <- c(y[(length(y)-1):length(y)], y, y[1:3])
    sx <- spline(1:length(x),x, ...)
    sy <- spline(1:length(y),y, ...)
    
    ind <- which(sx$x >= 3 & sx$x <= length(x)-2)
    
    return(list(x = sx$y[ind], 
                y = sy$y[ind]))
    
}

x <- rnorm(4)
y <- rnorm(4)


plot(x,y, asp=1, cex=3)
polygon(myspline(x,y, n=100, circular = TRUE), col=3)
lines(myspline(x,y, n=100))




# ~ cursive?

# O O O  ------- top (4)
# 
#   O
# 
# O O O  ------- mid (2)
# 
# O   O
# 
# O O O  ------- bot (0)
# 
# O   O


refpoints <- cbind(
    c(0,1,2, 1, 0,1,2, 0,1,2, 0,1,2, 0,1,2),
    c(4,4,4, 3, 2,2,2, 1,1,1, 0,0,0, -1,-1,-1)
)

plot(refpoints, asp=1)

# letters start at (-.5,1) and end at (2.5,1)

x <- pmin(rpois(1000, 3), rpois(1000, 3))
barplot(table(x))

np <- min(rpois(2,3))
while(np==0){ np <- min(rpois(2,3)) }

pts.id <- sample(1:nrow(refpoints), np, replace = FALSE)
pts <- rbind(c(-.5,1), refpoints[pts.id,,drop=FALSE], c(2.25,.75), c(2.5,1))

crv <- myspline(x=pts[,1], y=pts[,2], n=100)

plot(refpoints,asp=1)
rect(0,0,2,4, lty=2)
points(pts, col=2)
lines(crv)


getLetter <- function(alphabet = NULL){
    if(is.null(alphabet)){
        stop('forgot alphabet')
    }else{
        letter <- alphabet[[sample(length(alphabet),1)]]
        pts.id <- letter$idx
        width <- letter$width
    }
    pts <- rbind(c(-.5,1), refpoints[pts.id,,drop=FALSE], c(width-.75,.75))
    return(list(pts = pts, width = width))
}


alphabet <- lapply(1:24, function(i){
    np <- min(rpois(2,3))
    pts.id <- sample(1:nrow(refpoints), np, replace = FALSE)
    if(!any(refpoints[pts.id,2]==0)){
        extra <- sample(which(refpoints[,2]==0), 1)
        pts.id <- c(pts.id, extra)
        if(length(pts.id) > 1){
            pts.id <- sample(pts.id)
        }
    }
    if(any(refpoints[pts.id,2]==-1) & any(refpoints[pts.id,2]==4)){
        keeplow <- which.max(refpoints[pts.id,2]==-1) < 
            which.max(refpoints[pts.id,2]==4)
        if(keeplow){
            pts.id <- pts.id[-which(refpoints[pts.id,2]==4)]
        }else{
            pts.id <- pts.id[-which(refpoints[pts.id,2]==-1)]
        }
    }
    width <- 3
    if(!any(refpoints[pts.id,1]==2)){
        width <- 2
        if(!any(refpoints[pts.id,1]==1)){
            width <- 1
        }
    }
    if(!any(refpoints[pts.id,1]==0)){
        width <- width-1
        pts.id.new <- pts.id - 1
        pts.id.new[pts.id == 4] <- 4
        pts.id <- pts.id.new
    }
    return(list(idx = pts.id, width = width))
})

ALPHABET <- lapply(1:24, function(i){
    np <- min(rpois(2,3))
    pts.id <- sample(1:nrow(refpoints), np, replace = FALSE)
    if(!any(refpoints[pts.id,2]==0)){
        extra <- sample(which(refpoints[,2]==0), 1)
        pts.id <- c(pts.id, extra)
        if(length(pts.id) > 1){
            pts.id <- sample(pts.id)
        }
    }
    if(!any(refpoints[pts.id,2]==4)){
        extra <- sample(which(refpoints[,2]==4), 1)
        pts.id <- c(pts.id, extra)
        if(length(pts.id) > 1){
            pts.id <- sample(pts.id)
        }
    }
    if(any(refpoints[pts.id,2]==-1)){
        pts.id <- pts.id[-which(refpoints[pts.id,2]==-1)]
    }
    return(list(idx = pts.id, width = 3))
})



wordlength <- rpois(1, 4)
while(wordlength < 2){ wordlength <- rpois(1,4) }

word <- lapply(1:wordlength, function(i){
    if(i == 1){
        getLetter(ALPHABET)
    }else{
        getLetter(alphabet)
    }
})
lengths <- sapply(word, function(x){x$width})

for(i in 2:wordlength){
    word[[i]]$pts[,1] <- word[[i]]$pts[,1] + sum(lengths[1:(i-1)])
}

wordpts <- do.call(rbind, lapply(word, function(x){ x$pts }))
crv <- myspline(wordpts[,1], wordpts[,2], n = 100*wordlength)

# expansion/contraction
ecFactor <- runif(1, min=.8, max=1.5)
crv$x <- crv$x * ecFactor

# italicization
itFactor <- runif(1, min=-.5, max=.5)
crv$x <- crv$x + itFactor*crv$y


plot(range(crv$x), c(-1,5), col='white', axes=FALSE, asp=1)
abline(h=c(0,2,4), lty=c(1,2,1))
lines(crv)









layout(matrix(1:60, ncol=3))
par(mar=c(0,0,0,0))
for(signum in 1:60){
    wordlength <- rpois(1, 4)
    while(wordlength < 2){ wordlength <- rpois(1,4) }
    
    word <- lapply(1:wordlength, function(i){
        if(i == 1){
            getLetter(ALPHABET)
        }else{
            getLetter(alphabet)
        }
    })
    lengths <- sapply(word, function(x){x$width})
    
    for(i in 2:wordlength){
        word[[i]]$pts[,1] <- word[[i]]$pts[,1] + sum(lengths[1:(i-1)])
    }
    
    wordpts <- do.call(rbind, lapply(word, function(x){ x$pts }))
    crv1 <- myspline(wordpts[,1], wordpts[,2], n = 100*wordlength)
    
    wordlength <- rpois(1, 4)
    while(wordlength < 2){ wordlength <- rpois(1,4) }
    
    word <- lapply(1:wordlength, function(i){
        if(i == 1){
            getLetter(ALPHABET)
        }else{
            getLetter(alphabet)
        }
    })
    lengths <- sapply(word, function(x){x$width})
    
    for(i in 2:wordlength){
        word[[i]]$pts[,1] <- word[[i]]$pts[,1] + sum(lengths[1:(i-1)])
    }
    
    wordpts <- do.call(rbind, lapply(word, function(x){ x$pts }))
    crv2 <- myspline(wordpts[,1], wordpts[,2], n = 100*wordlength)
    
    # normalization
    crv1$x <- crv1$x - min(crv1$x) + 1
    crv2$x <- crv2$x + max(crv1$x) + .75
    
    # italicization
    itFactor <- runif(1, min=-.5, max=1)
    crv1$x <- crv1$x + itFactor*crv1$y
    crv2$x <- crv2$x + itFactor*crv2$y

    # expansion/contraction
    ecFactor <- runif(1, min=1, max=1.7)
    crv1$x <- crv1$x * ecFactor
    crv2$x <- crv2$x * ecFactor
    
    # line width
    lwd <- runif(1, min=1.5, max=2.5)
    if(runif(1) < .5){
        lwd <- 1.25
    }
    
    # color
    if(lwd > 1.5){
        col <- sample(1:2,1, prob = c(.9,.1))
    }else{
        col <- sample(1:5,1)
        if(col==5){
            col <- hsv(.8,0,runif(1,min=0,max=.6))
        }
        if(col==1){
            col <- hsv(runif(1), runif(1,max=.1), runif(1,max=.1))
        }
        if(col==4){
            col <- hsv(.66, runif(1,min=.5), runif(1,min=.4,max=1))
        }
    }

    
    #plot(range(c(crv1$x,crv2$x)), c(-1,5), col='white', axes=FALSE, asp=1)
    plot(c(0,60), c(-1,5), col='white', axes=FALSE, asp=1)
    #abline(h=c(0,2,4), lty=c(1,2,1))
    lines(c(0,53),c(0,0))
    lines(crv1, lwd=lwd, col = col)
    lines(crv2, lwd=lwd, col = col)
}






# reset
layout(1)
par(mar = c(5,4,4,2)+.1)



# calligraphic script
{
    wordlength <- rpois(1, 4)
    while(wordlength < 2){ wordlength <- rpois(1,4) }
    
    word <- lapply(1:wordlength, function(i){
        if(i == 1){
            getLetter(ALPHABET)
        }else{
            getLetter(alphabet)
        }
    })
    lengths <- sapply(word, function(x){x$width})
    
    for(i in 2:wordlength){
        word[[i]]$pts[,1] <- word[[i]]$pts[,1] + sum(lengths[1:(i-1)])
    }
    
    wordpts <- do.call(rbind, lapply(word, function(x){ x$pts }))
    crv1 <- myspline(wordpts[,1], wordpts[,2], n = 100*wordlength)
    
    wordlength <- rpois(1, 4)
    while(wordlength < 2){ wordlength <- rpois(1,4) }
    
    word <- lapply(1:wordlength, function(i){
        if(i == 1){
            getLetter(ALPHABET)
        }else{
            getLetter(alphabet)
        }
    })
    lengths <- sapply(word, function(x){x$width})
    
    for(i in 2:wordlength){
        word[[i]]$pts[,1] <- word[[i]]$pts[,1] + sum(lengths[1:(i-1)])
    }
    
    wordpts <- do.call(rbind, lapply(word, function(x){ x$pts }))
    crv2 <- myspline(wordpts[,1], wordpts[,2], n = 100*wordlength)
    
    # normalization
    crv1$x <- crv1$x - min(crv1$x) + 1
    crv2$x <- crv2$x + max(crv1$x) + .75
    
    # italicization
    itFactor <- runif(1, min=-.5, max=1)
    crv1$x <- crv1$x + itFactor*crv1$y
    crv2$x <- crv2$x + itFactor*crv2$y
    
    # expansion/contraction
    ecFactor <- runif(1, min=1, max=1.7)
    crv1$x <- crv1$x * ecFactor
    crv2$x <- crv2$x * ecFactor
} # generate signature (crv1, crv2)

plot(range(c(crv1$x,crv2$x)), c(-1,5), col='white', axes=FALSE, asp=1)
abline(h=c(0,2,4), lty=c(1,2,1))
lines(crv1)
lines(crv2)

angles1 <- atan2(diff(crv1$y), diff(crv1$x))
w1 <- 2*cos(2*(angles1 + pi/4)) + 3
angles2 <- atan2(diff(crv2$y), diff(crv2$x))
w2 <- 2*cos(2*(angles2 + pi/4)) + 3

plot(range(c(crv1$x,crv2$x)), c(-1,5), col='white', axes=FALSE, asp=1)
abline(h=c(0,2,4), lty=c(1,2,1))
for(i in 2:length(crv1$x)){
    lines(crv1$x[(i-1):i], crv1$y[(i-1):i], lwd=w1[i-1])
}
for(i in 2:length(crv2$x)){
    lines(crv2$x[(i-1):i], crv2$y[(i-1):i], lwd=w2[i-1])
}



# inspirational quote

nwords <- max(rpois(2,13))

wordlengths <- replicate(nwords,{
    wordlength <- rpois(1, 4)
    while(wordlength == 0){ wordlength <- rpois(1,4) }
    if(wordlength == 1){
        wordlength <- rpois(1,4) 
        while(wordlength == 0){ wordlength <- rpois(1,4) }
    }
    wordlength
})

wordrows <- 1 + (cumsum(wordlengths) %/% 50)


plot(c(-1,51), c(0, -max(wordrows)*6-6), col='white', asp=1)
for(r in unique(wordrows)){
    for(wi in which(wordrows==r)){
        
    }
}




