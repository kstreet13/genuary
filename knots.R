
# tSNE = knots (UMAP = splats)

n <- 5000
XX <- cbind(runif(1,min=1,max=10)*cos(scaleAB(1:n, b=2*pi)), runif(1,min=1,max=10)*sin(scaleAB(1:n, b=2*pi)))

require(Rtsne)
v <- Rtsne(XX)$Y

# find misordered points
dx <- diff(v[,1]); dy <- diff(v[,2])
dang <- abs(diff(atan2(dx,dy)))
dang <- pmin(dang, abs(dang-2*pi))
if(any(dang > pi/2)){
    ind <- which(dang > pi/2)
    if(any(dang[ind+1] > pi/2)){
        stop('reversal problem')
    }
}


# find gaps in tsne knot
d <- sqrt(diff(v[,1])^2 + diff(v[,2])^2 ) # need to recalculate
break.idx <- which(d > 10*mean(d)) # mean is already pull to the right
curves <- lapply(1:length(break.idx), function(i){
    bi <- break.idx[i]
    if(i == 1){
        prev <- 0
    }else{
        prev <- break.idx[i-1]
    }
    v[(prev+1):bi,]
})
curves[[1]] <- rbind(v[(break.idx[length(break.idx)]+1):n,],
                     curves[[1]])
gaps <- lapply(1:length(curves), function(i){
    ip1 <- i+1
    if(i == length(curves)){ ip1 <- 1 }
    
    n1 <- nrow(curves[[i]])
    if(n1 >= 20){
        befor <- curves[[i]][(n1-19):n1, ]
    }else{
        befor <- curves[[i]]
    }

    n2 <- nrow(curves[[ip1]])
    if(n2 >= 20){
        after <- curves[[ip1]][1:20, ]
    }else{
        after <- curves[[ip1]]
    }
    
    t <- sqrt(diff(c(befor[,1], after[,1]))^2 + diff(c(befor[,2], after[,2]))^2)
    t <- cumsum(c(0,t))
    
    s1 <- spline(cbind(t, c(befor[,1],after[,1])), n=150)
    s2 <- spline(cbind(t, c(befor[,2],after[,2])), n=150)
    s <- list(x = s1$y, y = s2$y)
    
    # make sure spline has same directionality
    if(sum((c(s$x[1],s$y[1]) - after[20,])^2) < 
       sum((c(s$x[1],s$y[1]) - befor[1,])^2)){
        s$x <- rev(s$x)
        s$y <- rev(s$y)
    }
    
    # trim the ends
    from <- befor[nrow(befor),]
    d <- sqrt(rowSums(t(t(cbind(s$x,s$y))-from)^2))
    dd <- diff(d)
    drop1 <- 1:which.max(dd > 0)
    to <- after[1,]
    d <- sqrt(rowSums(t(t(cbind(s$x,s$y))-to)^2))
    dd <- diff(d)
    drop2 <- which.max(dd > 0):length(d)
    
    return(cbind(s$x[-c(drop1,drop2)],s$y[-c(drop1,drop2)]))
})


###
# diagnostic
plot(v, asp=1, col = colorby(1:n))
for(curve in curves){ lines(curve) }
for(gap in gaps){ lines(gap, lwd=2, col=2) }
###

X <- NULL
for(i in 1:length(curves)){
    X <- rbind(X, cbind(curves[[i]], 1)) # 1 = curve
    X <- rbind(X, cbind(gaps[[i]], 2)) # 2 = gap
}
# for consistency, remove points that are too close to both neighbors
d <- sqrt(diff(X[,1])^2 + diff(X[,2])^2 )
avd <- (d[-1] + d[-length(d)]) / 2
cut <- quantile(avd, .05)
while(any(avd < cut)){
    idx2rm <- which(avd < cut) + 1
    #print(length(idx2rm))
    if(length(idx2rm)==1){
        X <- X[-idx2rm+1, ]
    }else{
        X <- X[-sample(idx2rm, 1), ]
    }
    d <- sqrt(diff(X[,1])^2 + diff(X[,2])^2 )
    avd <- (d[-1] + d[-length(d)]) / 2
}
n <- nrow(X)


# try making full heights
h1 <- rep(1, nrow(X))
if(X[1,3]==2){ h1[1] <- -1 }
for(i in 2:nrow(X)){
    if(X[i-1,3]==1){
        if(X[i,3]==1){
            h1[i] <- h1[i-1]+1
        }else{
            h1[i] <- -1
        }
    }else{
        if(X[i,3]==1){
            h1[i] <- 1
        }else{
            h1[i] <- h1[i-1]-1
        }
    }
}
h2 <- rep(1, nrow(X))
if(X[nrow(X),3]==2){ h2[nrow(X)] <- -1 }
for(i in (nrow(X)-1):1){
    if(X[i+1,3]==1){
        if(X[i,3]==1){
            h2[i] <- h2[i+1]+1
        }else{
            h2[i] <- -1
        }
    }else{
        if(X[i,3]==1){
            h2[i] <- 1
        }else{
            h2[i] <- h2[i+1]-1
        }
    }
}
fullheights <- h1
fullheights[which(X[,3]==1)] <- pmin(h1[which(X[,3]==1)], h2[which(X[,3]==1)])
fullheights[which(X[,3]==2)] <- pmax(h1[which(X[,3]==2)], h2[which(X[,3]==2)])
fullheights <- fullheights - min(fullheights) + 1

# random heights
# fullheights <- spline(c(0,rnorm(12),0), n = nrow(X))$y
# fullheights <- round(200*fullheights)
# fullheights <- fullheights - min(fullheights) + 1

# pastel colors
h <- runif(2); h <- c(h, h[2]+rnorm(1,sd=.1))
h[h > 1] <- 1; h[h < 0] <- 0
p <- runif(2); p[1] <- p[1]/2; p <- c(p, p[2]+rnorm(1,sd=.1))
p[p > 1] <- 1; p[p < 0] <- 0
p <- 0.15 + 0.8*p
bg <- hsv(h[1], p[1], sqrt(1-p[1]^2))
col1 <- hsv(h[2], p[2], sqrt(1-p[2]^2))
col2 <- hsv(h[3], p[3], sqrt(1-p[3]^2))
plot(c(1,1,1),c(1,1,1), cex=c(1000,20,15), col=c(bg,col1,col2)) # color test

plot(range(v[,1]),range(v[,2]), col='white', asp=1)
rect(-9999,-9999,9999,9999, col=bg)
for(h in -25:max(fullheights)){
    ind1 <- which(fullheights == h)
    ind2 <- which(fullheights-25 == h)
    points(X[ind2,1], X[ind2,2], col=col2, cex=3.5)
    points(X[ind1,1], X[ind1,2], col=col1, cex=2.6)
}

##


#C
plot(range(v[,1]),range(v[,2]), col='white', asp=1)
rect(-9999,-9999,9999,9999, col=bg)
for(curve in curves){
    lines(curve, lwd=23, col=col2)
    lines(curve, lwd=18, col=col1)
}
for(gap in gaps){
    lines(gap, lwd=23, col=col1, lend=1)
    lines(gap, lwd=18, col=col2, lend=1)
}
##




# preliminary plots for tweet
# A
n <- 5000
XX <- cbind(cos(scaleAB(1:n, b=2*pi)), sin(scaleAB(1:n, b=2*pi)))
plot(range(XX[,1]),range(XX[,2]), col='white', asp=1)
rect(-9999,-9999,9999,9999, col=bg)
points(XX, col=col2, cex=3.5)
points(XX, col=col1, cex=2.6)
# B
plot(range(v[,1]),range(v[,2]), col='white', asp=1)
rect(-9999,-9999,9999,9999, col=bg)
points(v, col=col2, cex=3)
points(v, col=col1, cex=2.3)
##


png('~/Desktop/B.png', width = 800, height = 800, res = 200)
par(mar=c(0,0,0,0))

dev.off()
par(mar=c(5,4,4,2)+.1)
##


###
# splats
###
require(uwot)
n <- 5000
XX <- cbind(runif(1,min=1,max=10)*cos(scaleAB(1:n, b=2*pi)), runif(1,min=1,max=10)*sin(scaleAB(1:n, b=2*pi)))

u <- umap(XX)
##

# find smallest square containing all points

chull <- u[chull(u),]

plot(range(u[,1]) + c(-10,10), range(u[,2]) + c(-10,10), asp=1, col='white')
rect(-9999,-9999,9999,9999, col=hsv(.18, .2, .98))
{
    x <- cbind(runif(300, min=min(u[,1]), max=max(u[,1])), 
               runif(300, min=min(u[,2]), max=max(u[,2])))
    sz <- rnorm(300)^2
    points(x, col = hsv(.65, .02, .75, .7), cex=sz)
}

polygon(chull, col = 'red3', lwd=40, border = 'red3')
polygon(u, col=2)


