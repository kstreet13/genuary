require(pracma)

extendPaths <- function(allpaths){
    p1 <- allpaths[[1]]
    p1 <- rbind(p1[1,] + 1000*(p1[1,] - p1[2,]), p1)
    p1 <- rbind(p1, p1[nrow(p1), ] + 1000*(p1[nrow(p1),] - p1[nrow(p1)-1,]))
    allpaths[[1]] <- p1
    if(length(allpaths) > 1){
        for(i in 2:length(allpaths)){
            pi <- allpaths[[i]]
            pi <- rbind(pi, pi[nrow(pi), ] + 1000*(pi[nrow(pi),] - pi[nrow(pi)-1,]))
            allpaths[[i]] <- pi
        }
    }
    return(allpaths)
}

plotPaths <- function(allpaths, add = FALSE, ...){
    if(!add){
        allx <- do.call(rbind, allpaths)
        plot(range(allx[,1]), range(allx[,2]), asp=1, col='white')
    }
    for(p in allpaths){
        lines(p, ...)
    }
}

lines.intersection <- function(P1, P2, P3, P4, interior.only=TRUE) {
    P1 <- as.vector(P1)
    P2 <- as.vector(P2)
    P3 <- as.vector(P3)
    P4 <- as.vector(P4)
    
    dx1 <- P1[1] - P2[1]
    dx2 <- P3[1] - P4[1]
    dy1 <- P1[2] - P2[2]
    dy2 <- P3[2] - P4[2]
    
    D <- det(rbind(c(dx1, dy1),
                   c(dx2, dy2)))
    if (D==0) {
        return(c(Inf, Inf))
    }
    D1 <- det(rbind(P1, P2))
    D2 <- det(rbind(P3, P4))
    
    X <- det(rbind(c(D1, dx1),
                   c(D2, dx2)))/D
    Y <- det(rbind(c(D1, dy1),
                   c(D2, dy2)))/D
    
    if (interior.only) {
        ## Compute the fractions of L1 and L2 at which the intersection
        ## occurs
        lambda1 <- -((X-P1[1])*dx1 + (Y-P1[2])*dy1)/(dx1^2 + dy1^2)
        lambda2 <- -((X-P3[1])*dx2 + (Y-P3[2])*dy2)/(dx2^2 + dy2^2)
        if (!((lambda1>0) & (lambda1<1) &
              (lambda2>0) & (lambda2<1))) {
            return(c(NA, NA))
        }
    }
    return(c(X, Y))
}

#c1 <- allpaths[[i]]
#c2 <- allpaths[[j]][-1, ]
curve_intersect <- function(c1, c2){
    d <- distmat(c1, c2)
    # search the 4 nearest points of possible contact
    for(wh.near in 1:4){
        magic.number <- sort(d)[wh.near]
        idx <- which(d == magic.number)
        i1 <- idx %% nrow(d)
        if(i1 == 0) i1 <- nrow(d)
        i2 <- 1 + idx %/% nrow(d)
        
        i1 <- (i1-5):(i1+5)
        i1 <- i1[i1 > 0 & i1 <= nrow(c1)]
        i2 <- (i2-5):(i2+5)
        i2 <- i2[i2 > 0 & i2 <= nrow(c2)]
        
        for(ii1 in i1[-1]){
            for(ii2 in i2[-1]){
                li <- lines.intersection(c1[ii1-1, ], c1[ii1, ],
                                         c2[ii2-1, ], c2[ii2, ])
                if(!is.na(li[1])){
                    break
                }
            }
            if(!is.na(li[1])){
                break
            }
        }
        if(!is.na(li[1])){
            break
        }
    }
    return(li)
}

# trim most recent (or any one specific) path to stop at intersections
stopatintersections <- function(allpaths, which = NA){
    if(length(allpaths) > 1){
        if(is.na(which)){
            j <- length(allpaths)
        }else{
            j <- which
        }
        for(i in (1:length(allpaths))[-j]){
            ci <- curve_intersect(allpaths[[i]][-1, ,drop=FALSE], allpaths[[j]][-1, ,drop=FALSE])
            while(!is.na(ci[1])){
                d <- distmat(allpaths[[j]], ci)
                stop <- min(which(d <= sort(d)[3]))
                if(stop <= 3){
                    allpaths <- allpaths[1:(j-1)]
                    break
                }else{
                    allpaths[[j]] <- rbind(allpaths[[j]][1:stop, ], ci)
                    ci <- curve_intersect(allpaths[[i]], allpaths[[j]][-c(1,nrow(allpaths[[j]])), ])
                }
            }
            if(length(allpaths) < j){
                break
            }
        }
    }
    return(allpaths)
}

# find breaking point among all paths
findBP <- function(allpaths){
    bpoints <- sapply(allpaths, function(path){
        vs <- diff(path)
        angles <- atan2(vs[,2], vs[,1])
        d.angles <- 1 - cos(diff(angles))
        if(length(d.angles)>0){
            # check that it hasn't already been used
            while(any(sapply(allpaths, function(p2){
                all(path[1+which.max(d.angles), ] == p2[1, ])
            }))){
                d.angles[which.max(d.angles)] <- 0
            }
            return(c(max(d.angles), 1+which.max(d.angles)))
        }else{
            return(c(0, 0))
        }
    })
    break.from <- sample(ncol(bpoints), 1, prob = bpoints[1,])
    #break.from <- which.max(bpoints[1,])
    break.at <- bpoints[2, break.from]
    return(list(break.from=break.from, break.at=break.at))
}


# extend to border
extend_to_border <- function(path, xr, yr){
    n <- nrow(path)
    v <- path[n, ] - path[n-1, ]
    v <- 4*v / sqrt(sum(v^2))
    
    toborder <- c(Inf,Inf)
    if(v[1] > 0){
        toborder[1] <- (xr[2] - path[n,1]) / v[1]
    }else{
        toborder[1] <- (path[n,1] - xr[1]) / v[1]
    }
    if(v[2] > 0){
        toborder[2] <- (yr[2] - path[n,2]) / v[2]
    }else{
        toborder[2] <- (path[n,2] - yr[1]) / v[2]
    }
    toborder <- ceiling(min(toborder))
    if(toborder > 0){
        supp <- t(t(outer(1:toborder, v)) + path[n, ])
    }else{
        supp <- matrix(nrow=0, ncol=2)
    }
    path <- rbind(path, supp)
    return(supp)
    #return(path)
}

# extend ends to maximum extent
extend_ends <- function(allpaths){
    full.length <- nrow(allpaths[[1]])
    # bounding rectangle
    allx <- do.call(rbind, allpaths)
    xr <- range(allx[,1])
    yr <- range(allx[,2])
    # first one is different b/c have to extend both ends
    ap.temp <- allpaths
    ap.temp[[1]] <- extend_to_border(ap.temp[[1]], xr,yr)
    if(nrow(ap.temp[[1]]) > 0){
        ap.temp <- stopatintersections(ap.temp, which = 1)
    }
    allpaths[[1]] <- rbind(allpaths[[1]], ap.temp[[1]])
    ap.temp[[1]] <- allpaths[[1]][nrow(allpaths[[1]]):1, ]
    ap.temp[[1]] <- extend_to_border(ap.temp[[1]], xr,yr)
    if(nrow(ap.temp[[1]]) > 0){
        ap.temp <- stopatintersections(ap.temp, which = 1)
        allpaths[[1]] <- rbind(ap.temp[[1]][nrow(ap.temp[[1]]):1,], allpaths[[1]])
    }
    
    # then the rest
    for(p.i in 2:length(allpaths)){
        ap.temp <- allpaths
        ap.temp[[p.i]] <- extend_to_border(ap.temp[[p.i]], xr,yr)
        if(nrow(ap.temp[[p.i]]) > 0){
            ap.temp <- stopatintersections(ap.temp, which = p.i)
            allpaths[[p.i]] <- rbind(allpaths[[p.i]], ap.temp[[p.i]])
        }
    }
    return(allpaths)
}

#######################################################


path <- matrix(0, nrow=1, ncol=2)
v <- rnorm(2)
v <- 10*v / sqrt(sum(v^2))
for(i in 1:100){
    v <- v + rnorm(2)
    path <- rbind(path, path[nrow(path), ] + v)
}
allpaths <- list(path)


bp <- findBP(allpaths)
vs <- diff(allpaths[[bp$break.from]])
v <- -(-vs[bp$break.at-1,] + vs[bp$break.at,])
v <- 10*v / sqrt(sum(v^2))

path <- allpaths[[bp$break.from]][bp$break.at, ,drop=FALSE]
for(i in 1:100){
    v <- v + rnorm(2)
    path <- rbind(path, path[nrow(path), ] + v)
}
allpaths[[length(allpaths)+1]] <- path
allpaths <- stopatintersections(allpaths)

plotPaths(allpaths)
for(i in 1:length(allpaths)){ lines(allpaths[[i]], col=i) }
print(length(allpaths))


shatter <- function(n = 2){
    path <- matrix(0, nrow=1, ncol=2)
    v <- rnorm(2)
    v <- 10*v / sqrt(sum(v^2))
    for(i in 1:500){
        v <- v + rnorm(2)
        path <- rbind(path, path[nrow(path), ] + v)
    }
    allpaths <- list(path)
    
    while(length(allpaths) < n){
        bp <- findBP(allpaths)
        vs <- diff(allpaths[[bp$break.from]])
        v <- -(-vs[bp$break.at-1,] + vs[bp$break.at,])
        v <- 10*v / sqrt(sum(v^2))
        
        path <- allpaths[[bp$break.from]][bp$break.at, ,drop=FALSE]
        for(i in 1:500){
            v <- v + rnorm(2)
            path <- rbind(path, path[nrow(path), ] + v)
        }
        allpaths[[length(allpaths)+1]] <- path
        allpaths <- stopatintersections(allpaths)
    }
    return(allpaths)
}



p <- shatter(50)

plotPaths(p)



# add to existing plot
u <- par()$usr
p <- shatter(20)
X <- do.call(rbind, p)
xr <- range(X[,1])
yr <- range(X[,2])
# two possible shrinkage factors, take the less extreme one
scalefac <- min(c(diff(xr) / (u[2]-u[1]), diff(yr) / (u[4]-u[3])))
shift <- c(mean(u[1:2]), mean(u[3:4])) - c(mean(xr)/scalefac, mean(yr)/scalefac)

p2 <- lapply(p, function(p){
    t(t(p / scalefac) + shift)
})

plotPaths(p2, add = TRUE, col='yellow3', lwd=3)
plotPaths(p2, add = TRUE, col='gold', lwd=2)



