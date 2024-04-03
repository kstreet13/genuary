require(pracma) # Norm and matrix dist
require(matrixStats)
require(deldir) # deldir
require(sp) # point.in.polygon
# make nicely separated, random points in space
repelled_points <- function(n, multiplier = 15, rw = c(0,1,0,1)){
    x0 <- matrix(runif(2*multiplier*n), ncol=2)
    x0[,1] <- (x0[,1] + rw[1])*(rw[2]-rw[1])
    x0[,2] <- (x0[,2] + rw[3])*(rw[4]-rw[3])
    km <- kmeans(x0, centers=n)
    return(km$centers)
}

# nicely separated points inside a polygon
repelled_in_shape <- function(n, poly, multiplier = 15){
    x0 <- cbind(runif(multiplier*n, min=min(poly[,1]), max=max(poly[,1])), 
                runif(multiplier*n, min=min(poly[,2]), max=max(poly[,2])))
    x0 <- x0[as.logical(point.in.polygon(x0[,1],x0[,2], poly[,1],poly[,2])), ]
    while(nrow(x0) < multiplier*n){
        n2 <- 2 * (multiplier*n - nrow(x0))
        extra <- cbind(runif(n2, min=min(poly[,1]), max=max(poly[,1])), 
                       runif(n2, min=min(poly[,2]), max=max(poly[,2])))
        extra <- extra[as.logical(point.in.polygon(extra[,1],extra[,2], poly[,1],poly[,2])), ]
        x0 <- rbind(x0, extra)
    }
    x0 <- x0[1:(multiplier*n), ]
    km <- kmeans(x0, centers=n)
    return(km$centers)
}


# pick out the polygon enclosing point i
border <- function(i, dd){
    pt <- c(dd$summary$x[i], dd$summary$y[i])
    edges <- dd$dirsgs[dd$dirsgs$ind1==i | dd$dirsgs$ind2==i, ]
    vertices <- unique(rbind(cbind(edges$x1, edges$y1), cbind(edges$x2, edges$y2)))
    # check for corners
    thirds <- c(edges$thirdv1,edges$thirdv2)
    thirds <- sort(thirds[thirds < 0])
    if(length(thirds) == 2){
        if(all(thirds == -2:-1)){
            vertices <- rbind(vertices, dd$rw[c(1,3)])
        }
        if(all(thirds == -3:-2)){
            vertices <- rbind(vertices, dd$rw[c(1,4)])
        }
        if(all(thirds == -4:-3)){
            vertices <- rbind(vertices, dd$rw[c(2,4)])
        }
        if(all(thirds == c(-4,-1))){
            vertices <- rbind(vertices, dd$rw[c(2,3)])
        }
    }
    vector <- t(t(vertices)-pt)
    angle <- apply(vector,1,function(x){
        atan2(x[2],x[1])
    })
    return(vertices[order(angle), ])
}


lines.intersection <- function(P1, P2, P3, P4, interior.only=FALSE) {
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


shrink.poly <- function(poly, r){
    # for each side, determine how much we will "cut off" in order to make smoothed.sides
    # if cutoff exceeds side length, skip it
    angles <- sapply(1:nrow(poly),function(i){
        ip1 <- i+1
        if(ip1 > nrow(poly)) ip1 <- 1
        im1 <- i-1
        if(im1 == 0) im1 <- nrow(poly)
        # arccos(x dot y / mag(x)mag(y))
        v1 <- poly[im1,] - poly[i,]
        v2 <- poly[ip1,] - poly[i,]
        return(acos(sum(v1*v2) / sqrt(sum(v1^2)*sum(v2^2))))
    })
    bites <- sapply(1:nrow(poly), function(i){
        r / tan(angles[i]/2)
    })
    keep <-sapply(1:nrow(poly),function(i){
        ip1 <- i+1
        if(ip1 > nrow(poly)) ip1 <- 1
        
        side.length <- pracma::Norm(poly[ip1,] - poly[i,])
        return(side.length > bites[i] + bites[ip1])
    })
    # recalculate angles with side(s) deleted
    while(any(!keep)){
        del <- which.max(!keep) # delete side that comes after this vertex
        dp1 <- del+1
        if(dp1 > nrow(poly)) dp1 <- 1
        dp2 <- dp1+1
        if(dp2 > nrow(poly)) dp2 <- 1
        dm1 <- del-1
        if(dm1 == 0) dm1 <- nrow(poly)
        
        newvert <- lines.intersection(poly[dm1,], poly[del,],
                                      poly[dp1,], poly[dp2,],
                                      interior.only = FALSE)
        poly[del,] <- newvert
        poly[dp1,] <- newvert
        poly <- unique(poly)
        
        # recalculate keep
        angles <- sapply(1:nrow(poly),function(i){
            ip1 <- i+1
            if(ip1 > nrow(poly)) ip1 <- 1
            im1 <- i-1
            if(im1 == 0) im1 <- nrow(poly)
            # arccos(x dot y / mag(x)mag(y))
            v1 <- poly[im1,] - poly[i,]
            v2 <- poly[ip1,] - poly[i,]
            return(acos(sum(v1*v2) / sqrt(sum(v1^2)*sum(v2^2))))
        })
        bites <- sapply(1:nrow(poly), function(i){
            r / tan(angles[i]/2)
        })
        keep <-sapply(1:nrow(poly),function(i){
            ip1 <- i+1
            if(ip1 > nrow(poly)) ip1 <- 1
            
            side.length <- pracma::Norm(poly[ip1,] - poly[i,])
            return(side.length > bites[i] + bites[ip1])
        })
    }
    
    
    
    # for each side in poly, move it in (toward center) by a distance r
    inside.sides <- lapply(1:nrow(poly),function(i){
        ip1 <- i+1
        if(ip1 > nrow(poly)) ip1 <- 1
        dir <- (poly[ip1,] - poly[i,]) %*% matrix(c(0,-1,1,0),2,2)
        dir <- dir * r / sqrt(sum(dir^2))
        return(rbind(
            poly[i,] + dir,
            poly[ip1,] + dir
        ))
    })
    inside.sides <- inside.sides[keep]
    
    centers <- t(sapply(1:length(inside.sides), function(i){
        ip1 <- i+1
        if(ip1 > length(inside.sides)) ip1 <- 1
        lines.intersection(inside.sides[[i]][1,], inside.sides[[i]][2,],
                           inside.sides[[ip1]][1,], inside.sides[[ip1]][2,],
                           interior.only = FALSE)
    }))
    centers <- centers[c(nrow(centers),1:(nrow(centers)-1)),]
    
    return(centers)
}

round.corners <- function(poly, r){
    # need to shrink it to get centers of circles that will provide rounded edges
    centers <- shrink.poly(poly, r)
    
    smoothed.sides <- lapply(1:nrow(centers),function(i){
        ip1 <- i+1
        if(ip1 > nrow(centers)) ip1 <- 1
        
        dir <- (centers[ip1,] - centers[i,]) %*% 
            matrix(c(0,1,-1,0),2,2)
        dir <- dir * r / pracma::Norm(dir)
        
        return(rbind(
            centers[i,] + dir,
            centers[ip1,] + dir
        ))
    })
    
    # get points along the circle to make smooth curves
    for(si in 1:length(smoothed.sides)){
        sip1 <- si + 1
        if(sip1 > nrow(centers)) sip1 <- 1
        ci <- sip1
        
        v1 <- smoothed.sides[[si]][2,] - centers[ci, ]
        a1 <- atan2(v1[2],v1[1])
        v2 <- smoothed.sides[[sip1]][1,] - centers[ci, ]
        a2 <- atan2(v2[2],v2[1])
        if(a1 > a2){
            a2 <- a2+2*pi
        }
        arc <- cbind(centers[ci,1] + r*cos(seq(a1,a2, length.out = 27)[-c(1,27)]),
                     centers[ci,2] + r*sin(seq(a1,a2, length.out = 27)[-c(1,27)])
        )
        
        smoothed.sides[[si]] <- rbind(smoothed.sides[[si]], arc)
    }
    smoothed.sides <- do.call(rbind, smoothed.sides)
    return(smoothed.sides)
}

resize.poly <- function(poly, percent){
    orig <- poly
    poly <- poly * percent/100
    poly <- t(t(poly) - colMeans(poly) + colMeans(orig))
    return(poly)
}

adjacent <- function(id, maze, return.inds = FALSE, min = .013){
    # look at maze$dirsgs for potential neighbors
    borders <- maze$dirsgs[maze$dirsgs$ind1 == id | maze$dirsgs$ind2 == id, ]
    # don't keep neightbors if the border is too short
    keep <- apply(borders,1,function(x){
        pracma::Norm(x[3:4]-x[1:2]) > min
    })
    borders <- borders[keep, ]
    out.inds <- c(borders$ind1, borders$ind2)
    out.inds <- out.inds[out.inds != id]
    if(return.inds){
        return(out.inds)
    }
    return(maze$parent[out.inds])
}


