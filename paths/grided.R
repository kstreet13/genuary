require(pracma) # for distmat
require(matrixStats)

grid <- matrix(0, 500, 500)
htmat <- grid

nob <- 5
obstacles <- cbind(sample(nrow(grid),nob), sample(ncol(grid),nob))
for(i in 1:nob){ grid[obstacles[i,1],obstacles[i,2]] <- -1 }

nat <- 8
attractions <- cbind(sample(nrow(grid),nat), sample(ncol(grid),nat))
for(i in 1:nat){ grid[attractions[i,1],attractions[i,2]] <- 100 }

showgrid <- function(grid){
    plot(c(.5,ncol(grid)+.5),c(.5,nrow(grid)+.5), col='white', asp=1)
    rect(.5,.5,ncol(grid)+.5,nrow(grid)+.5)
    for(ii in 1:nrow(grid)){
        for(jj in 1:ncol(grid)){
            if(grid[ii,jj]==-1){
                points(ii, jj, pch = 13, col = 2)
            }
            if(grid[ii,jj]==100){
                points(ii,jj, pch = 16, col = 3)
            }
        }
    }
}

showgrid(grid)


# setup htmat
height <- function(d){
    4 * exp(-d/20)
}
idx <- cbind(rep(1:nrow(grid), times = ncol(grid)),
             rep(1:ncol(grid), each = nrow(grid)))
d <- rowMins(distmat(idx, obstacles))
htmat[,] <- height(d)



path <- rbind(c(1,1),c(1,2),c(1,3),c(2,3),c(3,4))

validPath <- function(path){
    if(!is.matrix(path)){
        return(FALSE)
    }
    if(!is.numeric(path)){
        return(FALSE)
    }
    if(ncol(path) != 2){
        return(FALSE)
    }
    if(nrow(path) > 1){
        for(i in 2:nrow(path)){
            d <- abs(path[i,]-path[i-1,])
            if(max(d) != 1){
                return(FALSE)
            }
            if(sum(d)!=1 & sum(d)!=2){
                return(FALSE)
            }
        }
    }
    return(TRUE)
}
costFun <- function(path){
    d <- sum(sqrt(rowSums((path[-1, ] - path[-nrow(path), ])^2)))
    htdif <- sapply(2:nrow(path), function(pi){
        htmat[path[pi,1], path[pi,2]] - 
            htmat[path[pi-1,1], path[pi-1,2]]
    })
    htdif <- htdif[htdif > 0]
    return(sum(d,htdif))
}
neighbors <- function(p, grid){
    adj <- matrix(c(-1,0,1,1,1,0,-1,-1, 1,1,1,0,-1,-1,-1,0), ncol=2)
    out <- t(p + t(adj))
    out <- out[out[,1] > 0 & 
                   out[,1] <= nrow(grid) &
                   out[,2] > 0 &
                   out[,2] < ncol(grid), ]
    return(out)
}
traceback <- function(tree, from = nrow(tree)){
    path <- tree[from,1:2]
    parent <- tree[from,3]
    while(parent!=0){
        from <- parent
        path <- rbind(path, tree[from,1:2])
        parent <- tree[from,3]
    }
    return(path[nrow(path):1, ])
}
findPath <- function(A, B){
    
    A <- c(1,1)
    B <- c(100,40)
    
    # find minimum cost path from A to B
    # cost = distance + height increases
    
    tree <- c(A, parent = 0)
    current <- 1
    boundary <- neighbors(A, grid)
    priority <- -sqrt(colSums((t(boundary) - B)^2))
    working <- TRUE
    while(working){
        # pick highest priority boundary location
        checknext <- which.max(priority)
        # add it to the tree
        tree <- rbind(tree, c(boundary[checknext,], current))
        current
        
        
    }
    
}






