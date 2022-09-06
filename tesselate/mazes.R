source('utils.R')

# but make it mazes?

x <- repelled_points(100)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
maze <- dd
maze$parent <- rep(-1, nrow(dd$summary))
start <- 1

plot(dd, asp=1); points(x[1,1], x[1,2], col='red')

# where did it come from?
# 0 = origin, anything else = parent ID

previous <- function(id, maze){
    maze$parent[id]
}
fill_maze <- function(maze, start = NULL){
    if(is.null(start)){
        # pick a random start
        start <- sample(1:nrow(maze$summary),1)
    }
    maze$parent[start] <- 0
    last <- start
    while(sum(maze$parent == -1) > 0){
        # select next space from neighbors of 'last'
        adj <- adjacent(last, maze)
        # if no valid options, back up one step and try again
        poss <- which(adj == -1)
        if(length(poss) == 0){
            curr <- previous(last, maze)
        }else{
            curr <- adjacent(last, maze, return.inds = TRUE)[poss]
            if(length(curr) > 1) curr <- sample(curr, 1)
            maze$parent[curr] <- last
        }
        stopifnot(last!=curr)
        last <- curr
    }
    return(maze)
}

mazelines <- function(maze, ...){
    draw <- sapply(1:nrow(maze$dirsgs), function(i){
        i1 <- maze$dirsgs$ind1[i]
        i2 <- maze$dirsgs$ind2[i]
        return(maze$parent[i1]!=i2 && maze$parent[i2]!=i1)
    })
    segments(maze$dirsgs$x1[draw], maze$dirsgs$y1[draw],
             maze$dirsgs$x2[draw], maze$dirsgs$y2[draw], ...)
}






x <- repelled_points(500)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
maze <- dd
maze$parent <- rep(-1, nrow(dd$summary))
maze <- fill_maze(maze)

plot(dd, asp=1)

plot(0:1,0:1,asp=1, col='white')
points(x, col=rgb(0,0,0,.3))
rect(0,0,1,1)
mazelines(maze)


solvemaze <- function(maze, start=NULL, end=NULL){
    if(is.null(start)){
        d <- apply(maze$summary,1,function(x){
            pracma::Norm(x[1:2])
        })
        start <- unname(which.min(d))
        end <- unname(which.max(d))
    }
    # p1: start -> root
    p1 <- start
    parent <- maze$parent[start]
    while(parent != 0){
        p1 <- c(p1,parent)
        parent <- maze$parent[parent]
    }
    if(end %in% p1){
        return(p1[1:which.max(p1==end)])
    }
    # p2: end -> root
    p2 <- end
    parent <- maze$parent[end]
    while(parent != 0){
        p2 <- c(p2,parent)
        parent <- maze$parent[parent]
    }
    if(start %in% p2){
        return(rev(p2[1:which.max(p2==start)]))
    }
    while(p1[length(p1)] == p2[length(p2)]){
        last <- p1[length(p1)]
        p1 <- p1[-length(p1)]
        p2 <- p2[-length(p2)]
    }
    i12 <- c(p1[length(p1)], p2[length(p2)])
    if(any(maze$dirsgs$ind1 %in% i12 & maze$dirsgs$ind2 %in% i12)){
        path <- c(p1, rev(p2))
    }else{
        path <- c(p1, last, rev(p2))
    }
    return(path)
}


showpath <- function(maze, path, points = FALSE, col = 1, ...){
    pathcoords <- maze$summary[path[1], 1:2]
    for(i in 1:(length(path)-1)){
        i1 <- path[i]
        i2 <- path[i+1]
        line.ind <- which(maze$dirsgs$ind1 %in% c(i1,i2) & maze$dirsgs$ind2 %in% c(i1,i2))
        midpoint <- rowMeans(matrix(as.numeric(maze$dirsgs[line.ind,1:4]), 2,2))
        pathcoords <- rbind(pathcoords, midpoint,
                            maze$summary[i2, 1:2])
    }
    lines(pathcoords, col=col, ...)
    if(points){
        points(pathcoords[c(1,nrow(pathcoords)),], col = col, pch = 16)
    }
}




x <- repelled_points(500)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
maze <- dd
maze$parent <- rep(-1, nrow(dd$summary))
maze <- fill_maze(maze)

#plot(dd, asp=1)

plot(0:1,0:1,asp=1, col='white')
points(x, col=rgb(0,0,0,.3))
rect(0,0,1,1)
mazelines(maze)
points(maze$summary[maze$parent==0,1:2], col=2)

soln <- solvemaze(maze)
showpath(maze, soln, points=TRUE, col=3, lwd=2)



pathcoords <- maze$summary[soln[1], 1:2]
for(i in 1:(length(soln)-1)){
    i1 <- soln[i]
    i2 <- soln[i+1]
    line.ind <- which(maze$dirsgs$ind1 %in% c(i1,i2) & maze$dirsgs$ind2 %in% c(i1,i2))
    midpoint <- rowMeans(matrix(as.numeric(maze$dirsgs[line.ind,1:4]), 2,2))
    pathcoords <- rbind(pathcoords, midpoint,
                        maze$summary[i2, 1:2])
}
pathcoords[1,] <- c(.02,.02)
pathcoords[nrow(pathcoords),] <- c(.98,.98)



png('funky2.png', width = 600, height = 600, res = 150)

par(mar=c(1,1,1,1))
plot(0:1,0:1,asp=1, col='white',axes=FALSE)
rect(0,0,1,1, lwd=2)
mazelines(maze, lwd=2)

points(rbind(c(.02,.02),c(.98,.98)), col = 3:2, pch=16)

cc <- sample(4:6, nrow(x), replace = TRUE)
for(i in 1:nrow(x)){
    polygon(shrink.poly(border(i,maze), r=.008), col=alpha(cc[i],alpha=.2), border = NA)
}
lines(pathcoords, col=1, lwd=2.5)
lines(pathcoords, col=6, lwd=1.8)
points(rbind(c(.02,.02),c(.98,.98)), col = 3:2, pch=16)


dev.off()

par(mar=c(5.1,4.1,4.1,2.1)) # reset
