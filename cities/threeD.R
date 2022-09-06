source('~/Projects/mazes/utils.R')

m <- matrix(-1, 9, 9)
m <- fill_maze(m)
plot(c(-2,ncol(m)),c(-2,nrow(m)+2), col='white', axes=FALSE,
     xlab = '', ylab = '', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "grey95")
mlines(m, lwd=4)

m <- toThick(m)

h <- m
for(i in 1:nrow(h)){
    for(j in 1:ncol(h)){
        if(h[i,j]==1){
            h[i,j] <- 0
        }
        if(h[i,j]==-5){
            # taller ones near the center
            lambda <- abs(i/nrow(h)-.5) + abs(j/ncol(h)-.5) + 1e-6
            lambda <- min(1,lambda)
            h[i,j] <- rgeom(1, lambda)
        }
    }
}


# 2D y-axis = y + z / 2

plot(c(0,ncol(h)), c(0,nrow(h)+max(h)/2), col='white', axes=FALSE)
elines(m, col='grey25')
for(i in 1:nrow(h)){
    for(j in 1:ncol(h)){
        if(h[i,j]==0){
            rect()
        }
    }
}



