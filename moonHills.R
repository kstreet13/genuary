
makeOne <- function(x){
    if(runif(1) > 1){
        # day mode
        if(runif(1) > .5){
            bg.h <- runif(1, min=.45, max=.77) # blue
        }else{
            bg.h <- runif(1, min=.47, max=1.1) %% 1 # sunset?
        }
        bgcol <- hsv(bg.h, runif(1,min=0,max=.75), runif(1,min=.25))
        s <- runif(1, max=.8)
        b <- runif(1)
        col1 <- hsv(runif(1), s, b)
        col2 <- hsv(runif(1), s, b)
    }else{
        # night mode
        bgcol <- hsv(runif(1), runif(1), runif(1, max=.25))
        col1 <- hsv(runif(1), runif(1,min=.5), runif(1,min=.6))
        col2 <- hsv(runif(1), runif(1,min=0), runif(1,min=.6))
    }

    gridsize <- round(runif(1, min=5.5, max=13.5))
    
    grid <- cbind(rep(seq(0,gridsize,by=.5), each = 2*gridsize+1),
                  rep(seq(0,gridsize,by=.5), times = 2*gridsize+1))
    idx <- which(grid[,1] %% 1 == .5)
    grid[idx,2] <- grid[idx,2]-.25
    grid[,1] <- grid[,1] / sqrt(2)
    
    #size <- scaleAB(-sqrt(grid[,1]^2 + (2*grid[,2])^2))
    size <- scaleAB(-apply(grid,1, function(x){
        pracma::Norm(x - c(0,gridsize))
    }), b=.7)
    size[grid[,1]==min(grid[,1]) & grid[,2]==max(grid[,2])] <- 10
    cc <- rep('white', nrow(grid))
    
    slope <- runif(1, min=-.5,max=.9)
    center <- c(mean(range(grid[,1])),
                 weighted.mean(range(grid[,2]), w=c(1/3,2/3)))
    idx <- which(grid[,2]-center[2] < slope*(grid[,1]-center[1]))
    size[idx] <- scaleAB(grid[idx,2], a=.5, b=2)
    cc[idx] <- col1
    
    slope <- runif(1, min=-.5,max=.9)
    center <- c(mean(range(grid[,1])),
                weighted.mean(range(grid[,2]), w=c(.5,.5)))
    idx <- which(grid[,2]-center[2] < slope*(grid[,1]-center[1]))
    size[idx] <- scaleAB(grid[idx,2], a=.5,b=2)
    cc[idx] <- col2
        
    keep.square <- which(grid[,2] > max(grid[,2]) - diff(range(grid[,1])))
    grid <- grid[keep.square,]
    size <- size[keep.square]
    cc <- cc[keep.square]
    
    size <- size * 10 / gridsize
    
    plot(range(grid[,1])+c(-1,1),range(grid[,2])+c(-1,1), asp=1, axes=FALSE)
    rect(-9999,-9999,9999,9999, col=bgcol)
    points(grid, col=cc, cex = size, pch=16)
}



layout(matrix(1:9,nrow=3))
par(mar = c(1,1,1,1))
for(i in 1:9){
    makeOne()
}

par(mar = c(5,4,4,2)+.1)
layout(1)







for(i in 1:100){
    png(paste0('~/Desktop/test/',i,'.png'), width = 290, height = 290, res = 50)
    par(mar = c(.1,.1,.1,.1))
    makeOne()
    dev.off()
}



