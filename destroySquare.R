
# destroy a square


getpath <- function(start, grid, num_steps = 1000, step_length = 1){
    loc <- start
    path <- loc
    for(n in 1:num_steps){
        column_index = round(loc[1])
        row_index = round(loc[2])
        if(column_index > 0 & column_index <= ncol(grid) &
           row_index > 0 & row_index <= nrow(grid)){
            grid_angle = grid[row_index, column_index]
            loc <- loc + c(step_length * cos(grid_angle), 
                           step_length * sin(grid_angle))
            path <- rbind(path, loc)
        }else{
            break
        }
    }
    return(path)
}

getpathSpecial <- function(start, grid, num_steps = 1000, step_length = 1, center = c(50,50)){
    loc <- start
    path <- loc
    for(n in 1:num_steps){
        column_index = round(loc[1])
        row_index = round(loc[2])
        if(column_index > 0 & column_index <= ncol(grid) &
           row_index > 0 & row_index <= nrow(grid)){
            grid_angle <- grid[row_index, column_index]
            if(all(c(column_index,row_index)==center)){
                grid_angle <- runif(1,max=2*pi)
            }
            loc <- loc + c(step_length * cos(grid_angle), 
                           step_length * sin(grid_angle))
            path <- rbind(path, loc)
        }else{
            break
        }
    }
    return(path)
}


# spiral grid
grid <- matrix(0, nrow=50, ncol=50)
center <- c(25.5,25.5)
for(r in 1:nrow(grid)){
    for(c in 1:ncol(grid)){
        grid[r,c] <- (atan2(r-center[2], c-center[1]) + pi/2) %% (2*pi)
    }
}




showGrid(grid) #flowField.R





# combine them (spinning lightning grid)
spiral <- matrix(0, nrow=100, ncol=100)
center <- c(50,50)
for(r in 1:nrow(spiral)){
    for(c in 1:ncol(spiral)){
        spiral[r,c] <- (atan2(r-center[2], c-center[1]) + pi/2) %% (2*pi)
    }
}
spiral[49,51] <- spiral[50,51]
spiral[51,51] <- spiral[51,50]
spiral[51,49] <- spiral[50,49]
spiral[49,49] <- spiral[49,50]
rand <- matrix(0, nrow=50, ncol=50)
rand[,] <- runif(nrow(rand)*ncol(rand), min = -pi, max = pi)
rand <- mazing::expand_matrix(rand)


# p is matrix representing percent spiral (1-p is percent rand)
p <- matrix(0, nrow=100, ncol=100)
for(r in 1:nrow(spiral)){
    for(c in 1:ncol(spiral)){
        d <- sqrt(sum((c(c,r)-center)^2))
        p[r,c] <- .4 + .6*(1-pnorm(d*(7/40)-3))
        #p[r,c] <- min(c(max(c(1-(d/40)^2 ,0)),1))
    }
}

grid <- spiral*p + rand*(1-p)
#showGrid(grid)



png('~/Desktop/lightning.png', width = 2000, height = 2000, res = 250)

# rotation
theta <- runif(1, max = pi)
ro <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2,2)
# colors
bgcol <- fgcol <- runif(1)
while(abs(fgcol-bgcol) < .15){ fgcol <- runif(1) }
bgcol <- hsv(bgcol, .75, .25)
fgcol <- hsv(fgcol, .95, .95)
# color test
plot(c(1,1,1), c(1,1,1), cex = c(100,10,1), col=c(bgcol,fgcol,'white'))


plot(matrix(c(1,ncol(grid), 1,nrow(grid)),2,2) %*% ro, col='white',asp=1)
#rect(-9999,-9999,9999,9999, col=hsv(.74, .6, .3))
rect(-9999,-9999,9999,9999, col=bgcol)

paths1 <- lapply(1:400, function(pi){
    getpathSpecial(rnorm(2,mean=50,sd=20), grid, num_steps = 500, step_length = .5, center=c(50,50))
})
paths2 <- lapply(1:200, function(pi){
    getpathSpecial(rnorm(2,mean=50,sd=3), grid, num_steps = 50, step_length = .5, center=c(50,50))
})
paths <- c(paths1, paths2)

for(pth in paths){
    lines(pth %*% ro, col = alpha(fgcol,alpha=.1), lwd=1.5)
}
for(pth in paths){
    lines(pth %*% ro, lwd=.25, col = 'white')
}


dev.off()

