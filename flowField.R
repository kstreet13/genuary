
showGrid <- function(grid, step_length = .5){
    plot(c(1,ncol(grid)), c(1,nrow(grid)), col='white', asp=1)
    
    x0 <- rep(1:ncol(grid), each = nrow(grid))
    y0 <- rep(1:nrow(grid), times = ncol(grid))
    x1 <- x0 + cos(grid)*step_length
    y1 <- y0 + sin(grid)*step_length
    
    arrows(x0,y0,x1,y1, length=.005)
}



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

plot(c(left_x,right_x), c(bottom_y,top_y), col='white')
rect(0,0,width,height)


for(i in 1:1000){
    lines(getpath(runif(2,max=100), num_steps = 500, step_length = 1), lwd=.25)
}


lines(getpath(c(500,500)))
lines(getpath(c(500,500), num_steps = 100), col=alpha(2, alpha=.3), lwd=2)









# spiral grid
grid <- matrix(0, nrow=50, ncol=50)
center <- c(25.5,25.5)
for(r in 1:nrow(grid)){
    for(c in 1:ncol(grid)){
        grid[r,c] <- (atan2(r-center[2], c-center[1]) + pi/2) %% (2*pi)
    }
}




showGrid(grid)





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

theta <- pi/8
ro <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2,2)


png('~/Desktop/lightning.png', width = 2000, height = 2000, res = 250)


plot(matrix(c(1,ncol(grid), 1,nrow(grid)),2,2) %*% ro, col='white',asp=1)
rect(-9999,-9999,9999,9999, col=hsv(.74, .6, .3))

for(i in 1:400){
    pth <- getpathSpecial(rnorm(2,mean=50,sd=20), grid, num_steps = 500, step_length = .5, center=c(50,50))
    lines(pth %*% ro, col = alpha(hsv(.99,.95,.95), alpha=.1), lwd=1.5)
    lines(pth %*% ro, lwd=.25, col = 'white')
}
for(i in 1:200){
    pth <- getpathSpecial(rnorm(2,mean=50,sd=3), grid, num_steps = 50, step_length = .5, center=c(50,50))
    lines(pth %*% ro, col = alpha(hsv(.11,.2,1), alpha=.1), lwd=3)
    lines(pth %*% ro, lwd=.25, col = 'white')
}


dev.off()

