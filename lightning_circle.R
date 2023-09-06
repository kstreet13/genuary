
# shapes made out of lightning


{
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
            # if(all(c(column_index,row_index)==center)){
            #     grid_angle <- runif(1,max=2*pi)
            # }
            if(sqrt(sum((loc-center)^2)) < 2){
                p1 <- loc[1] - floor(loc[1])
                p2 <- loc[2] - floor(loc[2])
                ci1 = floor(loc[1])
                ri1 = floor(loc[2])
                angs <- c(grid[ri1,ci1], grid[ri1+1,ci1],
                          grid[ri1,ci1+1], grid[ri1+1,ci1+1])
                ws <- 1/c(sqrt(p1^2+p2^2),
                          sqrt(p1^2+(1-p2)^2),
                          sqrt((1-p1)^2+p2^2),
                          sqrt((1-p1)^2+(1-p2)^2))
                xc <- sum(ws * cos(angs)) / sum(ws)
                yc <- sum(ws * sin(angs)) / sum(ws)
                n <- sqrt(xc^2 + yc^2)
                xc <- step_length * xc/(2*n)
                yc <- step_length * yc/(2*n)
                loc <- loc + c(xc, yc)
            }else{
                grid_angle <- grid[row_index, column_index]
                loc <- loc + c(step_length * cos(grid_angle), 
                               step_length * sin(grid_angle))
            }
            path <- rbind(path, loc)
        }else{
            break
        }
    }
    return(path)
}

getpathRandCtr <- function(start, grid, num_steps = 1000, step_length = 1, center = c(50,50)){
    loc <- start
    path <- loc
    for(n in 1:num_steps){
        column_index = round(loc[1])
        row_index = round(loc[2])
        if(column_index > 0 & column_index <= ncol(grid) &
           row_index > 0 & row_index <= nrow(grid)){
            if(all(c(column_index,row_index)==center)){
                grid_angle <- runif(1,max=2*pi)
            }else{
                grid_angle <- grid[row_index, column_index]
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
}



# circle

r1 <- 19
r2 <- 27

spiral <- matrix(0, nrow=100, ncol=100)
center <- c(50.5,50.5)
for(r in 1:nrow(spiral)){
    for(c in 1:ncol(spiral)){
        d <- sqrt(sum(c(r-center[2], c-center[1])^2))
        if(d > r1 & d < r2){
            # circle
            spiral[r,c] <- atan2(r-center[2], c-center[1]) + pi/2
            #spiral[r,c] <- (atan2(r-center[2], c-center[1]) + pi/2 + runif(1, min=-pi/2, max=pi/2)) %% (2*pi) 
        }else if(d <= r1){
            # point out
            spiral[r,c] <- atan2(r-center[2], c-center[1])
            #spiral[r,c] <- atan2(r-center[2], c-center[1]) + runif(1, min=-pi, max=pi) %% (2*pi)
        }else if(d >= r2){
            # point in
            spiral[r,c] <- atan2(r-center[2], c-center[1]) - pi
            #spiral[r,c] <- (atan2(r-center[2], c-center[1]) - pi + runif(1, min=-pi, max=pi)) %% (2*pi)
        }
    }
}
rand <- matrix(0, nrow=50, ncol=50)
for(r in 1:nrow(rand)){
    for(c in 1:ncol(rand)){
        d <- sqrt(sum(c(r-center[2], c-center[1])^2))
        if(d > r1 & d < r2){
            # circle
            rand[r,c] <- runif(1, min=-pi/2, max=pi/2) 
        }else{
            rand[r,c] <- runif(1, min=-pi, max=pi)
        }
    }
}
rand <- mazing::expand_matrix(rand)
grid <- (spiral + rand) %% (2*pi)
#grid <- spiral

#showGrid(grid) #flowField.R




################
# DEFINE PATHS #
################
npaths <- 400

l <- runif(npaths, min = r1, max = r2)
a <- runif(npaths, min = 0, max = 2*pi)
starts <- cbind(center[1] + l*cos(a), center[2] + l*sin(a))
paths <- lapply(1:npaths, function(pi){
    getpath(starts[pi,], grid, num_steps = 500, step_length = .5)
})
dat <- unique(round(do.call(rbind, paths)))




###############
# PICK COLORS #
###############
bgH <- fgH <- runif(1)
while(abs(fgH-bgH) < .15){ fgH <- runif(1) }
bgcol <- hsv(bgH, .75, .25)
bgcol2 <- hsv(bgH, .8, .1)
fgcol <- hsv(fgH, .95, .95)
# color test
plot(c(1,1,1,1), c(1,1,1,1), cex = c(100,40,10,1), col=c(bgcol2,bgcol,fgcol,'white'))




############
# PLOTTING #
############

#
#png(paste0('~/Desktop/temp/',jj,'.png'), width = round(diff(xr)*40), height = round(diff(yr)*40), res = 250)
#par(mar=c(0,0,0,0))
#

xr <- c(1,ncol(grid))
yr <- c(1,nrow(grid))
plot(xr, yr, col='white',asp=1)
rect(-9999,-9999,9999,9999, col=bgcol2)
#rect(-9999,-9999,9999,9999, col=bgcol)

# shadow 1
for(pth in paths){
    lines(pth, col = alpha(fgcol,alpha=.2), lwd=4)
}

# shadow 2
for(pth in paths){
    lines(pth, col = alpha(fgcol,alpha=.1), lwd=2.5)
}






#
#dev.off()
#

