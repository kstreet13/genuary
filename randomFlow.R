
# lightning spiral with circular middle
# then make hexagons, some transparent, some not

getpath <- function(start, grid, num_steps = 1000, step_length = 1){
    loc <- start
    path <- loc
    for(n in 1:num_steps){
        if(n %% 20 == 0){
            # check if it's stuck
            xchk <- path[(nrow(path)-15):nrow(path), ]
            if(all(colMaxs(xchk)-colMins(xchk) < 2)){
                break
            }
        }
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
        if(n %% 20 == 0){
            # check if it's stuck
            xchk <- path[(nrow(path)-15):nrow(path), ]
            if(all(colMaxs(xchk)-colMins(xchk) < 2)){
                break
            }
        }
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

showGrid <- function(grid, step_length = .5){
    plot(c(1,ncol(grid)), c(1,nrow(grid)), col='white', asp=1)
    
    x0 <- rep(1:ncol(grid), each = nrow(grid))
    y0 <- rep(1:nrow(grid), times = ncol(grid))
    x1 <- x0 + cos(grid)*step_length
    y1 <- y0 + sin(grid)*step_length
    
    arrows(x0,y0,x1,y1, length=.005)
}


rand <- matrix(0, nrow=50, ncol=50)
rand[,] <- runif(nrow(rand)*ncol(rand), min = -pi, max = pi)
rand <- mazing::expand_matrix(rand)

#grid <- spiral*p + rand*(1-p)
grid <- rand

#showGrid(grid)

################
# DEFINE PATHS #
################
# do 1000
num.paths <- 1000
path.len <- 2000

paths <- lapply(1:num.paths, function(pi){
    getpath(runif(2,min=1,max=99), grid, num_steps = path.len, step_length = .5)
})

# then adjust grid
grid <- grid + sample(c(-.5,.5), nrow(grid)*ncol(grid), replace = TRUE)
grid[grid < -pi] <- 2*pi + grid[grid < -pi]
grid[grid > pi] <- grid[grid > pi] - 2*pi

# and do 1000 more
paths <- c(paths, lapply(1:num.paths, function(pi){
    getpath(runif(2,min=1,max=99), grid, num_steps = path.len, step_length = .5)
}))

#############
# SET FRAME #
#############
xr <- range(do.call(rbind, paths)[,1])
yr <- range(do.call(rbind, paths)[,2])



############
# PLOTTING #
############

#
#png(paste0('~/Desktop/temp/',jj,'.png'), width = round(diff(xr)*40), height = round(diff(yr)*40), res = 250)
par(mar=c(0,0,0,0))
#


plot(xr, yr, col='white',asp=1)
rect(-9999,-9999,9999,9999, col='lightblue4')

for(pth in paths){
    lines(pth, col = 'lightblue')
}


#
#dev.off()
#







par(mar=c(5,4,4,2)+.1)



