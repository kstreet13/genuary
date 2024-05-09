

showField <- function(field, xleft, ybottom, xright, ytop, add = TRUE){
    if(!add){
        plot(c(xleft,xright), c(ybottom,ytop), col='white')
    }

    x0 <- seq(from = xleft, to = xright, length.out = ncol(field[[1]]))
    x0 <- rep(x0, each = nrow(field[[1]]))
    y0 <- seq(from = ybottom, to = ytop, length.out = nrow(field[[1]]))
    y0 <- rep(y0, times = ncol(field[[1]]))
    
    x1 <- x0 + field[[1]]
    y1 <- y0 + field[[2]]
    
    arrows(x0,y0,x1,y1, length=.005)
}

convolve <- function(grid){
    grid <- grid + 
        cbind(grid[,-1],0) + # left
        cbind(0, grid[,-ncol(grid)]) + # right
        rbind(grid[-1,], 0) + # up
        rbind(0, grid[-nrow(grid),]) + # down
        cbind(0, rbind(grid[-1,], 0)[,-ncol(grid)]) + # up-right
        cbind(0, rbind(0, grid[-nrow(grid),])[,-ncol(grid)]) + # down-right
        cbind(rbind(0, grid[-nrow(grid),])[,-1], 0) + # down-left
        cbind(rbind(grid[-1,], 0)[,-1], 0) # up-left
        
    # denominator
    denom <- matrix(9, nrow = nrow(grid), ncol = ncol(grid))
    denom[,1] <- denom[,ncol(grid)] <- 6
    denom[1,] <- denom[nrow(grid),] <- 6
    denom[1,1] <- denom[1,ncol(grid)] <- denom[nrow(grid),1] <-
        denom[nrow(grid), ncol(grid)] <- 4
    
    return(grid/denom)
}

updateField <- function(field){
    field$x <- convolve(field$x)
    field$y <- convolve(field$y)
    return(field)
}

normField <- function(field){
    N <- sqrt(field$x^2 + field$y^2)
    field$x <- field$x / N
    field$y <- field$y / N
    return(field)
}


getpath <- function(field, start, xleft, ybottom, xright, ytop, 
                    num_steps = 100, step_length = 1){
    x0 <- seq(from = xleft, to = xright, length.out = ncol(field[[1]]))
    #x0 <- rep(x0, each = nrow(field[[1]]))
    y0 <- seq(from = ybottom, to = ytop, length.out = nrow(field[[1]]))
    #y0 <- rep(y0, times = ncol(field[[1]]))
    
    loc <- path <- start

    for(n in 1:num_steps){
        # inside the grid
        if(loc[1] > xleft & loc[1] < xright &
           loc[2] > ybottom & loc[2] < ytop){
            col.ind <- which.max(loc[1] < x0)
            col.ind <- c(col.ind-1, col.ind)
            row.ind <- which.max(loc[2] < y0)
            row.ind <- c(row.ind-1, row.ind)
            
            r.x <- loc[1] - x0[col.ind[1]]
            r.y <- loc[2] - y0[row.ind[1]]
            
            w <- 1/matrix(c(
                r.x^2 + r.y^2,
                r.x^2 + (1-r.y)^2,
                (1-r.x)^2 + r.y^2,
                (1-r.x)^2 + (1-r.y)^2
            ),2)
            w <- w/sum(w)
            
            vec <- c(sum(field$x[row.ind,col.ind]*w), sum(field$y[row.ind,col.ind]*w))
            
            loc <- loc + step_length*vec
            path <- rbind(path, loc)
            if(vec[1]^2+vec[2]^2 < 1e-6) return(path)
        }else{
            break
        }
    }
    return(path)
}




plot(c(-100,100), c(-100,100), col='white', asp=1)
rect(-50,-50,50,50)



# random
angles <- matrix(runif(100*100, max=2*pi), nrow=100)
field1 <- list(x = cos(angles), y = sin(angles))
field2 <- normField(updateField(field1))
field3 <- normField(updateField(field2))
field4 <- normField(updateField(field3))
field5 <- normField(updateField(field4))
field6 <- normField(updateField(field5))
field7 <- normField(updateField(field6))
field8 <- normField(updateField(field7))
field9 <- normField(updateField(field8))
field10 <- updateField(field9)

for(i in 1:10){
    plot(c(-100,100), c(-100,100), col='white', asp=1, main = i)
    rect(-50,-50,50,50)
    abline(30,-1)
    abline(-30,-1)
    if(i==1) showField(field1,-100,-100,100,100) 
    if(i==2) showField(field2,-100,-100,100,100) 
    if(i==3) showField(field3,-100,-100,100,100) 
    if(i==4) showField(field4,-100,-100,100,100) 
    if(i==5) showField(field5,-100,-100,100,100) 
    if(i==6) showField(field6,-100,-100,100,100) 
    if(i==7) showField(field7,-100,-100,100,100) 
    if(i==8) showField(field8,-100,-100,100,100) 
    if(i==9) showField(field9,-100,-100,100,100) 
    if(i==10) showField(field10,-100,-100,100,100) 
}


plot(c(-100,100), c(-100,100), col='white', asp=1)
rect(-50,-50,50,50)
showField(field9,-100,-100,100,100)
path <- getpath(field9, c(0,0), -100,-100,100,100,
                num_steps = 1000, step_length = .1)
lines(path, col=2)


for(i in 1:500){
    path <- getpath(field9, runif(2,min=-50,max=50), -100,-100,100,100,
                    num_steps = 1000, step_length = .1)
    lines(path, col=i)
}



# diagonals
field <- list(x = matrix(0, 100,100), y = matrix(0, 100,100))
xs <- rep(1:100, each=100)
ys <- rep(1:100, times=100)
field$x[xs+ys <= 100] <- .1
field$x[xs+ys > 100] <- -.1
field$y[xs+ys <= 100] <- .1
field$y[xs+ys > 100] <- -.1

ind1 <- which(xs+ys==90 & xs > 25 & xs < 75 & ys > 25 & ys < 75)
ind2 <- which(xs+ys==110 & xs > 25 & xs < 75 & ys > 25 & ys < 75)
for(i in 1:30){
    field$x[ind1] <- -cos(pi/4)
    field$x[ind2] <- cos(pi/4)
    field$y[ind1] <- sin(pi/4)
    field$y[ind2] <- -sin(pi/4)
    field <- updateField(field)
}

plot(c(-100,100), c(-100,100), col='white', asp=1)
rect(-50,-50,50,50)
showField(field,-100,-100,100,100)
abline(30,-1)
abline(-30,-1)

for(i in 1:500){
    path <- getpath(field, runif(2,min=-50,max=50), -100,-100,100,100,
                    num_steps = 1000, step_length = .1)
    lines(path, col=i)
}


