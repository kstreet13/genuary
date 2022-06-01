
# lightning spiral with circular middle
# then make hexagons, some transparent, some not

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


###################
# GLOBAL SETTINGS #
###################
artsy <- FALSE
spiralcenter <- FALSE
hexes <- TRUE
whitelightning <- TRUE



for(jj in 1:50){

spiralcenter <- runif(1) < .5
hexes <- runif(1) < .5
whitelightning <- runif(1) < .5


# combine them (spinning lightning grid)
spiral <- matrix(0, nrow=100, ncol=100)
center <- c(50,50)
for(r in 1:nrow(spiral)){
    for(c in 1:ncol(spiral)){
        spiral[r,c] <- (atan2(r-center[2], c-center[1]) + pi/2) %% (2*pi)
    }
}
rand <- matrix(0, nrow=50, ncol=50)
rand[,] <- runif(nrow(rand)*ncol(rand), min = -pi, max = pi)
rand <- mazing::expand_matrix(rand)

# p is matrix representing percent spiral (1-p is percent rand)
p <- matrix(0, nrow=100, ncol=100)
for(r in 1:nrow(spiral)){
    for(c in 1:ncol(spiral)){
        d <- sqrt(sum((c(c,r)-center)^2))
        if(spiralcenter){
            p[r,c] <- .4 + .6*(1-pnorm(d*(7/30)-3))
            ####p[r,c] <- min(c(max(c(1-(d/40)^2 ,0)),1))
        }else{
            p[r,c] <- .4
        }
    }
}

grid <- spiral*p + rand*(1-p)

#showGrid(grid) #flowField.R


################
# DEFINE PATHS #
################
paths <- lapply(1:400, function(pi){
    if(spiralcenter){
        getpathSpecial(rnorm(2,mean=50,sd=.5), grid, num_steps = 500, step_length = .5, center=c(50,50))
    }else{
        getpathRandCtr(rnorm(2,mean=50,sd=.8), grid, num_steps = 500, step_length = .5, center=c(50,50))
    }
})


###################
# DEFINE HEX GRID #
###################
# sample hexes with lots of lightning

hr <- 9
dat <- unique(round(do.call(rbind, paths)))
xl <- range(dat[,1])
yl <- range(dat[,2])
xseq <- seq(xl[1],xl[2], by=hr)
yseq <- seq(yl[1],yl[2], by=.5*hr*sqrt(3))
#ang <- sample(c(0,pi/6,pi/3,pi,2))

centers <- cbind(rep(xseq, times=length(yseq)),
                 rep(yseq, each=length(xseq)))
for(i in 1:length(yseq)){
    if(i %% 2 == 0){
        centers[centers[,2]==yseq[i], 1] <- centers[centers[,2]==yseq[i], 1] + hr/2
    }
}
angseq <- seq(pi/6, 11*pi/6, by=pi/3)

# decide which hexes to keep
ind <- which(duplicated(round(do.call(rbind, paths))))
dat <- unique(round(do.call(rbind, paths))[ind,])

datInHex <- apply(centers,1,function(cnt){
    sum(sqrt(rowSums(t(t(dat)-cnt)^2)) < hr/2)
})
keep <- datInHex > 0
which.drop <- which(datInHex < quantile(datInHex[datInHex>0], .2))
keep[which.drop] <- FALSE
ndrop <- round(sum(keep)/15)
which.drop <- sample(which(keep), ndrop)
keep[which.drop] <- FALSE


#############
# SET FRAME #
#############
xr <- range(c(dat[,1], centers[keep,1]-hr/2, centers[keep,1]+hr/2))
yr <- range(c(dat[,2], centers[keep,2]-.25*hr*sqrt(3), centers[keep,2]+.25*hr*sqrt(3)))
if(artsy){
    p <- min(runif(2, min = .35, max = 2.5))
    if(runif(1) < .5){
        xr <- c(xr[1], xr[1] + p*(xr[2]-xr[1]))
    }else{
        xr <- c(xr[2] - p*(xr[2]-xr[1]), xr[2])
    }
    if(runif(1) < .5){
        yr <- c(yr[1], yr[1] + p*(yr[2]-yr[1]))
    }else{
        yr <- c(yr[2] - p*(yr[2]-yr[1]), yr[2])
    }
}


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
png(paste0('~/Desktop/temp/',jj,'.png'), width = round(diff(xr)*40), height = round(diff(yr)*40), res = 250)
par(mar=c(0,0,0,0))
#


plot(xr, yr, col='white',asp=1)
rect(-9999,-9999,9999,9999, col=bgcol2)
#rect(-9999,-9999,9999,9999, col=bgcol)

# shadow 1
for(pth in paths){
    lines(pth, col = alpha(fgcol,alpha=.2), lwd=4)
}



# draw hexes
if(hexes){
    for(i in 1:nrow(centers)){
        poly <- t(centers[i,] +
                      t((hr/sqrt(3)) * cbind(cos(angseq), sin(angseq))))
        if(keep[i]){
            polygon(poly, col = bgcol)
        }
    }
}

# shadow 2
for(pth in paths){
    lines(pth, col = alpha(fgcol,alpha=.1), lwd=2.5)
}
if(whitelightning){
    # white
    for(pth in paths){
        lines(pth, lwd=.25, col = hsv(fgH,.4,1,.7))
        #lines(pth, lwd=.25, col = rgb(0,0,0,.7))
    }
}

if(hexes){
    # hex edges
    for(i in 1:nrow(centers)){
        if(keep[i]){
            poly <- t(centers[i,] +
                          t((hr/sqrt(3)) * cbind(cos(angseq), sin(angseq))))
            polygon(poly, border = rgb(0,0,0,.1))
        }
    }
}




#
dev.off()
#


}




par(mar=c(5,4,4,2)+.1)





# dots stuff

# dots <- rep(FALSE, length(keep))
# poss.dots <- which(datInHex > quantile(datInHex[datInHex>0], .25) &
#                        datInHex < quantile(datInHex[datInHex>0], .75))
# ndots <- min(rpois(1, 1.5), length(poss.dots))
# if(ndots > 0){
#     which.dots <- sample(poss.dots, ndots)
#     dots[which.dots] <- TRUE
# }
# dots <- rep(FALSE, length(keep))# dots[which.drop] <- TRUE



# if(dots[i]){
#     poly <- t(centers[i,] +
#                   t((.97*hr/sqrt(3)) * cbind(cos(angseq), sin(angseq))))
#     if(keep[i]){
#         polygon(poly, col = bgcol, border = NA)
#     }else{
#         polygon(poly, col = bgcol2, border = NA)
#     }
#     sp <- rbind(poly, poly[1,])
#     sp <- st_polygon(list(sp))
#     pts <- st_sfc(lapply(1:nrow(dat),function(i){ st_point(dat[i,])}))
#     plotdots <- st_intersects(pts, sp, sparse = FALSE)[,1]
#     points(dat[which(plotdots),], col='white', pch=16, cex=.4)
# }