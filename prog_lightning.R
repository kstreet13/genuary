
# lightning that builds on itself
require(matrixStats)
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

repelled_points <- function(n, multiplier = 15, rw = c(0,1,0,1)){
    x0 <- matrix(runif(2*multiplier*n), ncol=2)
    x0[,1] <- (x0[,1] + rw[1])*(rw[2]-rw[1])
    x0[,2] <- (x0[,2] + rw[3])*(rw[4]-rw[3])
    km <- kmeans(x0, centers=n)
    return(km$centers)
}

angle_avg <- function(a1, a2, w1 = .5, w2 = .5){
    n.w1 <- w1 / (w1+w2)
    n.w2 <- w2 / (w1+w2)
    (a2 + (n.w1/(n.w1+n.w2))*((a1-a2) %% (2*pi))) %% (2*pi)
}

###################
# GLOBAL SETTINGS #
###################
artsy <- FALSE
hexes <- FALSE
whitelightning <- TRUE





rand <- matrix(0, nrow=50, ncol=50)
rand[,] <- runif(nrow(rand)*ncol(rand), min = 0, max = 2*pi)
rand <- mazing::expand_matrix(rand)

# still want some local smoothing
# within local 10x10
for(i in 1:10){
    for(j in 1:10){
        r.ind <- (10*(i-1)+1):(10*i)
        c.ind <- (10*(j-1)+1):(10*j)
        m <- runif(1, min = 0, max = 2*pi)
        rand[r.ind,c.ind] <- angle_avg(rand[r.ind,c.ind], m, .7, .3)
    }
}
# within local 33x33
for(i in 1:3){
    for(j in 1:3){
        r.ind <- which(1:100 > (i-1)*100/3 & 1:100 <= (i)*100/3)
        c.ind <- which(1:100 > (j-1)*100/3 & 1:100 <= (j)*100/3)
        m <- runif(1, min = 0, max = 2*pi)
        rand[r.ind,c.ind] <- angle_avg(rand[r.ind,c.ind], m, .7, .3)
    }
}

grid <- rand

#showGrid(grid) #flowField.R


################
# DEFINE PATHS #
################
num.paths <- 1000
path.len <- 500

paths <- list()
start <- c(50.5, 50.5)
for(p.i in 1:num.paths){
    paths[[p.i]] <- rbind(start,
                          getpath(start+rnorm(2,sd=.8), grid, num_steps = path.len, step_length = .5))
    lens <- lengths(paths) /2
    #w.p <- sample(length(paths),1, prob = 1:length(paths))
    #start <- paths[[w.p]][sample(lens[w.p], 1, prob = 1:lens[w.p]), ]
    w.p <- sample(length(paths),1, prob = 1:length(paths))
    start <- ifelse(runif(1) < .5,
        paths[[w.p]][1,], paths[[w.p]][lens[w.p],])
    
}



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
#png(paste0('~/Desktop/temp/',jj,'.png'), width = round(diff(xr)*40), height = round(diff(yr)*40), res = 250)
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
            polygon(poly, col = bgcol, border = bgcol2)
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

# if(hexes){
#     # hex edges
#     for(i in 1:nrow(centers)){
#         if(keep[i]){
#             poly <- t(centers[i,] +
#                           t((hr/sqrt(3)) * cbind(cos(angseq), sin(angseq))))
#             polygon(poly, border = rgb(0,0,0,.1))
#         }
#     }
# }




#
#dev.off()
#







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