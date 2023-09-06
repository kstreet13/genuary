
# background?

# grey boxes, negated by random walk from each point along the border

size <- c(400, 300)

mat <- matrix(1, nrow = size[2], ncol = size[1])


border <- rbind(cbind(1, 2:ncol(mat)),
                cbind(nrow(mat), 2:ncol(mat)),
                cbind(2:nrow(mat), 1),
                cbind(2:nrow(mat), ncol(mat)))

middle <- c(.5+nrow(mat)/2, .5+ncol(mat)/2)
move <- function(current){
    poss <- rbind(current + c(1,0),
                  current + c(0,1),
                  current + c(-1,0),
                  current + c(0,-1))
    prob <- colSums((t(poss) - middle)^2)
    move <- sample(4,1, prob = prob)
    if(move==1){
        return(current + c(1,0))
    }
    if(move==2){
        return(current + c(0,1))
    }
    if(move==3){
        return(current + c(-1,0))
    }
    if(move==4){
        return(current + c(0,-1))
    }
}


for(i in 1:nrow(border)){
    current <- border[i,]
    while(current[1] <= nrow(mat) & current[1] > 0 &
          current[2] <= ncol(mat) & current[2] > 0){
        mat[current[1],current[2]] <- 0
        
        current <- move(current)
        

    }
}

image(mat)




s1 <- spline(rnorm(12, sd=.25), n = ncol(mat))
s2 <- spline(rnorm(19, sd=.25), n = nrow(mat))

mat2 <- mat
mat2[,] <- .5 + s2$y
mat2 <- t(t(mat2) + s1$y)
mn <- min(mat2); mx <- max(mat2)
mat2 <- .2 + ((mat2 - mn) / (mx - mn)) * .6 + runif(nrow(mat)*ncol(mat), min=-.1, max=.1)
mat2[mat==0] <- 0



image <- as.raster(1-mat2)



png('~/Desktop/bg.png', height = 3000, width = 4000, res=150)
op <- par(mar=c(.5,.5,.5,.5))
plot(0:1,0:1, axes=FALSE)
rasterImage(image, 0,0,1,1, interpolate = FALSE)
dev.off()


par(op)








