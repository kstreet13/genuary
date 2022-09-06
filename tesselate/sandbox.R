source('utils.R')
require(pracma)
require(matrixStats)
require(deldir)

# make nicely separated, random points in space
x <- repelled_points(100)

plot(0:1,0:1,asp=1, col='white')
points(x)
rect(0,0,1,1)

dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))

plot(dd, wlines = 'tess', wpoints = 'dummy')


plot(0:1,0:1,asp=1, col='white')
points(x)
rect(0,0,1,1)
segments(dd$dirsgs$x1, dd$dirsgs$y1, dd$dirsgs$x2, dd$dirsgs$y2)



plot(0:1,0:1,asp=1, col='white')
pal <- c(brewer.pal(9,'Reds')[9], brewer.pal(9,'Oranges')[7], brewer.pal(9,'Blues')[3])
for(i in 1:nrow(x)){
    cc <- sample(pal,1)
    polygon(border(i,dd), border=cc, col=cc, lwd=0.3)
}


#
#
#
#




x <- repelled_points(10)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))


plot(dd,asp=1)
rect(-.05,-.05,1.05,1.05, col='whitesmoke')
pal <- c(brewer.pal(8,'Pastel2')[1:3])
for(i in dd$ind.orig){
    cc <- sample(pal,1)
    polygon(round.corners(shrink.poly(border(i,dd), r=.02), r=.04), border=NA, col=cc, lwd=0.3)
}



x <- repelled_points(50)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))

plot(0:1,0:1,asp=1, col='white')
rect(-.05,-.05,1.05,1.05, col='darkslategray') # darkslategray snow2
pal <- c(brewer.pal(9,'Reds')[9], brewer.pal(9,'Oranges')[7], brewer.pal(9,'Blues')[3])
for(i in 1:nrow(x)){
    cc <- sample(pal,1)
    polygon(round.corners(shrink.poly(border(i,dd), r=.01), r=.01), border=NA, col=cc, lwd=0.3)
}


# show how it's done
x <- repelled_points(10)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
r <- .05
plot(0:1,0:1, col='white', asp=1)

poly <- border(4,dd) #
polygon(poly, col=alpha(2,alpha=.5))
centers <- shrink.poly(poly, r)
for(i in 1:nrow(centers)){
    circ <- cbind(
        centers[i,1] + r*cos(seq(0,2*pi, length.out=100)),
        centers[i,2] + r*sin(seq(0,2*pi, length.out=100))
    )
    polygon(circ)
}
lines(round.corners(poly, r), col=3, lwd=2)



#
#
#
#

x <- repelled_points(200)

plot(0:1,0:1,asp=1, col='white')
points(x, cex = sample(c(1,1.5,2.5), 200, replace = TRUE, prob = 3:1))
rect(0,0,1,1)



p <- c(.5,.5)
tri <- rbind(c(.25,.25), c(.6,.3), c(.45, .7))

plot(0:1,0:1,asp=1, col='white')
polygon(tri, col=rgb(0,0,0,.5))
xseq <- seq(0,1, by=.02)
intri <- sapply(xseq,function(x){ point_in_triangle(c(x,.5), tri)})
points(xseq, rep(.5,length(xseq)), col = 1+as.numeric(intri))



x <- repelled_points(10)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))

plot(0:1,0:1,asp=1, col='white')
for(i in 1:nrow(x)){
    polygon(round.corners(shrink.poly(border(i,dd), r=.01), r=.01))
}

poly <- round.corners(shrink.poly(border(1,dd), r=.01), r=.01)
s <- seq(0,1, by=.02)
inshape <- sapply(1:length(s), function(i){
    point_in_shape(c(s[i],rev(s)[i]), poly, x[1,])
})
points(s,rev(s), col=2+as.numeric(inshape))

p <- c(s[21],rev(s)[21])


point_in_shape(p, poly, eye)


#
#
#





# old version that didn't do exactly what I wanted, but could be useful for
# slightly less uniformly spread points
n <- 100
x <- matrix(runif(2*n), ncol=2)
entropy <- function(p, base = exp(1)){
    p <- p[p > 0]
    -sum(p * log(p, base = base))
}
score <- function(x){
    entropy(table(cut(x[,1], breaks=(0:5)/5))/n, base = 5) +
        entropy(table(cut(x[,1], breaks=(0:10)/10))/n, base = 10) +
        entropy(table(cut(x[,1], breaks=(0:20)/20))/n, base = 20) +
        entropy(table(cut(x[,2], breaks=(0:5)/5))/n, base = 5) +
        entropy(table(cut(x[,2], breaks=(0:10)/10))/n, base = 10) +
        entropy(table(cut(x[,2], breaks=(0:20)/20))/n, base = 20)
}
for(i in 1:nrow(x)){
    xi <- x[i, ]
    x.new <- t(xi + matrix(c(0,0,rnorm(20, sd=.08)), nrow=2))
    # bounce off walls
    x.new[x.new > 1] <- 2 - x.new[x.new > 1]
    x.new[x.new < 0] <- -x.new[x.new < 0]
    # pick best point among old and new
    sc <- apply(x.new,1,function(xn){
        x.temp <- x
        x.temp[i,] <- xn
        score(x.temp)
    })
    # d <- rowSums(sqrt(distmat(x.new, x[-1,])))
    # d.wall <- .5 - rowMaxs(abs(x.new - .5))
    x[i,] <- x.new[which.max(sc),]
}
