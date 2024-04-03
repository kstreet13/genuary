
# play with splines

# spline (curve lines through fixed points. regular spline function treats x as predictor, y as outcome, but I just want a curve)
myspline <- function(x,y, sort = FALSE, circular = FALSE, ...){
    if(sort){
        y <- y[order(x)]
        x <- sort(x)
    }
    if(circular){
        x <- c(x[(length(x)-1):length(x)], x, x[1:3])
        y <- c(y[(length(y)-1):length(y)], y, y[1:3])
    }
    sx <- spline(1:length(x),x, ...)
    sy <- spline(1:length(y),y, ...)
    if(circular){
        ind <- which(sx$x >= 3 & sx$x <= length(x)-2)
        sx$y <- sx$y[ind]
        sy$y <- sy$y[ind]
    }
    return(list(x=sx$y, y=sy$y))
}

x <- rnorm(10)
y <- 1:10

s1 <- myspline(x, y, n=201)

plot(s1, asp=1)



# start from straight line
plot(c(1,45),c(1,10), asp=1, col='white')

x <- rep(1,10)
y <- 1:10
lines(myspline(x,y, n=201))

for(i in 2:40){
    x <- x + 1 + rnorm(10, sd=.1)
    lines(myspline(x,y, n=201))
}



# start from wiggly line
plot(c(0,105),c(1,10), asp=1, col='white')

x <- rnorm(10, sd=.5)
y <- 1:10
lines(myspline(x,y, n=201))

# negative correlation between perturbation and current status will stabilize it
for(i in 2:100){
    x <- x + 1 + (-.01*scale(x) + rnorm(10, sd=.1))
    lines(myspline(x,y, n=201))
}







# make them colored polygons
n <- 100
plot(c(0,n+5),c(1,20), asp=1, col='white')


pal <- colorby(1:n, colors = brewer.pal(11,'Spectral'))

x <- rnorm(10, sd=.5)
y <- seq(2,20,by=2)
s.old <- myspline(x,y, n=201)
lines(s.old)

# negative correlation between perturbation and current status will stabilize it
for(i in 2:(n+1)){
    x <- x + 1 + (-.01*scale(x) + rnorm(10, sd=.1))
    s.new <- myspline(x,y, n=201)
    lines(s.new)
    polygon(c(s.new$x, rev(s.old$x)), c(s.new$y, rev(s.old$y)), col=pal[i-1])
    s.old <- s.new
}






# overlapping (river?)
# start from wiggly line
plot(c(-2,2),c(1,10), asp=1, col='white')

x <- rnorm(10, sd=.5)
y <- 1:10
lines(myspline(x,y, n=201))

# negative correlation between perturbation and current status will stabilize it
for(i in 2:50){
    x <- x + (-.01*scale(x) + rnorm(10, sd=.1))
    lines(myspline(x,y, n=201))
}



# start from wiggly line, random "depth"
depth <- sample(1:10, 1)

plot(c(-1,12),c(1,10), asp=1, col='white', main=depth)
for(depth in 1:10){
    x <- depth + rnorm(10*depth, sd=.5)
    y <- seq(1,10, length.out = 10*depth)
    
    # negative correlation between perturbation and current status will stabilize it
    for(i in 1:50){
        lines(myspline(x,y, n=301), lwd = sqrt(depth), col = alpha(1, alpha=1/(depth^2)))
        x <- x + (-.01*scale(x) + rnorm(10*depth, sd=.1))
    }
    
}



# start from wiggly line, slowly get more "depth"
plot(c(-1,52),c(1,10), asp=1, col='white')

depthseq <- seq(1,10, length.out = 100)

x <- rnorm(10, sd=.5)
y <- 1:10
s <- myspline(x,y, n=301)

# negative correlation between perturbation and current status will stabilize it
for(depth in depthseq){
    #lines(s$x + 5*depth, s$y, lwd = sqrt(depth), col = alpha(1, alpha=1/(depth^2)))
    lines(s$x + 5*depth, s$y)
    x <- s$x[round(seq(1,301,length.out=length(x)+1))]
    y <- s$y[round(seq(1,301,length.out=length(y)+1))]
    x <- x + (-.01*scale(x) + rnorm(length(x), sd=.1))
    s <- myspline(x,y, n=301)
}
