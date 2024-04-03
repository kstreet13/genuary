n <- 100
switch <- rbinom(n, 1, .1)
rates <- c(1, 100)
rate <- rep(rates,times=sum(switch))[cumsum(switch)]
x <- cumsum(rexp(n, rate = rate))

y <- rnorm(n, sd=log(rate))




# gentle arc + noise
# 1.25*pi - 1.75*pi
theta <- seq(1.4*pi, 1.6*pi, length.out = 200)
x <- 10*cos(theta)
y <- -10*sin(theta)








# spline (curve lines through fixed points)
myspline <- function(x,y, sort = FALSE, circular = FALSE, by.dist = FALSE, ...){
    stopifnot(length(x)==length(y))
    if(sort){
        y <- y[order(x)]
        x <- sort(x)
    }
    if(circular){
        x <- c(x[(length(x)-1):length(x)], x, x[1:3])
        y <- c(y[(length(y)-1):length(y)], y, y[1:3])
    }
    if(by.dist){
        d <- sqrt(diff(x)^2 + diff(y)^2)
        d <- c(0,cumsum(d))
    }else{
        d <- 1:length(x)
    }
    sx <- spline(d,x, ...)
    sy <- spline(d,y, ...)
    if(circular){
        ind <- which(sx$x >= 3 & sx$x <= length(x)-2)
        sx$y <- sx$y[ind]
        sy$y <- sy$y[ind]
    }
    return(list(x=sx$y, y=sy$y))
}
point <- function(p, ...){
    points(p[1], p[2], ...)
}

# window = 0,10,0,10

theta <- runif(1,0,2*pi)
anchor <- c(5,5) + 1*c(cos(theta),sin(theta))
p2 <- c(5,5) + 2*c(cos(theta),sin(theta))
theta <- theta+pi/2
b <- tan(theta)
a <- anchor[2] - anchor[1] * b
if(a < 0){
    p1 <- c(-a/b, 0)
}else if(a > 10){
    p1 <- c((10-a)/b, 10)
}else{
    p1 <- c(0, a)
}
if(a+b*10 < 0){
    p3 <- c(-a/b, 0)
}else if(a+b*10 > 10){
    p3 <- c((10-a)/b, 10)
}else{
    p3 <- c(10, a+b*10)
}

# plot(c(0,10),c(0,10),col='white', asp=1)
# abline(v=c(0,10))
# abline(h=c(0,10))
# lines(5+1*cos(seq(0,2*pi,length.out=200)),
#       5+1*sin(seq(0,2*pi,length.out=200)))
# points(anchor[1],anchor[2])
# lines(c(anchor[1]-100*cos(theta),anchor[1],anchor[1]+100*cos(theta)),
#       c(anchor[2]-100*sin(theta),anchor[2],anchor[2]+100*sin(theta)))
# point(p1)
# point(p2)
# point(p3)

s <- myspline(c(p1[1],p2[1],p3[1]), c(p1[2],p2[2],p3[2]), n=30)
# lines(s)

jj <- sample(c(2:29), 1) # 2:14, 17:29
p4 <- c(s$x[jj], s$y[jj]) + rnorm(2, sd=.5)
while(any(p4 < 0) | any(p4 > 10)){
    p4 <- c(s$x[jj], s$y[jj]) + rnorm(2, sd=.5)
}
# points(s$x[jj], s$y[jj], col=2)
if(jj < 15){
    s <- myspline(c(p1[1],p4[1],p2[1],p3[1]), c(p1[2],p4[2],p2[2],p3[2]), by.dist = TRUE, n=30)
}else{
    s <- myspline(c(p1[1],p2[1],p4[1],p3[1]), c(p1[2],p2[2],p4[2],p3[2]), by.dist = TRUE, n=30)
}
# lines(s)

# add squiggles
n.drop <- sample(2:5, 1)
ii <- sample(2:(30-(n.drop+1)), 1)
s1 <- c(s$x[ii], s$y[ii])
s2 <- c(s$x[ii+n.drop], s$y[ii+n.drop])
d <- sqrt(sum((s1-s2)^2))

# point(s1, col=2)
# point(s2, col=2)

# make un-rotated squiggles (squigs)
{
    k <- sample(5:7, 1)
    # .5, 1,...,1, .5
    r <- (d / (k+1)) / 2
    d2 <- d-2*r
    
    squigs <- cbind(
        r*cos(seq(1.5*pi, 2*pi, length.out = 15)),
        d2/2+r + r*sin(seq(1.5*pi, 2*pi, length.out = 15))
    )
    for(j in 1:k){
        if(j %% 2 == 1){
            squigs <- rbind(squigs, cbind(
                (2*j)*r + r*cos(seq(pi, 0, length.out = 30)),
                d2-r + r*sin(seq(pi, 0, length.out = 30))
            ))
        }else{
            squigs <- rbind(squigs, cbind(
                (2*j)*r + r*cos(seq(pi, 2*pi, length.out = 30)),
                r + r*sin(seq(pi, 2*pi, length.out = 30))
            ))
        }
    }
    if(k %% 2 == 0){
        squigs <- rbind(squigs, cbind(
            (2*(k+1))*r + r*cos(seq(pi, pi/2, length.out = 15)),
            d2/2-r + r*sin(seq(pi, pi/2, length.out = 15))
        ))
    }else{
        squigs <- rbind(squigs, cbind(
            (2*(k+1))*r + r*cos(seq(pi, 1.5*pi, length.out = 15)),
            d2/2+r + r*sin(seq(pi, 1.5*pi, length.out = 15))
        ))
    }
    squigs <- t(t(squigs) - c(d/2, d2/2))
    squigs[,2] <- squigs[,2] * sample(c(-1,1),1)
}

phi <- -atan2(s2[2]-s1[2], s2[1]-s1[1])
R <- matrix(c(cos(phi), sin(phi), -sin(phi), cos(phi)), 2)
squigs <- t(t(squigs %*% R) + (s1+s2)/2)

s$x <- c(s$x[1:(ii-1)], squigs[,1], s$x[(ii+(n.drop+1)):30])
s$y <- c(s$y[1:(ii-1)], squigs[,2], s$y[(ii+(n.drop+1)):30])

plot(c(0,10),c(0,10),col='white', asp=1)
lines(s)
