
# more abstract version
source('~/Projects/tesselate/utils.R')
require(pracma)

x <- repelled_points(1000)
sz <- (rexp(1000, 2.1) + .3) / 3
bt <- (runif(1000) + sz) / 2
bt[bt > 1] <- 1


plot(0:1,0:1, col='white',asp=1,axes=FALSE)
rect(0,0,1,1, col = hsv(.65, .8, .25))
points(x, pch=16, cex = sz, col = hsv(.18, .04, .98, bt))

# random rectangles for ground?


road <- function(){
    angle <- runif(2, pi, 2*pi)
    angle <- angle[which.max(abs(angle-3*pi/2))]
    anchor <- c(NA,NA)
    while(any(is.na(anchor))){
        xy <- runif(2, .25,.75)
        if(xy[2] > .5) xy[2] <- 1-xy[2]
        if(pracma::Norm(xy - c(.5,.5)) < .1){
            anchor <- xy
        }
    }
    w <- min(runif(2, 0,.05)) + abs(sin(angle)/50)
    bigV <- c(cos(angle), sin(angle))
    bigV <- bigV / min(abs(bigV))
    smallV <- w * c(cos(angle+pi/2), sin(angle+pi/2)) / 2
    shape <- rbind(anchor + smallV, anchor - smallV, 
                   anchor - smallV + bigV, anchor + smallV + bigV)
    rc <- hsv(runif(1, .5,.75), 
              runif(1, .2,.4), 
              runif(1, .15,.4))
    lc <- hsv(runif(1, .18,.22), 
              runif(1, 0,.5), runif(1, .9,1), 
              runif(1, .1,1))
    
    d <- min(sample(25:200,2))
    nlights <- as.integer(d*pracma::Norm(bigV))
    
    l.xy <- cbind(anchor[1]+smallV[1] + bigV[1]*(1:nlights-1)/(nlights-1),
                  anchor[2]+smallV[2] + bigV[2]*(1:nlights-1)/(nlights-1))
    l.xy <- rbind(l.xy, cbind(anchor[1]-smallV[1] + bigV[1]*(1:nlights-1)/(nlights-1),
                              anchor[2]-smallV[2] + bigV[2]*(1:nlights-1)/(nlights-1)))
    l.xy <- l.xy[l.xy[,1] > -.05 & l.xy[,2] > -.05 & l.xy[,1] < 1.05 & l.xy[,2] < 1.05, ]
    
    polygon(shape, col=rc, border = NA)
    points(l.xy, col = lc, cex = runif(1,.3,.8))
    
    #if(wide and right angle) # add two-directions of cars
    if(w > .035 & abs(angle-3*pi/2)<pi/3){
        d <- sample(50:200,1)
        nlights <- as.integer(d*pracma::Norm(bigV))
        v <- runif(1, .9,1)
        alph <- runif(1, .5,.9)
        sz <- runif(1,.2,.5)
        
        l.xy <- cbind(anchor[1]+smallV[1]/3 + bigV[1]*(1:nlights-1)/(nlights-1),
                      anchor[2]+smallV[2]/3 + bigV[2]*(1:nlights-1)/(nlights-1))
        l.xy <- l.xy[l.xy[,1] > -.05 & l.xy[,2] > -.05 & l.xy[,1] < 1.05 & l.xy[,2] < 1.05, ]
        lc.red <- hsv(runif(1, 0,.1), runif(1, .5,1), v, alph)
        points(l.xy, col = lc.red, cex = sz)
        
        
        l.xy <- cbind(anchor[1]-smallV[1]/3 + bigV[1]*(1:nlights-1)/(nlights-1),
                      anchor[2]-smallV[2]/3 + bigV[2]*(1:nlights-1)/(nlights-1))
        l.xy <- l.xy[l.xy[,1] > -.05 & l.xy[,2] > -.05 & l.xy[,1] < 1.05 & l.xy[,2] < 1.05, ]
        lc <- hsv(runif(1, .18,.22), runif(1, 0,.35), v, alph)
        points(l.xy, col = lc, cex = sz*1.25)
    }
}











# random parrallelograms (vertical sides) = buildings
building <- function(){
    angle <- runif(2, 0, pi)
    angle <- angle[which.max(abs(angle - pi/2))]
    x <- runif(1, .35,.65)
    y <- mean(runif(2, .1,.45))
    h <- runif(1, .1,.5)
    w <- runif(1, .05,.2)
    v <- c(w*cos(angle), w*sin(angle))
    c <- as.integer(70*w)
    r <- as.integer(50*h)
    cc <- hsv(runif(1, .5,.75), 
              runif(1, .7,1), 
              runif(1, .15,.4))
    shape <- rbind(c(x,y),
                   c(x+v[1], y+v[2]), 
                   c(x+v[1], y+h+v[2]), 
                   c(x, y+h))
    
    lp <- t(sapply(1:r, function(rr){
        p.on.floor <- runif(1, .2,.9)
        rbinom(c, 1,p.on.floor)
    }))
    rc <- cbind(rep(1:c, each = r), rep(1:r, times=c))[which(lp==1), ]
    wind.xy <- cbind(x + v[1]*rc[,1]/(c+1),
                     y + v[2]*rc[,1]/(c+1) + h*rc[,2]/(r+1))
    
    ptype <- 15
    if(angle > pi/6 & angle < 5*pi/6) ptype <- 18
    
    polygon(shape, col=cc, border=NA)
    points(wind.xy, pch = ptype, cex = .6,
           col = hsv(runif(1, .18,.22), 
                     runif(1, 0,.3), runif(1, .9,1), 
                     runif(1, .5,1)))
}
