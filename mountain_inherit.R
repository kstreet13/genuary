# mountains?
# Gwen's request

source('tesselate/utils.R')

xmax <- 5
ymax <- 3

# set color for left side, then let it run across
brown <- hsv(.12, .7, .5)
blue <- hsv(.6, .9, .95)
# colvec <- c('brown4','skyblue3')

x <- repelled_points(700, rw = c(0,xmax,0,ymax))
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,xmax,0,ymax))
dd$mountain <- rep(NA, nrow(x))
dd$h <- rep(NA, nrow(x))
dd$s <- rep(NA, nrow(x))
dd$v <- rep(NA, nrow(x))
dd$mutn <- rep(FALSE, nrow(x))

plot(c(0,xmax),c(0,ymax),asp=1, col='white')
#points(x)
rect(0,0,xmax,ymax)
segments(dd$dirsgs$x1, dd$dirsgs$y1, dd$dirsgs$x2, dd$dirsgs$y2)

leftind <- dd$dirsgs[dd$dirsgs$thirdv1 == -2 | dd$dirsgs$thirdv2 == -2, c('ind1','ind2')]
leftind <- unique(as.numeric(as.matrix(leftind)))
leftcoly <- dd$summary$y[leftind]
leftind <- leftind[order(leftcoly)]
leftcoly <- sort(leftcoly)

# set initial conditions of left column
for(li in 1:length(leftind)){
    i <- leftind[li]
    dd$mountain[i] <- ifelse(leftcoly[li] > .65*ymax, FALSE,TRUE)
}
# run the inheritance for "mountain"
for(i in order(dd$summary$x)){
    if(is.na(dd$mountain[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=.01)
        neds <- neds[!is.na(dd$mountain[neds])]
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v) * abs(cos(atan2(v[1],v[2])))
        })
        dd$mountain[i] <- sample(dd$mountain[neds],1, prob = blen)
    }
}

# # show left column
# for(i in leftind){
#     polygon(border(i,dd), col = c('blue','brown')[1+dd$mountain[i]])
# }
# # show all
# for(i in 1:nrow(x)){
#     polygon(border(i,dd), col = c('blue','brown')[1+dd$mountain[i]])
# }

# set initial mountain color
first <- which.min(dd$summary$x + xmax*(!dd$mountain))
dd$h[first] <- .12
dd$s[first] <- .4
dd$v[first] <- .3

# variation
standard.vary <- function(h,s,v){
    h1 <- h + rnorm(1, sd = .005)
    s1 <- s + rnorm(1, sd = .02)
    if(s1 < 0) s1 <- 0
    if(s1 > 1) s1 <- 1
    v1 <- v + rnorm(1, sd = .02)
    if(v1 < 0) v1 <- 0
    if(v1 > .6) v1 <- .6
    return(c(h1,s1,v1))
}
mutation.vary <- function(h,s,v){
    h1 <- h + rnorm(1, sd = .005)
    s1 <- s + rnorm(1, sd = .1)
    if(s1 < 0) s1 <- 0
    if(s1 > 1) s1 <- 1
    v1 <- v + rnorm(1, sd = .1)
    if(v1 < 0) v1 <- 0
    if(v1 > .6) v1 <- .6
    return(c(h1,s1,v1))
}

# run inheritance for mountain region colors
lemmegetback2ya <- NULL
for(i in which(dd$mountain)[order(dd$summary$x[dd$mountain])]){
    if(is.na(dd$h[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=0)
        neds <- neds[!is.na(dd$h[neds])]
        if(length(neds) == 0){
            # skip for now
            lemmegetback2ya <- c(lemmegetback2ya, i)
            next 
        }
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v)
        })
        # inherit from this neighbor
        inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
        # possible mutation
        if(runif(1) < .05){
            c1 <- mutation.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
            dd$mutn[i] <- TRUE
        }else{
            c1 <- standard.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
        }
        dd$h[i] <- c1[1]
        dd$s[i] <- c1[2]
        dd$v[i] <- c1[3]
                
        for(j in rev(lemmegetback2ya)){
            neds <- adjacent(j, dd, return.inds = TRUE, min=0)
            neds <- neds[!is.na(dd$h[neds])]
            if(length(neds) == 0){
                next
            }
            blen <- sapply(neds, function(id){
                bID <- which(apply(dd$dirsgs,1,function(ds){
                    all(ds[5:6] %in% c(id,j))
                }))
                v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
                pracma::Norm(v)
            })
            # inherit from this neighbor
            inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
            # possible mutation
            if(runif(1) < .05){
                c1 <- mutation.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
                dd$mutn[j] <- TRUE
            }else{
                c1 <- standard.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
            }
            dd$h[j] <- c1[1]
            dd$s[j] <- c1[2]
            dd$v[j] <- c1[3]
            
            lemmegetback2ya <- lemmegetback2ya[-which(lemmegetback2ya == j)]
        }
    }
}


# show all
plot(c(0,xmax),c(0,ymax),asp=1, col='white')
#points(x)
#rect(0,0,xmax,ymax, col = blue)
blues <- colorby(0:1000, colors = c('white','white',blue))
for(i in 0:1000){
    lines(x=c(0,xmax), y=c(ymax*i/1000,ymax*i/1000), lwd=2, col = blues[i])
}

for(i in 1:nrow(x)){
    if(dd$mountain[i]){
        #polygon(border(i,dd), col = hsv(dd$h[i], dd$s[i], dd$v[i]))
        for(o2f in 1:6){
            brd <- border(i,dd)
            brd <- brd + runif(2*nrow(brd), min = -.008, max = .008) # .008 for dark one
            polygon(brd, col = alpha(hsv(dd$h[i], dd$s[i], dd$v[i]), alpha = .35),
                    border = NA)
        }
    }
}
rect(0,0,xmax,ymax, col = alpha(blue,alpha=.35))








##############################################
# round two: midground mountains # 22222222222222222222
##############################################
# reset
x <- repelled_points(600, rw = c(0,xmax,0,ymax))
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,xmax,0,ymax))
dd$mountain <- rep(NA, nrow(x))
dd$h <- rep(NA, nrow(x))
dd$s <- rep(NA, nrow(x))
dd$v <- rep(NA, nrow(x))
dd$mutn <- rep(FALSE, nrow(x))

leftind <- dd$dirsgs[dd$dirsgs$thirdv1 == -2 | dd$dirsgs$thirdv2 == -2, c('ind1','ind2')]
leftind <- unique(as.numeric(as.matrix(leftind)))
leftcoly <- dd$summary$y[leftind]
leftind <- leftind[order(leftcoly)]
leftcoly <- sort(leftcoly)

# set initial conditions of left column
for(li in 1:length(leftind)){
    i <- leftind[li]
    dd$mountain[i] <- ifelse(leftcoly[li] > .5*ymax, FALSE,TRUE)
}
# run the inheritance for "mountain"
for(i in order(dd$summary$x)){
    if(is.na(dd$mountain[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=.01)
        neds <- neds[!is.na(dd$mountain[neds])]
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v) * abs(cos(atan2(v[1],v[2])))
        })
        dd$mountain[i] <- sample(dd$mountain[neds],1, prob = blen)
    }
}

# set initial mountain color
first <- which.min(dd$summary$x + xmax*(!dd$mountain))
dd$h[first] <- .12
dd$s[first] <- .6
dd$v[first] <- .4

# run inheritance for mountain region colors
lemmegetback2ya <- NULL
for(i in which(dd$mountain)[order(dd$summary$x[dd$mountain])]){
    if(is.na(dd$h[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=0)
        neds <- neds[!is.na(dd$h[neds])]
        if(length(neds) == 0){
            # skip for now
            lemmegetback2ya <- c(lemmegetback2ya, i)
            next 
        }
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v)
        })
        # inherit from this neighbor
        inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
        # possible mutation
        if(runif(1) < .05){
            c1 <- mutation.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
            dd$mutn[i] <- TRUE
        }else{
            c1 <- standard.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
        }
        dd$h[i] <- c1[1]
        dd$s[i] <- c1[2]
        dd$v[i] <- c1[3]
        
        for(j in rev(lemmegetback2ya)){
            neds <- adjacent(j, dd, return.inds = TRUE, min=0)
            neds <- neds[!is.na(dd$h[neds])]
            if(length(neds) == 0){
                next
            }
            blen <- sapply(neds, function(id){
                bID <- which(apply(dd$dirsgs,1,function(ds){
                    all(ds[5:6] %in% c(id,j))
                }))
                v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
                pracma::Norm(v)
            })
            # inherit from this neighbor
            inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
            # possible mutation
            if(runif(1) < .05){
                c1 <- mutation.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
                dd$mutn[j] <- TRUE
            }else{
                c1 <- standard.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
            }
            dd$h[j] <- c1[1]
            dd$s[j] <- c1[2]
            dd$v[j] <- c1[3]
            
            lemmegetback2ya <- lemmegetback2ya[-which(lemmegetback2ya == j)]
        }
    }
}


# add strip of mountains
for(i in 1:nrow(x)){
    if(dd$mountain[i]){
        #polygon(border(i,dd), col = hsv(dd$h[i], dd$s[i], dd$v[i]))
        for(o2f in 1:4){
            brd <- border(i,dd)
            brd <- brd + runif(2*nrow(brd), min = -.004, max = .004) # .008 for dark one
            polygon(brd, col = alpha(hsv(dd$h[i], dd$s[i], dd$v[i]), alpha = .4),
                    border = NA)
        }
    }
}
rect(0,0,xmax,ymax, col = alpha(blue,alpha=.35))



##############################################
# round three: forground mountains # 333333333333333333
##############################################
# reset
x <- repelled_points(400, rw = c(0,xmax,0,ymax))
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,xmax,0,ymax))
dd$mountain <- rep(NA, nrow(x))
dd$h <- rep(NA, nrow(x))
dd$s <- rep(NA, nrow(x))
dd$v <- rep(NA, nrow(x))
dd$mutn <- rep(FALSE, nrow(x))

leftind <- dd$dirsgs[dd$dirsgs$thirdv1 == -2 | dd$dirsgs$thirdv2 == -2, c('ind1','ind2')]
leftind <- unique(as.numeric(as.matrix(leftind)))
leftcoly <- dd$summary$y[leftind]
leftind <- leftind[order(leftcoly)]
leftcoly <- sort(leftcoly)

# set initial conditions of left column
for(li in 1:length(leftind)){
    i <- leftind[li]
    dd$mountain[i] <- ifelse(leftcoly[li] > .35*ymax, FALSE,TRUE)
}
# run the inheritance for "mountain"
for(i in order(dd$summary$x)){
    if(is.na(dd$mountain[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=.01)
        neds <- neds[!is.na(dd$mountain[neds])]
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v) * abs(cos(atan2(v[1],v[2])))
        })
        dd$mountain[i] <- sample(dd$mountain[neds],1, prob = blen)
    }
}

# set initial mountain color
first <- which.min(dd$summary$x + xmax*(!dd$mountain))
dd$h[first] <- .2
dd$s[first] <- .7
dd$v[first] <- .4

# run inheritance for mountain region colors
lemmegetback2ya <- NULL
for(i in which(dd$mountain)[order(dd$summary$x[dd$mountain])]){
    if(is.na(dd$h[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=0)
        neds <- neds[!is.na(dd$h[neds])]
        if(length(neds) == 0){
            # skip for now
            lemmegetback2ya <- c(lemmegetback2ya, i)
            next 
        }
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v)
        })
        # inherit from this neighbor
        inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
        # possible mutation
        if(runif(1) < .05){
            c1 <- mutation.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
            dd$mutn[i] <- TRUE
        }else{
            c1 <- standard.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
        }
        dd$h[i] <- c1[1]
        dd$s[i] <- c1[2]
        dd$v[i] <- c1[3]
        
        for(j in rev(lemmegetback2ya)){
            neds <- adjacent(j, dd, return.inds = TRUE, min=0)
            neds <- neds[!is.na(dd$h[neds])]
            if(length(neds) == 0){
                next
            }
            blen <- sapply(neds, function(id){
                bID <- which(apply(dd$dirsgs,1,function(ds){
                    all(ds[5:6] %in% c(id,j))
                }))
                v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
                pracma::Norm(v)
            })
            # inherit from this neighbor
            inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
            # possible mutation
            if(runif(1) < .05){
                c1 <- mutation.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
                dd$mutn[j] <- TRUE
            }else{
                c1 <- standard.vary(dd$h[inherit.from],dd$s[inherit.from],dd$v[inherit.from])
            }
            dd$h[j] <- c1[1]
            dd$s[j] <- c1[2]
            dd$v[j] <- c1[3]
            
            lemmegetback2ya <- lemmegetback2ya[-which(lemmegetback2ya == j)]
        }
    }
}


# add strip of mountains
for(i in 1:nrow(x)){
    if(dd$mountain[i]){
        polygon(border(i,dd), col = hsv(dd$h[i], dd$s[i], dd$v[i]), 
                border=hsv(dd$h[i], dd$s[i], dd$v[i]))
    }
}




# jagged border
# which.border <- unique(as.numeric(as.matrix(dd$dirsgs[dd$dirsgs$bp1 | dd$dirsgs$bp2, c('ind1','ind2')])))
# dd$p[which.border] <- 0

