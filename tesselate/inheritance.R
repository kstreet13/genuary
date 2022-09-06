source('utils.R')

# inheritance

# set color for top row, then let it run down
topcol <- c('cadetblue3','cornsilk1','dimgray')
topcol <- rep(c('lavenderblush4','cornsilk1','cadetblue3','cornsilk1','lavenderblush4'), times=c(3,2,4,2,3))
#topcol <- brewer.pal(9,'Set1')[c(1,5,6,3,2,4)]

x <- repelled_points(400)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
dd$color <- rep(NA, nrow(x))

plot(0:1,0:1,asp=1, col='white')
#points(x)
rect(0,0,1,1)
segments(dd$dirsgs$x1, dd$dirsgs$y1, dd$dirsgs$x2, dd$dirsgs$y2)

topind <- which(dd$dirsgs$thirdv1 == -3 | dd$dirsgs$thirdv2 == -3)
toprow <- unique(c(dd$dirsgs$ind1[topind], dd$dirsgs$ind2[topind]))
toprowx <- dd$summary$x[toprow]
toprow <- toprow[order(toprowx)]
toprowx <- sort(toprowx)
rm(topind)

breakpoints <- c(0,(1:length(topcol))/length(topcol))
for(ti in 1:length(toprow)){
    i <- toprow[ti]
    dd$color[i] <- topcol[max(which(toprowx[ti] > breakpoints))]
}

for(i in 1:length(toprow)){
    polygon(border(toprow[i],dd), col = dd$color[toprow[i]])
}

for(i in order(dd$summary$y, decreasing = TRUE)){
    if(is.na(dd$color[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=.01)
        neds <- neds[!is.na(dd$color[neds])]
        # border length
        blen <- sapply(neds, function(id){
            bID <- which(apply(dd$dirsgs,1,function(ds){
                all(ds[5:6] %in% c(id,i))
            }))
            v <- as.numeric(dd$dirsgs[bID,3:4]-dd$dirsgs[bID,1:2])
            pracma::Norm(v) * abs(cos(atan2(v[2],v[1])))
        })
        dd$color[i] <- sample(dd$color[neds],1, prob = blen)
    }
}

for(i in 1:nrow(x)){
    polygon(border(i,dd), col = dd$color[i])
}

################################################################################
################################################################################
################################################################################
################################################################################
# start from single point

source('utils.R')

# set color for one polygon, then let it seep out
colorspace <- brewer.pal(9,'Set1')[c(1,5,6,3,2,4)]
initcol <- 4


x <- repelled_points(500)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
dd$color <- rep(NA, nrow(x))
dd$mutn <- rep(FALSE, nrow(x))

plot(0:1,0:1,asp=1, col='white')
#points(x)
rect(0,0,1,1)
segments(dd$dirsgs$x1, dd$dirsgs$y1, dd$dirsgs$x2, dd$dirsgs$y2)


topleftind <- which.min(x[,1]^2 + (1-x[,2])^2)
dd$color[topleftind] <- initcol

ord <- order(x[,1]^2 + (1-x[,2])^2)
lemmegetback2ya <- NULL


for(i in ord){
    if(is.na(dd$color[i])){
        neds <- adjacent(i, dd, return.inds = TRUE, min=.002)
        neds <- neds[!is.na(dd$color[neds])]
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
        # with no mutation
        dd$color[i] <- as.numeric(sample(as.character(dd$color[neds]),1, prob = blen)) # bitten by sample()'s "convenience" again
        # possible mutation
        if(runif(1) < .05){
            dd$mutn[i] <- TRUE
            dd$color[i] <- dd$color[i] + 1 * sign(rnorm(1))
            if(dd$color[i] < 1){
                dd$color[i] <- 6
            }
            if(dd$color[i] > 6){
                dd$color[i] <- 1
            }
        }
        
        for(j in lemmegetback2ya){
            neds <- adjacent(j, dd, return.inds = TRUE, min=.01)
            neds <- neds[!is.na(dd$color[neds])]
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
            # with no mutation
            dd$color[j] <- as.numeric(sample(as.character(dd$color[neds]),1, prob = blen))
            # possible mutation
            if(runif(1) < .05){
                dd$mutn[j] <- TRUE
                dd$color[j] <- dd$color[j] + 1 * sign(rnorm(1))
                if(dd$color[j] < 1){
                    dd$color[j] <- 6
                }
                if(dd$color[j] > 6){
                    dd$color[j] <- 1
                }
            }
            lemmegetback2ya <- lemmegetback2ya[-which(lemmegetback2ya == j)]
        }
    }
}

for(i in 1:nrow(x)){
    if(!is.na(dd$color[i])){
        polygon(border(i,dd), col = colorspace[dd$color[i]])
    }
}
points(x[dd$mutn,], cex=.5)


# explore colors
x.seq <- seq(0,1, length.out = 20)
p.seq <- seq(0,1, length.out = 10)

plot(c(1,20),c(1,10),col='white')

for(i in 1:length(p.seq)){
    p <- 0.15 + 0.7*p.seq[i]
    points(1:20, rep(11-i, 20), cex=3, col = hsv(x.seq, p, 1-p^2))
}


# (h)sv space
plot(0:1,0:1, col='white', asp=1)
for(ii in x.seq){
    for(jj in x.seq){
        points(ii,jj, col = hsv(.33, s=ii, v=jj), pch=15, cex=3)
    }
}
curve(1-x^2, from=.15, to=.85, add=TRUE)
curve(sqrt(1-x^2), from=.15, to=.95, add=TRUE,lwd=3)
curve((1-x^3)^(1/3), from=.15, to=.85, add=TRUE,lwd=3)

################################################################################
################################################################################
################################################################################
################################################################################
# mural

source('tesselate/utils.R')

# set initial color
h.init <- .5 # .33
p.init <- .5

x <- repelled_points(1000, rw = c(0,10,0,1))
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,10,0,1))
dd$h <- rep(NA, nrow(x))
dd$p <- rep(NA, nrow(x))
dd$mutn <- rep(FALSE, nrow(x))

plot(c(0,10),0:1,asp=1, col='white')
rect(0,0,10,1)
#segments(dd$dirsgs$x1, dd$dirsgs$y1, dd$dirsgs$x2, dd$dirsgs$y2)

ord <- order(x[,1])

dd$h[ord[1]] <- h.init
dd$p[ord[1]] <- p.init
dd$mutn[ord[1]] <- TRUE

lemmegetback2ya <- NULL

for(i in ord){
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
        # with standard variation
        inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
        dd$h[i] <- dd$h[inherit.from] + rnorm(1, sd = .02)
        dd$p[i] <- dd$p[inherit.from] + rnorm(1, sd = .02)
        if(dd$p[i] < 0) dd$p[i] <- 0
        if(dd$p[i] > 1) dd$p[i] <- 1
        # possible mutation
        if(runif(1) < .05){
            dd$mutn[i] <- TRUE
            dd$h[i] <- dd$h[i] + rnorm(1, sd = .1)
            dd$p[i] <- dd$p[i] + rnorm(1, sd = .1)
            if(dd$p[i] < 0) dd$p[i] <- 0
            if(dd$p[i] > 1) dd$p[i] <- 1
        }

        for(j in lemmegetback2ya){
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
            # with standard variation
            inherit.from <- as.numeric(sample(as.character(neds), 1, prob = blen))
            dd$h[j] <- dd$h[inherit.from] + rnorm(1, sd = .02)
            dd$p[j] <- dd$p[inherit.from] + rnorm(1, sd = .02)
            if(dd$p[j] < 0) dd$p[j] <- 0
            if(dd$p[j] > 1) dd$p[j] <- 1
            # possible mutation
            if(runif(1) < .05){
                dd$mutn[j] <- TRUE
                dd$h[j] <- dd$h[j] + rnorm(1, sd = .1)
                dd$p[j] <- dd$p[j] + rnorm(1, sd = .1)
                if(dd$p[j] < 0) dd$p[j] <- 0
                if(dd$p[j] > 1) dd$p[j] <- 1
            }
            
            lemmegetback2ya <- lemmegetback2ya[-which(lemmegetback2ya == j)]
        }
    }
}


dd$h <- dd$h %% 1
if(any(dd$p > .95) | any(dd$p < .15)){
    dd$p <- 0.15 + 0.8*dd$p
}
for(i in 1:nrow(x)){
    if(!is.na(dd$h[i])){
        #print(paste0('h = ',dd$h[i], ',  p = ',dd$p[i]))
        polygon(border(i,dd), col = hsv(dd$h[i], dd$p[i], sqrt(1-dd$p[i]^2)),
                border = NA)
    }
}

#points(x[dd$mutn,], cex=.25)
for(i in 1:nrow(x)){
    if(dd$mutn[i]){
        polygon(border(i,dd))
    }
}




