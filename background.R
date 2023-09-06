# draw 10000 of something

# draw each polygon four times, alpha = .25, minor purturbations

source('tesselate/utils.R')

# set initial color
h.init <- .18 # .33, .5
p.init <- .5

x <- repelled_points(3000, rw = c(0,4,0,3))
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,4,0,3))
dd$h <- rep(NA, nrow(x))
dd$p <- rep(NA, nrow(x))
dd$mutn <- rep(FALSE, nrow(x))



ord <- order((x[,1]-2)^2+(x[,2]-1.5)^2)



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
if(any(dd$p > .95) | any(dd$p < .4)){
    dd$p <- 0.4 + 0.55*dd$p
}

which.border <- unique(as.numeric(as.matrix(dd$dirsgs[dd$dirsgs$bp1 | dd$dirsgs$bp2, c('ind1','ind2')])))
dd$p[which.border] <- 0


png('~/Desktop/bg2.png', height = 3000, width = 4000, res=150)
par(mar=c(.0,.0,.0,.0))

plot(c(0,4),c(0,3),asp=1, col='white', axes=FALSE, xlab='',ylab='')
#rect(0,0,1,1, col=hsv(.74, .4, .25))
#segments(dd$dirsgs$x1, dd$dirsgs$y1, dd$dirsgs$x2, dd$dirsgs$y2)

for(i in 1:nrow(x)){
    if(!is.na(dd$h[i])){
        #print(paste0('h = ',dd$h[i], ',  p = ',dd$p[i]))
        for(o2f in 1:4){
            brd <- border(i,dd)
            brd <- brd + runif(2*nrow(brd), min = -.004, max = .004) # .008 for dark one
            polygon(brd, col = alpha(hsv(dd$h[i], dd$p[i], sqrt(1-dd$p[i]^2)), alpha = .2),
                    border = NA)
        }
    }
}

dev.off()


