require(pracma) # for distmat
require(matrixStats)
source('~/mystuff.R')

nob <- 5
obstacles <- cbind(runif(nob), runif(nob))
rownames(obstacles) <- 1:nob
nat <- 8
attractions <- cbind(runif(nat), runif(nat))
rownames(attractions) <- LETTERS[1:nat]

plot(0:1,0:1, col='white', asp=1)
rect(0,0,1,1)
points(obstacles, pch = 13, col = 2)
points(attractions, pch = 16, col = 3)

nants <- 10
ants <- cbind(x = runif(nants), y = runif(nants))
#ants <- cbind(rep(attractions[,1], length.out=nants),
#              rep(attractions[,2], length.out=nants))
goal <- sample(nrow(attractions), nants, replace = TRUE)
visited <- matrix(FALSE, nrow = nants, ncol = nrow(attractions))
stepsize <- .01
tails <- array(NA, dim=c(nants,2,8))
tails[,,1] <- ants

worn <- matrix(0, nrow=2/stepsize, ncol=2/stepsize)
idx <- floor(attractions*2/stepsize)+1
for(wi in 1:nat){
    worn[idx[wi,1], idx[wi,2]] <- 9
}

# it's about the slope, not the value
penalty <- function(d){
    exp(-20*d)/4
}
reward <- function(d){
    #1/(1+10*d)
    .5 - d/3 + .5/(2*d+1)^2
}
wornness <- function(times){
    sqrt(times)/1000
}

# track one ant
track <- ants[1,,drop=FALSE]

oldsum <- sum(worn)
for(jj in 1:5000){
    obs.jit <- obstacles + matrix(rnorm(2*nob, sd=.005), ncol=2)
    for(i in 1:nants){
        draw <- TRUE
        # determine next step, `nxt`
        if(all(ants[i,]==attractions[goal[i],])){
            #print('1: at destination')
            visited[i,goal[i]] <- TRUE
            if(all(visited[i,])){
                #print('1b: new ant')
                nxt <- runif(2)
                visited[i,] <- FALSE
                draw <- FALSE
            }else{
                #print('1a: new goal')
                goal[i] <- sample((1:nrow(attractions))[!visited[i,]],1)
                nxt <- ants[i,]
            }
        }else if(sqrt(sum((ants[i,]-attractions[goal[i],])^2)) < stepsize){
            #print('2: close to goal')
            nxt <- attractions[goal[i],]
        }else{
            dirs <- seq(0,2*pi, length.out = 9)[-1] + runif(1, 0, pi)
            spots <- cbind(ants[i,1] + cos(dirs)*stepsize,
                           ants[i,2] + sin(dirs)*stepsize)
            obD <- distmat(spots, obs.jit)
            p <- rowSums(penalty(obD))
            atD <- sqrt(rowSums(t(t(spots) - attractions[goal[i], ])^2))
            r <- reward(atD)
            #
            idx <- floor(spots*2/stepsize)+1
            w.i <- apply(idx,1,function(x){
                if(x[1]>0 && x[2]>0 && x[1]<=nrow(worn) && x[2]<=ncol(worn)){
                    return(worn[x[1],x[2]])
                }else{
                    return(0)
                }
            })
            w <- wornness(w.i)
            #
            nxt <- spots[which.max(r+w - p), ]
        }
        # if it's a continuation, draw it
        if(draw){
            #cc <- c(rgb(0,0,0,.1),rgb(.1,.1,1,.5))[1+(i==1)]
            cc <- rgb(0,0,0,.1)
            lines(c(ants[i,1], nxt[1]), c(ants[i,2],nxt[2]), col=cc)
            stopifnot(sqrt(sum((ants[i,]-nxt)^2)) <= stepsize+.0001)
        }
        # check if stuck, if so, change goal
        md <- max(dist(rbind(nxt, ants[i,], t(tails[i,,]))))
        if(!is.na(md) & md<2*stepsize){
            if(sum(!visited[i,]) > 1){
                newgoal <- sample((1:nrow(attractions))[!visited[i,]],1)
            }else{
                newgoal <- sample((1:nrow(attractions))[visited[i,]],1)
            }
            goal[i] <- newgoal
        }
        # update position
        ants[i,] <- nxt
    }
    # update tails
    tails[,,2:8] <- tails[,,1:7]
    tails[,,1] <- ants
    track <- rbind(track, ants[1,])
    
    #update worn matrix
    idx <- floor(ants*2/stepsize)+1
    for(wi in 1:nants){
        x <- idx[wi,]
        if(x[1]>0 && x[2]>0 && x[1]<=nrow(worn) && x[2]<=ncol(worn)){
            worn[idx[wi,1], idx[wi,2]] <- min(c(9, worn[idx[wi,1], idx[wi,2]]+1))
        }
    }
    stopifnot(sum(worn) - oldsum <= nants)
    oldsum <- sum(worn)
}
#




x = seq(0,sqrt(2), length.out = 100)
plot(x, reward(x), ylim=0:1) # reward
points(x, penalty(x), col=2) # penalty

# derivative of penalty = 
# derivative of reward = 





split.after <- which(sapply(1:(nrow(track)-1), function(i){
    if(sqrt(sum((track[i+1,]-track[i,])^2)) > stepsize*1.01){
        return(TRUE)
    }
    FALSE
}))
split.after <- c(0, split.after, nrow(track))
journeys <- lapply(1:(length(split.after)-1), function(i){
    track[(split.after[i]+1):(split.after[i+1]),]
})



