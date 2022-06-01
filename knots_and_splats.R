
n <- 5000
X <- cbind(runif(1,min=1,max=10)*cos(scaleAB(1:n, b=2*pi)), runif(1,min=1,max=10)*sin(scaleAB(1:n, b=2*pi)))

require(uwot)
u <- umap(X)
v <- Rtsne::Rtsne(X)

plot(u, asp=1, col = colorby(1:n))
plot(v$Y, asp=1, col = colorby(1:n))

d <- sqrt(diff(v$Y[,1])^2 + diff(v$Y[,2])^2 )
break.idx <- which(d > 10*mean(d)) # mean is already pull to the right

curves <- lapply(1:length(break.idx), function(i){
    bi <- break.idx[i]
    if(i == 1){
        prev <- 0
    }else{
        prev <- break.idx[i-1]
    }
    v$Y[(prev+1):bi,]
})
curves[[1]] <- rbind(v$Y[(break.idx[length(break.idx)]+1):n,],
                     curves[[1]])



# find smallest square containing all points

chull <- v$Y[chull(v$Y),]

plot(range(v$Y[,1]) + c(-10,10), range(v$Y[,2]) + c(-10,10), asp=1)
#polygon(chull, col = 2, lwd=40, border = 2)
for(crv in curves){
    lines(crv, lwd=30, col=2)
}
for(crv in curves){
    lines(crv, lwd=8)
}



