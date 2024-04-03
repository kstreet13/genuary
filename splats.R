###
# splats
###
require(uwot)

for(ii in 1:10){

n <- round(runif(1, min=500, max=5000))
XX <- cbind(runif(1,min=1,max=10)*cos(scaleAB(1:n, b=2*pi)), runif(1,min=1,max=10)*sin(scaleAB(1:n, b=2*pi)))


if(n > 2000){
    init <- 'pca'
}else{
    init <- sample(c('pca','agspectral','lvrandom'), 1)
}

nneigh <- rpois(1,15)
u <- umap(XX, n_neighbors = nneigh,
          init = init)
##

# find smallest square containing all points
chull <- u[chull(u),]

randPastel <- function(n = 1){
    h <- runif(n)
    p <- runif(n, min = .15, max = .95)
    hsv(h, p, sqrt(1-p^2))
}



png(filename = paste0('~/Desktop/temp/',ii,'.png'), width = 500, height = 500)

plot(range(u[,1]) + c(-10,10), range(u[,2]) + c(-10,10), asp=1, col='white',
     main = paste(n, init, nneigh))
rect(-9999,-9999,9999,9999, col=hsv(h=.18, .1, .98))
#rect(-9999,-9999,9999,9999, col=randPastel())
#rect(-9999,-9999,9999,9999, col=hsv(.18, .2, .98))

c1 <- randPastel()
polygon(chull, col = c1, lwd=40, border = c1)
c2 <- randPastel()
polygon(u, col=c2, border = c2)

dev.off()

}




# funky background dots
# {
#     x <- cbind(runif(300, min=min(u[,1]), max=max(u[,1])), 
#                runif(300, min=min(u[,2]), max=max(u[,2])))
#     sz <- rnorm(300)^2
#     points(x, col = hsv(.65, .02, .75, .7), cex=sz)
# }
