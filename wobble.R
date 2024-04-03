
n <- 1000
XX <- cbind(runif(1,min=1,max=10)*cos(scaleAB(1:n, b=2*pi)), runif(1,min=1,max=10)*sin(scaleAB(1:n, b=2*pi)))


XX <- cbind(1:1000, 0)
require(Rtsne)
v <- Rtsne(XX)$Y

require(uwot)
u <- umap(XX)



# wobble and (orthogonal) sub-wobble
wobble <- function(xx, complexity, amplitude){
    y <- rnorm(complexity)
    s <- spline(1:complexity, y, n = n)
    wobble <- XX + cbind(0, amplitude*s$y)
    y <- rnorm(complexity)
    s <- spline(1:complexity, y, n = n)
    wobble <- wobble + cbind(amplitude*s$y,0)
    return(wobble)
}

x1 <- wobble(XX,10,4)
plot(x1, asp=1, type='l')

x2 <- wobble(x1, 20,5)
lines(x2)
x3 <- wobble(x2, 20,5)
lines(x3)
x4 <- wobble(x3, 20,5)
lines(x4)







