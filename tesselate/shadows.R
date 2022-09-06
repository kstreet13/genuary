
# make a shadow of a shape

x <- repelled_points(10)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))

poly <- round.corners(border(1,dd), .03)

plot(0:1,0:1, col='white',asp=1,axes=FALSE)
polygon(poly)

# think of it like a calligraphy pen
# hold it at an angle, trace every line

r <- .06
angle <- 3*pi/4

v <- c(cos(angle)*r, sin(angle)*r)
shifted <- t(t(poly) + v)

for(i in 1:nrow(poly)){
    ip1 <- i+1
    if(ip1 > nrow(poly)) ip1 <- 1
    shadow <- rbind(poly[i,], shifted[i,], shifted[ip1,], poly[ip1,])
    polygon(shadow, col=2, border = NA)
}

polygon(poly, col='white')





