

# plaid pattern?
plot(0:1,0:1, col='white', asp=1)
# order by color
rect(0,.4, 1,.6, col=alpha('white', alpha=.6), border = NA) # white, horizontal
rect(.4,0, .6,1, col=alpha('white', alpha=.6), border = NA) # white vertical
rect(0,.2, 1,.4, col=alpha('black', alpha=.6), border = NA) # black horizontal
rect(0,.6, 1,.8, col=alpha('black', alpha=.6), border = NA) # black horizontal
rect(.2,0, .4,1, col=alpha('black', alpha=.6), border = NA) # black vertical
rect(.6,0, .8,1, col=alpha('black', alpha=.6), border = NA) # black vertical
rect(0,0, 1,.2, col=alpha('darkred', alpha=.6), border = NA) # red horizontal
rect(0,.8, 1,1, col=alpha('darkred', alpha=.6), border = NA) # red horizontal
rect(0,0, .2,1, col=alpha('darkred', alpha=.6), border = NA) # red vertical
rect(.8,0, 1,1, col=alpha('darkred', alpha=.6), border = NA) # red vertical



mat <- matrix(c('darkred','gray10','gray93','gray10','darkred'), 5,5)
for(j in 1:5){
    columncolor <- c('darkred','gray10','gray93','gray10','darkred')[j]
    mat[,j] <- sapply(mat[,j],function(x){
        colorRampPalette(c(x,columncolor))(3)[2]
    })
}
image <- as.raster(mat)
rasterImage(image, 0,0, 1,1, interpolate = FALSE, angle=45)



horizontalpattern <- rep(c('darkred','gray10','gray93','gray10'), 4)
verticalpattern <- rep(c('darkred','gray10','gray93','gray10'), 4)

mat <- matrix(horizontalpattern, nrow=length(horizontalpattern),ncol=length(verticalpattern))


for(j in 1:ncol(mat)){
    mat[,j] <- sapply(mat[,j],function(x){
        colorRampPalette(c(x,verticalpattern[j]))(3)[2]
    })
}
image <- as.raster(mat)
rasterImage(image, 0,0, 1,1, interpolate = FALSE)



horizontalpattern <- rep(c('darkred','gray10','gray93','gray10'), times=4, each=10)
verticalpattern <- rep(c('darkred','gray10','gray93','gray10'), times=4, each=10)

mat <- matrix(horizontalpattern, nrow=length(horizontalpattern),ncol=length(verticalpattern))

for(j in 1:ncol(mat)){
    mat[,j] <- sapply(mat[,j],function(x){
        colorRampPalette(c(x,verticalpattern[j]))(3)[2]
    })
}

#accents
mat[45,] <- 'goldenrod'
mat[,45] <- 'goldenrod'
mat[135,] <- 'goldenrod'
mat[,135] <- 'goldenrod'

image <- as.raster(mat)
rasterImage(image, 0,0, 1,1, interpolate = FALSE)


# draw over everything except poly

notpoly <- rbind(
    c(0,0),
    c(0,1),
    c(1,1),
    c(1,0),
    c(0,0),
    poly,
    poly[1,]
)
polygon(poly, col=2)
polygon(notpoly, col=3, border = NA)








# make one "special" (plaid) shape
x <- repelled_points(8)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
special <- 1
{
    horizontalpattern <- rep(c('darkred','gray10','gray93','gray10'), times=4, each=10)
    verticalpattern <- rep(c('darkred','gray10','gray93','gray10'), times=4, each=10)
    mat <- matrix(horizontalpattern, nrow=length(horizontalpattern),ncol=length(verticalpattern))
    for(j in 1:ncol(mat)){
        mat[,j] <- sapply(mat[,j],function(x){
            colorRampPalette(c(x,verticalpattern[j]))(3)[2]
        })
    }
    image <- as.raster(mat)
}

plot(0:1,0:1, col='white', asp=1)

rasterImage(image, .2,0, 1.2,1, interpolate = FALSE, angle=0)

poly <- round.corners(shrink.poly(border(special,dd),r=.01),r=.04)
notpoly <- rbind(
    c(-.05,-.05),
    c(-.05,1.05),
    c(1.05,1.05),
    c(1.05,-.05),
    c(-.05,-.05),
    poly,
    poly[1,]
)
polygon(notpoly, col=colorRampPalette(c('darkslategray','white'))(10)[3], border = NA)

#pal <- sample(rep(c('darkred', 'gray93','gray70'),times=c(3,3,2)))
pal <- c('crap', 'gray93', 'gray70', 'darkred', 'gray70', 'gray93', 'gray93', 'darkred')
for(i in (1:nrow(x))[-special]){
    cc <- pal[i]
    polygon(round.corners(shrink.poly(border(i,dd), r=.01), r=.04), border=NA, col=cc, lwd=0.3)
}
for(i in (1:nrow(x))){
    polygon(round.corners(shrink.poly(border(i,dd), r=.01), r=.04))
}

