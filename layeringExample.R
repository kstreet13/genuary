
# layers


png('~/Desktop/1.png', width = 500, height = 500, res = 150)
old.par <- par(bg=NA, mar = c(0,0,0,0))
plot(1)
dev.off()

png('~/Desktop/2.png', width = 500, height = 500, res = 150)
old.par <- par(bg=NA, mar = c(0,0,0,0))
plot(1:2, col='white')
dev.off()


require(png)
one <- readPNG('~/Desktop/1.png')
two <- readPNG('~/Desktop/2.png')


plot(1:10, col='white')
rect(-9999,-9999,9999,9999, col=hsv(.74, .6, .3))
rasterImage(one, 1,1,10,10)
rasterImage(two, 1,1,10,10)


# reset background
par(old.par)
