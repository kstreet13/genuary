
# lightning
# this isn't really what lightning looks like

x <- repelled_points(100)
dd <- deldir(x = x[,1], y = x[,2], rw = c(0,1,0,1))
maze <- dd
maze$parent <- rep(-1, nrow(dd$summary))
maze <- fill_maze(maze)


topind <- which(maze$dirsgs$thirdv1 == -3 | maze$dirsgs$thirdv2 == -3)
toprow <- unique(c(maze$dirsgs$ind1[topind], maze$dirsgs$ind2[topind]))
toprowx <- maze$summary$x[toprow]
top <- toprow[which(toprowx == sort(toprowx)[as.integer(length(toprow)/2)])]

botind <- which(maze$dirsgs$thirdv1 == -1 | maze$dirsgs$thirdv2 == -1)
botrow <- unique(c(maze$dirsgs$ind1[botind], maze$dirsgs$ind2[botind]))
botrowx <- maze$summary$x[botrow]
bot <- botrow[which(botrowx == sort(botrowx)[round(length(botrow)/2)])]

path <- solvemaze(maze, start = top, end = bot)


plot(0:1,0:1,asp=1, col='white')
rect(0,0,1,1)
mazelines(maze)
showpath(maze, path, points=TRUE, col=3, lwd=2)



plot(0:1,0:1,asp=1, col='white')
rect(-0.1,-0.1,1.1,1.1, col = 'darkslateblue')
rect(-0.1,-0.1,1.1,1.1, col = rgb(0,0,0,.4))
showpath(maze, path, col=alpha(brewer.pal(9,'Set3')[2], alpha=.02), lwd=150)
showpath(maze, path, col=alpha(brewer.pal(9,'Set3')[2], alpha=.02), lwd=100)
for(i in seq(50,2,by=-2)){
    showpath(maze, path, col=alpha(brewer.pal(9,'Set3')[2], alpha=.01), lwd=i)
}
showpath(maze, path, col=alpha(brewer.pal(9,'Set3')[2], alpha=.9), lwd=2)






