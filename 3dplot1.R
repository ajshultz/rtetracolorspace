verts <- diag(4)
verts <- rbind(verts,matrix(c(0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0),ncol=4,byrow=TRUE))
colnames(verts) <- c("u","s","m","l")
rownames(verts) <- c("u","s","m","l")

vertcart <- cartCoord(verts)
scatterplot3d(refcart,xlim=c(-.8,.8),ylim=c(-.8,.8),zlim=c(-.8,.8),angle=45)

s3d <- scatterplot3d(vertcart,angle=45)
s3d$points3d(vertcart,type="b",lty=1)