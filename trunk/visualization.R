drawTcs <- function(bg.color="white", line.color="black", label.color=line.color, label.offset=.08, label.family = par3d("family"), label.font = par3d("font"), label.cex = par3d("cex"), label.useFreeType=par3d("useFreeType"), view.theta=180, view.phi=90, view.fov=50){
	tcsVertices <- diag(4)
	cartVertices <- cartCoord(tcsVertices)	
	rgl.open()
	for (i in 1:4){
		for(j in 1:4){
			lines3d(cartVertices[c(i,j),1],cartVertices[c(i,j),2],cartVertices[c(i,j),3],color=line.color)
		}
	}
	rgl.bg(color=bg.color)
	rgl.texts(cartVertices[1,]*(label.offset+1),text="u",color=label.color,adj=0.5, family=label.family, font=label.font, cex=label.cex, useFreeType= label.useFreeType)
	rgl.texts(cartVertices[2,]*(label.offset+1),text="s",color=label.color,adj=0.5, family=label.family, font=label.font, cex=label.cex, useFreeType= label.useFreeType)
	rgl.texts(cartVertices[3,]*(label.offset+1),text="m",color=label.color,adj=0.5, family=label.family, font=label.font, cex=label.cex, useFreeType= label.useFreeType)
	rgl.texts(cartVertices[4,]*(label.offset+1),text="l",color=label.color,adj=0.5, family=label.family, font=label.font, cex=label.cex, useFreeType= label.useFreeType)
	
	view3d(theta=view.theta,phi=view.phi,fov=view.fov)

	
	
}

#The type can be point, sphere, or cube.  If anything else is provided, it will be treated as a shape3d object, or list thereof, and plotted using shade3d and shapelist3d, scaled according to cube.size/2.
plotPoints <- function(coordinates, point.color="black", type="sphere", sphere.radius=.02, cube.size=.04, labels=NULL, label.color="black", label.offset=ifelse(type=="sphere",4*sphere.radius,2*cube.size)*c(-1,0,0), label.adj = 0.5, label.family = par3d("family"), label.font = par3d("font"), label.cex = 0.7*par3d("cex"), label.useFreeType=par3d("useFreeType")){
	if(type=="point"){
		points3d(coordinates[,1],coordinates[,2],coordinates[,3],color=point.color)
	}else if(type=="sphere"){
		spheres3d(coordinates[,1],coordinates[,2],coordinates[,3],color=point.color,radius=sphere.radius)
	}else if(type=="cube"){
		shape <- cube3d()
		shade3d(shapelist3d(shape,x=coordinates[,1],y=coordinates[,2],z=coordinates[,3],size=cube.size/2, color=point.color, specular=point.color))
	}else{
		shade3d(shapelist3d(type,x=coordinates[,1],y=coordinates[,2],z=coordinates[,3],size=cube.size/2, color=point.color, specular=point.color))
	}
	
	#Label the points if labels were given
	if(!is.null(labels)){
		rgl.texts(coordinates[,1]+label.offset[1],coordinates[,2]+label.offset[2],coordinates[,3]+label.offset[3], color=label.color, text=labels, adj=label.adj, family=label.family, font=label.font, cex=label.cex, useFreeType= label.useFreeType)
	}
	
	
	
}

connectPoints <- function(points1, points2, line.color="black", line.width=1){
	for(i in 1:(dim(points1)[1])){
		lines3d(c(points1[i,1],points2[i,1]), c(points1[i,2],points2[i,2]), c(points1[i,3],points2[i,3]),color=line.color,lwd=line.width)
	}
}