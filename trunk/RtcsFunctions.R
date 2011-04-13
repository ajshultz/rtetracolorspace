#A function for trapezoid rule integration.
trapz <- function(x,y){
	idx = 2:length(x)
    return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
	}

#Calculate the relative stimulation of the cone types inputted.  R should be data frame of reflectance spectra, C should be the cone sensitivity curves.
stim <- function(R,C){
	
	wlR <- R[,1]
	wlC <- C[,1]
	
	#Interpolation of R based on the wavelengths of C
	RapproxC <- apply(X=R[-1], MARGIN=2, FUN = function(xcol){approx(x=wlR,y=xcol,xout=wlC,method="constant")$y})
#	CapproxR <- apply(X=C[-1], MARGIN=2, FUN = function(xcol){approx(x=wlC,y=xcol,xout=wlR,method="constant")$y})

	result <- matrix(nrow=ncol(RapproxC),ncol=ncol(C)-1)

	rownames(result)<-colnames(R[-1])
	colnames(result)<-c(colnames(C[-1]))
	
	for (i in 1:ncol(RapproxC)) {
		for (j in 2:ncol(C)){
			prod <- RapproxC[,i]*C[,j]
			result[i,j-1]<-trapz(wlC,prod)
			}
		}

#	result <- matrix(nrow=ncol(R)-1,ncol=ncol(C)-1)
#	R <- R[apply(CapproxR,1,FUN=function(row){!(any(is.na(row)))}),]
#	CapproxR <- CapproxR[apply(CapproxR,1,FUN=function(row){!(any(is.na(row)))}),]
#
#	wlR <- R[,1]
#
#	rownames(result)<-colnames(R[-1])
#	colnames(result)<-c(colnames(C[-1]))
#	for (i in 2:ncol(R)) {
#		for (j in 1:ncol(CapproxR)){
#			prod <- R[,i]*CapproxR[,j]
#			result[i-1,j]<-trapz(wlR,prod)
#			}
#		}
	result <- result/rowSums(result)
	
	#As in tetracolorspace refelctance spectra with a normalized brilliance less than 0.05 are set in the achromatic center.
	brill <- normBrill(R)
	for (i in 1:nrow(result)){
		if (brill[i] < 0.05){
			result[i,]<- c(0.25,0.25,0.25,0.25)
			}
		}
	return(result)
	}

#Transform cone stimulation values (in a tetrachromat) into Cartesian coordinates.	
cartCoord <- function(stimvals){
	
	X <- function(s,m,u){
		((1-2*s-m-u)/2)*sqrt(3/2)
		}
	Y <- function(m,u){
		(-1+3*m+u)/(2*sqrt(2))
		}
	Z <- function(u){
		u-(1/4)
		}
	
	result <- matrix(nrow=nrow(stimvals),ncol=3)
	rownames(result)<-rownames(stimvals)
	colnames(result)<-c("X","Y","Z")
	for (i in 1:nrow(stimvals)){
		x <- X(s=stimvals[i,2],m=stimvals[i,3],u=stimvals[i,1])
		y <- Y(m=stimvals[i,3],u=stimvals[i,1])
		z <- Z(u=stimvals[i,1])
		result[i,]<-c(x,y,z)
		}
	return(result)
	}

#Translate Cartesian coordinates into sphereical coordinates, providing hue-theta,hue-phi, and r, or chroma.
sphereCoord <- function(cartcoor){
	
	phi <- function(X,Y,Z){
		return(atan2(Z,sqrt(X^2+Y^2)))}
		#return((pi/2)-(acos(Z/(sqrt(X^2+Y^2+Z^2)))))}
		
	theta <- function(Y,X){
		return(atan2(Y,X))}
		
	r <- function(X,Y,Z){
		return(sqrt(X^2+Y^2+Z^2))}
		
	result <- matrix(nrow=nrow(cartcoor),ncol=3)
	rownames(result)<-rownames(cartcoor)
	colnames(result)<-c("theta","phi","r")
		
	for (i in 1:nrow(cartcoor)){
		phival <- phi(cartcoor[i,1],cartcoor[i,2],cartcoor[i,3])
		if (is.nan(phival) == TRUE) phival <- 0
		thetaval <- theta(cartcoor[i,2],cartcoor[i,1])
		rval <- r(cartcoor[i,1],cartcoor[i,2],cartcoor[i,3])
		result[i,] <- c(thetaval,phival,rval)
		}
	return(result)
	}

#A measure of the angle between two color vectors.
alphaFun <- function(theta1,theta2,phi1,phi2){
	cosangle <- cos(phi1)*cos(phi2)*cos(theta1 - theta2)+sin(phi1)*sin(phi2)
	cosangle <- max(-1,min(1,cosangle))
	return(acos(cosangle))
		}

#The maximum possible acheived chroma for a given angle.  The input is a matrix or dataframe of spherical coordinates.
rMax <- function(refsphere){
	vertstim <- diag(4)
	rownames(vertstim) <- colnames(vertstim)<- c("u","s","m","l")
	vertcart <- cartCoord(vertstim)
	vertsphere <- sphereCoord(vertcart)
	
	alphaMax <- vector()
	for (i in 1:nrow(refsphere)){
		alphau <- alphaFun(refsphere[i,1],vertsphere[1,1],refsphere[i,2],vertsphere[1,2])
		alphas <- alphaFun(refsphere[i,1],vertsphere[2,1],refsphere[i,2],vertsphere[2,2])
		alpham <- alphaFun(refsphere[i,1],vertsphere[3,1],refsphere[i,2],vertsphere[3,2])
		alphal <- alphaFun(refsphere[i,1],vertsphere[4,1],refsphere[i,2],vertsphere[4,2])
		alphaMax[i] <- max(c(alphau,alphas,alpham,alphal))
		}
	names(alphaMax)<-rownames(refsphere)
	rmax <- vector()
	for (i in 1:length(alphaMax)){
		if (refsphere[i,3]==0) {
			rmax[i] <- 0
			}
		else rmax[i] <- .25/cos(pi-alphaMax[i])
		}
	
	names(rmax) <- rownames(refsphere)
	return(rmax)
	}

#The acheived chroma for each given patch, given the set of sphereical coordinates and rmax for each hue.	
acheivedR <- function(refsphere,rmax){
	ar <- vector()
	for (i in 1:length(rmax)){
		if (rmax[i]==0) ar[i] <- 0
		else ar[i] <- refsphere[i,3]/rmax[i]
		}
	names(ar)<-names(rmax)
	return(ar)
	}

#Normalized brilliance for a set of spectra calculated between the wavelengths of 300 and 700 nm.  	
normBrill <- function(refs){
	oo <- refs[1]>=300 & refs[1]<=700
	trimmed <- refs[oo,]
	min <- trimmed[1,1]
	max <- trimmed[nrow(trimmed),1]

	brill <- vector()
	for (i in 2:ncol(trimmed)){
		brill[i-1] <- trapz(trimmed[,1],trimmed[,i])/(max-min)
		}
	names(brill)<-colnames(refs)[-1]
	brill <- brill/100
	return(brill)
	}
	
#The voluem of the minimum convex hull (polygon), calculated from a set of Cartesian coordinates.
colorVolume <- function(refcart){
	require(geometry)
	#Jason's idea
	a <- sum(svd(refcart,nu=0,nv=0)$d>1e-6)
	poly <- list()
	if (a >= 3){
	poly <- convhulln(refcart, option = "FA")
		}
	else poly$vol <- 0
	return(poly$vol)	
	}

#Produces a matrix of Euclidean distances between all pairs of a dataframe of Cartesian coordinates from reflectance spectra.  
colorSpan <- function(refcart){
	
	euclidean <- function(cartvec1,cartvec2){
		edist <- sqrt((cartvec1[1]-cartvec2[1])^2+(cartvec1[2]-cartvec2[2])^2+(cartvec1[3]-cartvec2[3])^2)
		return(edist)
		}
	
	result <- matrix(nrow=nrow(refcart),ncol=nrow(refcart))
	for (i in 1:nrow(refcart)){
		for (j in 1:nrow(refcart)){
			result[i,j] <- euclidean(refcart[i,],refcart[j,])
			}
		}
	rownames(result) <- rownames(refcart)
	colnames(result) <- rownames(refcart)
	return(result)
	}

colorSpanSingle <- function(cartvec1,cartvec2){
		edist <- sqrt((cartvec1[1]-cartvec2[1])^2+(cartvec1[2]-cartvec2[2])^2+(cartvec1[3]-cartvec2[3])^2)
		return(edist)
}
colorSpanBi <- function(refcart1,refcart2,matchOnly=FALSE){
	
	euclidean <- function(cartvec1,cartvec2){
		edist <- sqrt((cartvec1[1]-cartvec2[1])^2+(cartvec1[2]-cartvec2[2])^2+(cartvec1[3]-cartvec2[3])^2)
		return(edist)
		}
	
	result <- matrix(nrow=nrow(refcart1),ncol=nrow(refcart2))

	for (i in 1:nrow(refcart1)){
		for (j in 1:nrow(refcart2)){
			result[i,j] <- euclidean(refcart1[i,],refcart2[j,])
		}
	}
	
	rownames(result) <- rownames(refcart1)
	colnames(result) <- rownames(refcart2)
	return(result)	
}	
	


#Given a matrix of color spans, this produces a summary, the mean, variance, and maximum span.
summary.colorSpan <- function(colorspan){
	
	lowertri <- colorspan[lower.tri(colorspan)]
	colorspan <- lowertri[lowertri>0]
	
	mean <- mean(colorspan)
	var <- sd(colorspan)^2
	max <- max(colorspan)
	
	summary <- c(mean,var,max)
	names(summary) <- c("mean","var","max")
	
	return(summary)
	}
	
#Given a dataframe or matrix of sphereical coordinates from reflectance spectra, this produces a matrix of the angle between each of the color vectors
hueDisp <- function(refsphere){
	result <- matrix(nrow=nrow(refsphere),ncol=nrow(refsphere))
	for (i in 1:nrow(refsphere)){
		for (j in 1:nrow(refsphere)){
			result[i,j] <- alphaFun(refsphere[i,1],refsphere[j,1],refsphere[i,2],refsphere[j,2])
			}
		}
	rownames(result) <- rownames(refsphere)
	colnames(result) <- rownames(refsphere)		
	return(result)
	}

#Given a matrix of hue disparities, this finds the mean, variance, and maximum disparity.
summary.hueDisp <- function(huedisp){
	lowertri <- huedisp[lower.tri(huedisp)]
	hue <- lowertri[lowertri>0]
	
	mean <- mean(hue)
	var <- sd(hue)^2
	max <- max(hue)
	
	summary <- c(mean,var,max)
	names(summary) <- c("mean","var","max")
	
	return(summary)
	}

avgChroma <- function(spherecoord){
	mean(spherecoord[,3])
	}

avgAcheivedChroma <- function(acheivedchroma){
	mean(acheivedchroma)
	}

avgBrill <- function(brill){
	mean(brill)
	}

#Summary function to return a dataframe of hue, chroma (r and acheived R), and brilliance given a set of reflectance spectra and spectral sensitivities (C).
tristimulus <- function(refs, C){
	refstims <- stim(refs,C)
	refcart <- cartCoord(refstims)
	refsphere <- sphereCoord(refcart)
	rmax <- rMax(refsphere)
	acheivedr <- acheivedR(refsphere,rmax)
	normbrill <- normBrill(refs)
	
	tri <- data.frame(refsphere,acheivedr,normbrill)
	return(tri)
	}

#Summary function to give whole-patch measurements, color span avg, variance, and max, color volume, hue disparity avg, variance, and max, average brilliance, average chroma, and average acheived chroma.
summaryOfpatches <- function(refs,C){
	refstims <- stim(refs,C)
	refcart <- cartCoord(refstims)
	refsphere <- sphereCoord(refcart)
	rmax <- rMax(refsphere)
	acheivedr <- acheivedR(refsphere,rmax)
	normbrill <- normBrill(refs)
	vol <- colorVolume(refcart)
	disp <- hueDisp(refsphere)
	disp.summary <- summary.hueDisp(disp)
	spans <- colorSpan(refcart)
	spans.summary <- summary.colorSpan(spans)
	avgchroma <- avgChroma(refsphere)
	avgachchroma <- avgAcheivedChroma(acheivedr)
	avgbrill <- avgBrill(normbrill)	
	avgchroma <- avgChroma(refsphere)
		
	summ <- c(spans.summary,vol,disp.summary,avgbrill,avgchroma,avgachchroma)
	names(summ) <- c("AvgSpan","VarSpan","MaxSpan","Volume","AvgHueDisp","VarHueDisp","MaxHueDisp","AvgBrill","AvgChroma","AvgAchChroma")
	
	return(summ)}