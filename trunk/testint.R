function(R,C){
	
	Rround <- R
	Rround[[1]] <- floor(R[[1]])
	
	#Bin the R data
	for(i in 2:(ncol(R))){
		for(j in 1:length(C[[1]])){
			Rround[j,i] <- mean(R[Rround[[1]]==c[j,1],i])
		}
	}
	
	result <- t(as.matrix(Rround[-1])) %*% C[-1]
	result <- result/rowSums(result)
	rownames(result) <- colnames(R)
	colnames(result) <- colnames(C)
	
}