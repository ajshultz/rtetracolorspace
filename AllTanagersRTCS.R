source("/Users/allisonshultz/Desktop/Rtetracolorspace/RtcsFunctions.R")
load("bluetitss.dat")

files <- list.files("/Users/allisonshultz/Desktop/RefDataPythonResults/RefDataSpeciesSummary")

spesplit <- strsplit(files,split="_")
spe <- vector()
for (i in 1:length(spesplit)){
	spe[i]<-spesplit[[i]][1]
	}

patchsumm <- matrix(nrow=length(files),ncol=10)
for (i in 1:length(files)){
	refs <- read.csv(paste("/Users/allisonshultz/Desktop/RefDataPythonResults/RefDataSpeciesSummary/",files[i],sep=""))
	summ <- summaryOfpatches(refs,ss)
	patchsumm[i,]<-summ
	print(files[i])
	}
rownames(patchsumm)<-spe
colnames(patchsumm)<-c("AvgSpan","VarSpan","MaxSpan","Volume","AvgHueDisp","VarHueDisp","MaxHueDisp","AvgBrill","AvgChroma","AvgAchChroma")

write.csv(patchsumm, file="RTCS_PatchSum.csv")
