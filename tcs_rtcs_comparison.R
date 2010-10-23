tcs_m <- read.table("FiveCladesTCSmale_noRT.txt")
rownames(tcs_m)<-paste(rownames(tcs_m),"m",sep="")

tcs_f <- read.table("FiveCladesTCSfemale_noRT.txt")
rownames(tcs_f)<-paste(rownames(tcs_f),"f",sep="")

tcs <- rbind(tcs_m,tcs_f)

rtcs <- read.csv("RTCS_PatchSum.csv",row.names=1)
head(rtcs)

rtcs_m_trim <- rtcs[rownames(tcs),]
nrow(rtcs_m_trim)
nrow(tcs)

head(rtcs_m_trim)
head(tcs)

colnames(tcs)<-c("tcs_AvgSpan","tcs_VarSpan","tcs_MaxSpan","tcs_Volume","tcs_AvgHueDisp","tcs_VarHueDisp","tcs_MaxHueDisp","tcs_AvgBrill","tcs_AvgChroma")

compare <- data.frame(rtcs_m_trim[,1],tcs[,1],rtcs_m_trim[,2],tcs[,2],rtcs_m_trim[,3],tcs[,3],rtcs_m_trim[,4],tcs[,4],rtcs_m_trim[,5],tcs[,5],rtcs_m_trim[,6],tcs[,6],rtcs_m_trim[,7],tcs[,7],rtcs_m_trim[,8],tcs[,8],rtcs_m_trim[,9],tcs[,9])

rownames(compare) <- rownames(tcs)
colnames(compare) <-  c("rtcs_AvgSpan","tcs_AvgSpan","rtcs_VarSpan","tcs_VarSpan","rtcs_MaxSpan","tcs_MaxSpan","rtcs_Volume","tcs_Volume","rtcs_AvgHueDisp","tcs_AvgHueDisp","rtcs_VarHueDisp","tcs_VarHueDisp","rtcs_MaxHueDisp","tcs_MaxHueDisp","rtcs_AvgBrill","tcs_AvgBrill","rtcs_AvgChroma","tcs_AvgChroma")

se <- seq(1,18,2)

layout(matrix(1:9,3,3))
for (i in se){
	plot(compare[,i],compare[,i+1],main=colnames(compare)[i])
	}


rownames(compare)[abs(compare[, 1]-compare[,2])>.01]
