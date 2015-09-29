awaytimeplayed <- function(x,y)
{
	E <- subset(x[c("a1","a2","a3","a4","a5","period","time")])
	D <- subset.data.frame(E,E[,6]==1)
	C <- unclass(strptime(D[,7], format="%M:%S"))
	sec <- C$sec
	min <- C$min
	t <- rep(0,nrow(y))
	matcht <- data.frame(y,t)
	for (i in 1:nrow(D))
	{
		away <- c(which((as.character(y[,1]))==D[i,1]),which((as.character(y[,1]))==D[i,2]),which((as.character(y[,1]))==D[i,3]),which((as.character(y[,1]))==D[i,4]),which((as.character(y[,1]))==D[i,5]))
		time <- 720 - (sec[i] + min[i]%*%60)
		for (j in 1:length(away))
				matcht[away[j],3]=matcht[away[j],3]+time
	}
matcht
}