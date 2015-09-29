FreeThrowsAway <- function(x,y)
{
	FTM <- rep(0,nrow(y))
	FTT <- rep(0,nrow(y))
	FTP  <- rep(0,nrow(y))
	match <- data.frame(y,FTM,FTT,FTP)

	D <- subset(x[c("a1","a2","a3","a4","a5","period","etype","player","result")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="free throw")
		{
			j <- which(as.character(match[,1])==D[i,8])
			FTT[j]=FTT[j]+1
			match[j,4]=FTT[j]

			if (D[i,9]=="made")
			{
				FTM[j]=FTM[j]+1
				match[j,3]=FTM[j]
			}

			FTP[j]=(FTM[j]/FTT[j])*100
			match[j,5]=FTP[j]
		}
	}
match
}