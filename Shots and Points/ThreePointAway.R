ThreePointAway <- function(x,y)
{
	threepm <- rep(0,nrow(y))
	threept <- rep(0,nrow(y))
	threepp <- rep(0,nrow(y))
	match <- data.frame(y,threept,threepm,threepp)

	D <- subset(x[c("a1","a2","a3","a4","a5","period","etype","player","result","type")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot"&&D[i,10]=="3pt")
		{
			j <- which(as.character(match[,1])==D[i,8])
			threept[j]=threept[j]+1
			match[j,3]=threept[j]

			if (D[i,9]=="made")
			{
				threepm[j]=threepm[j]+1
				match[j,4]=threepm[j]
			}

			threepp[j]=(threepm[j]/threept[j])*100
			match[j,5]=threepp[j]
		}
	}
match
}