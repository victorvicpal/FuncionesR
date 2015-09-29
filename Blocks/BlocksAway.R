BlocksAway <- function(x,y)
{
	blocks <- rep(0,nrow(y))
	blocksag <- rep(0,nrow(y))
	match <- data.frame(y,blocks,blocksag)

	D <- subset(x[c("a1","a2","a3","a4","a5","period","etype","block","player")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot"&&D[i,8]!="")
		{
			j <- which(as.character(match[,1])==D[i,8])

			blocks[j]=blocks[j]+1
			match[j,3]=blocks[j]

			k <- which(as.character(match[,1])==D[i,9])

			blocksag[k]=blocksag[k]+1
			match[k,4]=blocksag[k]
		}
	}
	match
}