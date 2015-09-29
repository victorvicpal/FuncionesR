AssistAway <- function(x,y)
{
	assist <- rep(0,nrow(y))
	match <- data.frame(y,assist)

	D <- subset(x[c("a1","a2","a3","a4","a5","period","etype","assist","reason")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot"&&D[i,8]!="")
		{
			j <- which(as.character(match[,1])==D[i,8])
			assist[j]=assist[j]+1
			match[j,3]=assist[j]
		}
	}
match
}