StealsHome <- function(x,y)
{
	steals <- rep(0,nrow(y))
	match <- data.frame(y,steals)

	D <- subset(x[c("h1","h2","h3","h4","h5","period","etype","steal")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="turnover"&&D[i,8]!="")
		{
			j <- which(as.character(match[,1])==D[i,8])
			steals[j]=steals[j]+1
			match[j,3]=steals[j]
		}
	}
match
}