rebound <- function(x,y)
{
	ryd <- rep(0,nrow(y))
	ryo <- rep(0,nrow(y))
	ryt <- rep(0,nrow(y))
	match <- data.frame(y,ryd,ryo,ryt)

	for (i in 1:nrow(x))
	{
		if (x[i,14]=="rebound")
		{
			type <- x[i,30]
			j <- which(as.character(match[,1])==x[i,24])
			if (type=="def")
				{
					match[j,3] <- ryd[j]+1
					match[j,5] <- ryt[j]+1
					ryd[j]=ryd[j]+1
					ryt[j]=ryt[j]+1
				}
			else 	{
						match[j,4] <- ryo[j]+1
						match[j,5] <- ryt[j]+1
						ryo[j]=ryo[j]+1
						ryt[j]=ryt[j]+1
					}
		}
	}
match
}