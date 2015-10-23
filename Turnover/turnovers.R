TurnoversAway <- function(x,y)
{
	turnover <- rep(0,nrow(y))
	badpass <- rep(0,nrow(y))
	traveling <- rep(0,nrow(y))
	match <- data.frame(y,turnover,badpass,traveling)

	D <- subset(x[c("a1","a2","a3","a4","a5","period","etype","player","reason")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="turnover")
		{
			j <- which(as.character(match[,1])==D[i,8])
			turnover[j]=turnover[j]+1
			match[j,3]=turnover[j]
		}
		if (D[i,9]=="bad pass")
		{
			j <- which(as.character(match[,1])==D[i,8])
			badpass[j]=badpass[j]+1
			match[j,4]=badpass[j]
		}
		if (D[i,9]=="traveling")
		{
			j <- which(as.character(match[,1])==D[i,8])
			traveling[j]=traveling[j]+1
			match[j,5]=traveling[j]
		}
	}
	match
}