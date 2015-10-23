Home2pts <- function(x,y)
{
twoptsm <- rep(0,nrow(y))
twoptst <- rep(0,nrow(y))
FGtwo <- rep(0,nrow(y))
match <- data.frame(y,twoptsm,twoptst,FGtwo)

E <- subset(x[c("h1","h2","h3","h4","h5","period","etype","player","result","type")])
D <- subset.data.frame(E,E[,6]==1)

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot"&&D[i,9]!="3pt")
		{
			j <- which(as.character(match[,1])==D[i,8])
			if (D[i,10]=="made")
			{
				twoptst[j]=twoptst[j]+1
				twoptsm[j]=twoptsm[j]+1
				FGtwo[j]=(twoptsm[j]/twoptst[j])*100
				match[j,3]=twoptsm[j]
				match[j,4]=twoptst[j]
				match[j,5]=FGtwo[j]
			}
			else
			{
				twoptst[j]=twoptst[j]+1
				FGtwo[j]=(twoptsm[j]/twoptst[j])*100
				match[j,4]=twoptst[j]
				match[j,5]=FGtwo[j]
			}
		}
	}
match
}