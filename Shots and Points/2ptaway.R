Away2pts <- function(x,y)
{
twoptsmade <- rep(0,nrow(y))
twoptstempted <- rep(0,nrow(y))
FGtwo <- rep(0,nrow(y))
match <- data.frame(y,twoptsmade,twoptstempted,FGtwo)

E <- subset(x[c("a1","a2","a3","a4","a5","period","etype","player","result","type")])
D <- subset.data.frame(E,E[,6]==1||E[,6]==2)

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot"&&D[i,10]!="3pt")
		{
			j <- which(as.character(match[,1])==D[i,8])
			if (D[i,9]=="made")
			{
				twoptstempted[j]=twoptstempted[j]+1
				twoptsmade[j]=twoptsmade[j]+1
				FGtwo[j]=(twoptsmade[j]/twoptstempted[j])*100
				match[j,3]=twoptsmade[j]
				match[j,4]=twoptstempted[j]
				match[j,5]=FGtwo[j]
			}
			else
			{
				twoptstempted[j]=twoptstempted[j]+1
				FGtwo[j]=(twoptsmade[j]/twoptstempted[j])*100
				match[j,4]=twoptstempted[j]
				match[j,5]=FGtwo[j]
			}
		}
	}
match
}