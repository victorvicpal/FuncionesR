FoulAgainstHome <- function(x,y)
{
	foul <- rep(0,nrow(y))
	technical <- rep(0,nrow(y))
	offensive <- rep(0,nrow(y))
	loose <- rep(0,nrow(y))
	personal <- rep(0,nrow(y))
	shooting <- rep(0,nrow(y))

	match <- data.frame(y,foul,technical,offensive,loose,personal,shooting)

	D <- subset(x[c("h1","h2","h3","h4","h5","period","etype","opponent","player","type")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="foul")
		{
			j <- which(as.character(match[,1])==D[i,8])
			foul[j]=foul[j]+1
			match[j,3]=foul[j]

			if (D[i,10]=="technical foul")
			{
				technical[j]=technical[j]+1
				match[j,4]=technical[j]
			}
			if (D[i,10]=="offensive")
			{
				offensive[j]=offensive[j]+1
				match[j,5]=offensive[j]
			}
			if (D[i,10]=="loose ball")
			{
				loose[j]=loose[j]+1
				match[j,6]=loose[j]
			}
			if (D[i,10]=="personal")
			{
				personal[j]=personal[j]+1
				match[j,7]=personal[j]
			}
			if (D[i,10]=="shooting")
			{
				shooting[j]=shooting[j]+1
				match[j,8]=shooting[j]
			}
		}
	}
match
}