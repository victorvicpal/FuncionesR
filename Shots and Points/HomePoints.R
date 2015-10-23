HomePoints <- function(x,y)
{
	points <- rep(0,nrow(y))

	match <- data.frame(y,points)

	D <- subset(x[c("h1","h2","h3","h4","h5","period","etype","player","result","points")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot"&&D[i,9]=="made")
		{
			j <- which(as.character(match[,1])==D[i,8])
			points[j]=points[j]+D[i,10]
			match[j,3]=points[j]
		}
	}
match
}