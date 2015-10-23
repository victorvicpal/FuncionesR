AwayShots <- function(x)
{

	teams <- toString(x[1,13])

	for (i in 1:nrow(x))
	if (x[i,13]!="OFF")
	teams <- unique(c(teams,toString(x[i,13])))

	E <- subset(x[c("team","etype","x","y","result")])
	G <- subset(E,E[,2]=="shot")
	D <- subset(G,G[,1]==teams[1])

	F <- subset(D[,3:5])

	for (i in 1:nrow(D))
	{
		if (D[i,4]>47)
			{
				F[i,2] <- 47-(D[i,4]-47)
				F[i,1] <- 50-D[i,3]
				F[i,3] <- D[i,5]
			}
		else
			{
				F[i,2] <- D[i,4]
				F[i,1] <- D[i,3]
				F[i,3] <- D[i,5]
			}
	}

#Parte correspondiente a la visualización de datos

dup <- duplicated(F[,1:2])

for (i in 1:nrow(F))
	{
		if (dup[i]==TRUE)
		{
			F[i,1]=F[i,1]+runif(1,0,0.7)
			F[i,2]=F[i,2]+runif(1,0,0.7)
		}
	}

#Parte correspondiente a la visualización de datos

d <- c("x","y","score")
colnames(F) <- d
F
}