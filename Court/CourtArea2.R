CourtArea2 <- function(M)
{
	#Ymax for each x
	xu <- unique(M$x)
	n <- length(xu)
	yu <- rep(0,n)
	Mu <- data.frame(xu,yu)

	for (i in 1:n)
	{
		Mu$yu[i] <- max(M$y[M$x==Mu$xu[i]])
	}

	#Left Matrix
	ymaxl <- max(Mu$yu[1:2])
	ymax <- max(Mu$yu)
	n <- which.max(Mu$yu)-1
	ML <- Mu[1:n,]

	for (i in 2:n)
	{
		if (ML$yu[i]>ymaxl) 
		{
			ymaxl <- ML$yu[i]
		}
		ML[i,] <- ML[which(ML$yu==ymaxl),]
	}
	ML <- ML[-ML$x!=0,]
	xu <- unique(ML$xu)
	yu <- unique(ML$yu)
	ML <- data.frame(xu,yu)

	#Right Matrix
	m <- length(Mu$yu)
	MR <- Mu[m:n+1,]
	MR <- MR[complete.cases(MR),]
	n <- length(MR$yu)
	ymaxl <- max(MR$yu[1:2])

	for (i in 1:n)
		{
			if (MR$yu[i]>ymaxl) 
			{
				ymaxl <- MR$yu[i]
			}
			MR[i,] <- MR[which(MR$yu==ymaxl),]
		}
	MR
	xu <- unique(MR$xu)
	yu <- unique(MR$yu)
	MR <- data.frame(xu,yu)
	n <- length(MR$xu)
	MR <- MR[n:1,]
	p0 <- c(ML[1,1],4.25)
	pn <- c(MR[n,1],4.25)
	names(p0) <- c('xu','yu')
	names(pn) <- names(pn)

	M <- rbind.data.frame(p0,ML,MR,pn)

}