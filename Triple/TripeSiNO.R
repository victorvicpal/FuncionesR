triple.function <- function(A)
{
	tri <- rep(0,nrow(A))

	for (i in 1:nrow(A))
	{
		if (A$x[i]<3 || A$x[i]>47 || ((A$x[i]<=47 || A$x[i]>=3) & A$y[i]>sqrt(23.7^2-(A$x[i]-25)^2)+5.25))
		tri[i] <- 1
		else
		tri[i] <- 0
	}
	tri
}