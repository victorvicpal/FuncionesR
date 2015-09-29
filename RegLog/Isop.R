Isop <- function(p,coef)
{
	require('rootSolve')
	y <- seq(4,28,0.2)
	Ap <- matrix(0,length(y),4)
	Ap <- as.data.frame(Ap)
	colnames(Ap) <- c('pr','x1','x2','y')
	Ap$pr <- p
	for (i in 1:length(y))
	{
		f <- function(x) coef$x*x+coef$`I(x^2)`*x^2+coef$`x:y`*y[i]*x+coef$`(Intercept)`+y[i]*coef$y+y[i]^2*coef$`I(y^2)`-log(p)+log(1-p)
		maximo <- optimize(f, interval=c(0, 50), maximum=TRUE)
		if (maximo$objective>0)
		{
			All <- uniroot.all(f, c(0, 50))
			if (length(All)>1)
			{
			Ap[i,2] <- All[1]
			Ap[i,3] <- All[2]
			Ap[i,4] <- y[i]
			}
			else
			{
			Ap[i,2] <- All[1]
			Ap[i,3] <- 0.000001
			Ap[i,4] <- y[i]
			}
		}
	}
row_sub <- apply(Ap, 1, function(row) all(row !=0 ))
na.omit(Ap[row_sub,])

Ap1 <- cbind.data.frame(Ap$pr,Ap$x1,Ap$y)
Ap2 <- cbind.data.frame(Ap$pr,Ap$x2,Ap$y)

names(Ap1) <- c('pr','x','y')
names(Ap2) <- names(Ap1)

Ap <- rbind.data.frame(Ap1,Ap2)
row_sub <- apply(Ap, 1, function(row) all(row !=0 ))
Ap[row_sub,]
}