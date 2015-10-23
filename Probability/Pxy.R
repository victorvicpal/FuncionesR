Pxy <- function(player,season)
{
	require(Coeficientes)
	
	#Coefficients
	coef <- Coeficientes(player,season)

	#Define x and y
	x <- seq(0,50,0.1)
	y <- seq(5.25,40,0.1)

	#Grid xy
	A <- expand.grid(x,y)
	names(A) <- c('x','y')


	x <- A$x
	y <- A$y
	triple <- rep(0,length(x))

	for (i in length(triple)) 
	{
		if(x[i]<3 | x[i]>47)
		{
			triple <- 1
		}
		else
		{
			if (y[i]>sqrt((23.71^2)-(x[i]-25)^2)+5.25)
			{
				triple <- 1
			}
		}
	}

	#Probability
	Prob <- coef$"(Intercept)" + coef$"x"*x + coef$"y"*y + coef$"x:y"*x*y + coef$"I(x^2)"*x^2 + coef$"I(y^2)"*y^2 + triple*(coef$"triple1:x"*x + coef$"triple1:y"*y + coef$"triple1:x:y"*x*y + coef$"triple1:I(x^2)"*x^2 + coef$"triple1:I(y^2)"*y^2)
	p <- exp(Prob)/(1 + exp(Prob))

	C <- cbind.data.frame(x,y,p)
}