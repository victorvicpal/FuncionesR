DrawPlayer3 <- function(player,season,pv=0.01)
{
	load("BetaZone5.Rdata")
	require(ggplot2)

	#Define x and y
    x <- seq(0,50,0.1)
    y <- seq(4,40,0.1)
    
    #Grid xy
    A <- expand.grid(x,y)
    names(A) <- c('x','y')
    
    x <- A$x
    y <- A$y

    #Specify the 'zones'
    triple <- ifelse(x<3 | x>47,1,ifelse(y>(sqrt(23.71^2-(x-25)^2)+5.25),1,0))
    A$triple <- triple
    zone1 <- ifelse(y>=4 & y<=10 & x>=17 & x<=33,1,0)
    A$zone1 <- zone1
    zone2 <- ifelse(y<=19 & y>10 & x>=17 & x<=33,1,0)
    A$zone2 <- zone2
    zone3 <- ifelse(y>19 & (y>=((13.75/(-8))*(x-25)+5.25)) & (y>=((13.75/8)*(x-25)+5.25)) & y<=(sqrt(23.71^2-(x-25)^2)+5.25),1,0)
    A$zone3 <- zone3
    zone4 <- ifelse(x<17 & x>=3 & (y<((13.75/(-8))*(x-25)+5.25)) & y<=(sqrt(23.71^2-(x-25)^2)+5.25),1,0)
    A$zone4 <- zone4
    zone5 <- ifelse(x>33 & x<=47 & (y<((13.75/8)*(x-25)+5.25)) & y<=(sqrt(23.71^2-(x-25)^2)+5.25),1,0)
	A$zone5 <- zone5

	#Default Coefficients
	

	#Coefficients of the player
	coef <- Betazone5(player,season,pv)

}