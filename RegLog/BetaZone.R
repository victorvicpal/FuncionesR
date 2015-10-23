BetaZone <- function(player,season)
{
	#SQL <- paste("select player, x ,y , if(result='made',1,0) as result, if(x<3 or x>47,1,if(y>(sqrt(pow(23.71,2)-Pow((x-25),2))+5.25),1,0)) as triple,if(y<=10 and x>=17 and x<=33,1,0) as zone1,if(y<=19 and y>10 and x>=17 and x<=33,1,0) as zone2,if(x>=17 and x<=33 and y>19 and (y>=((13.75/(-8))*(x-25)+5.25)) and (y>=((13.75/8)*(x-25)+5.25)) and y<=(sqrt(pow(23.71,2)-Pow((x-25),2))+5.25),1,0) as zone3,if(x<17 and x>=3 and (y<((13.75/(-8))*(x-25)+5.25)) and y<=(sqrt(pow(23.71,2)-Pow((x-25),2))+5.25),1,0) as zone4,if(x>33 and x<=47 and (y<((13.75/8)*(x-25)+5.25)) and y<=(sqrt(pow(23.71,2)-Pow((x-25),2))+5.25),1,0) as zone5 from nba.play_by_play where etype='shot' and y>3 and game_id in (select id from nba.game where season='",season,"') and player='",player,"';", sep="")
	SQL <- paste("select player, x ,y , if(result='made',1,0) as result, if(x<3 or x>47,1,if(y>(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0)) as triple,if(y<=10 and x>=17 and x<=33,1,0) as zone1,if(y<=19 and y>10 and x>=17 and x<=33,1,0) as zone2,if(y>19 and (y>=((13.75/(-8))*(x-25)+5.25)) and (y>=((13.75/8)*(x-25)+5.25)) and y<=(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0) as zone3,if(x<17 and x>=3 and (y<((13.75/(-8))*(x-25)+5.25)) and y<=(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0) as zone4,if(x>33 and x<=47 and (y<((13.75/8)*(x-25)+5.25)) and y<=(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0) as zone5 from nba.play_by_play where etype='shot' and y>3 and game_id in (select id from nba.game where season='",season,"') and player='",player,"';", sep="")
	M <- dbGetQuery(con,SQL)
	require(logistf)
	triplem <- mean(M[,5])
	M[,5:10] <- apply(M[,5:10],2,factor)

	#Define x and y
    x <- seq(0,50,0.1)
    y <- seq(4,40,0.1)
    
    #Grid xy
    A <- expand.grid(x,y)
    names(A) <- c('x','y')
    
    x <- A$x
    y <- A$y

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

	if (triplem>0.1)
	{
		output <- logistf(result~(x+y+x*y+I(x^2)+I(y^2))*(1+triple+zone1+zone2+zone3+zone4+zone5),data=M)
		coef <- as.data.frame(t(output$coefficients))
		prob0 <- coef$`(Intercept)`+coef$x*x+coef$y*y+coef$`x:y`*x*y+coef$`I(x^2)`*x^2+coef$`I(y^2)`*y^2
		probtr <- triple*(coef$triple+coef$`x:triple`*x+coef$`y:triple`*y+coef$`x:y:triple`*x*y+coef$`I(x^2):triple`*x^2+coef$`I(y^2):triple`*y^2)
		probz1 <- zone1*(coef$zone1+coef$`x:zone1`*x+coef$`y:zone1`*y+coef$`x:y:zone1`*x*y+coef$`I(x^2):zone1`*x^2+coef$`I(y^2):zone1`*y^2)
		probz2 <- zone2*(coef$zone2+coef$`x:zone2`*x+coef$`y:zone2`*y+coef$`x:y:zone2`*x*y+coef$`I(x^2):zone2`*x^2+coef$`I(y^2):zone2`*y^2)
		probz3 <- zone3*(coef$zone3+coef$`x:zone3`*x+coef$`y:zone3`*y+coef$`x:y:zone3`*x*y+coef$`I(x^2):zone3`*x^2+coef$`I(y^2):zone3`*y^2)
		probz4 <- zone4*(coef$zone4+coef$`x:zone4`*x+coef$`y:zone4`*y+coef$`x:y:zone4`*x*y+coef$`I(x^2):zone4`*x^2+coef$`I(y^2):zone4`*y^2)
		probz5 <- zone5*(coef$zone5+coef$`x:zone5`*x+coef$`y:zone5`*y+coef$`x:y:zone5`*x*y+coef$`I(x^2):zone5`*x^2+coef$`I(y^2):zone5`*y^2)
		prob <- prob0+probtr+probz1+probz2+probz3+probz4+probz5
		A$p <- exp(prob)/(1+exp(prob))
		A <- A[-which(A$y>sqrt(32^2-(A$x-25)^2)+5.25),]
	}
	else
	{
		output <- logistf(result~(x+y+x*y+I(x^2)+I(y^2))*(1+zone1+zone2+zone3+zone4+zone5),data=M)
		coef <- as.data.frame(t(output$coefficients))
		prob0 <- coef$`(Intercept)`+coef$x*x+coef$y*y+coef$`x:y`*x*y+coef$`I(x^2)`*x^2+coef$`I(y^2)`*y^2
		probz1 <- zone1*(coef$zone11+coef$`x:zone11`*x+coef$`y:zone11`*y+coef$`x:y:zone11`*x*y+coef$`I(x^2):zone11`*x^2+coef$`I(y^2):zone11`*y^2)
		probz2 <- zone2*(coef$zone21+coef$`x:zone21`*x+coef$`y:zone21`*y+coef$`x:y:zone21`*x*y+coef$`I(x^2):zone21`*x^2+coef$`I(y^2):zone21`*y^2)
		probz3 <- zone3*(coef$zone31+coef$`x:zone31`*x+coef$`y:zone31`*y+coef$`x:y:zone31`*x*y+coef$`I(x^2):zone31`*x^2+coef$`I(y^2):zone31`*y^2)
		probz4 <- zone4*(coef$zone41+coef$`x:zone41`*x+coef$`y:zone41`*y+coef$`x:y:zone41`*x*y+coef$`I(x^2):zone41`*x^2+coef$`I(y^2):zone41`*y^2)
		probz5 <- zone5*(coef$zone51+coef$`x:zone51`*x+coef$`y:zone51`*y+coef$`x:y:zone51`*x*y+coef$`I(x^2):zone51`*x^2+coef$`I(y^2):zone51`*y^2)
		prob <- prob0+probz1+probz2+probz3+probz4+probz5
		A$p <- exp(prob)/(1+exp(prob))
		A <- A[-which((A$y>sqrt(23.71^2-(A$x-25)^2)+5.25)|(A$x<3)|(A$x>47)),]
	}
	
A	

}