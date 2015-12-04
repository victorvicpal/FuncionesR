Pxy3 <- function(player,season,pv=0.01)
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
    coef <- rep(0,42)
    names(coef) <- c('(Intercept)','x','y','x:y','I(x^2)','I(y^2)','triple1','x:triple1','y:triple1','x:y:triple1','I(x^2):triple1','I(y^2):triple1','zone11','x:zone11','y:zone11','x:y:zone11','I(x^2):zone11','I(y^2):zone11','zone21','x:zone21','y:zone21','x:y:zone21','I(x^2):zone21','I(y^2):zone21','zone31','x:zone31','y:zone31','x:y:zone31','I(x^2):zone31','I(y^2):zone31','zone41','x:zone41','y:zone41','x:y:zone41','I(x^2):zone41','I(y^2):zone41','zone51','x:zone51','y:zone51','x:y:zone51','I(x^2):zone51','I(y^2):zone51')
    
    #Coefficients of the player
    coefp <- BetaZone5(player,season,pv)
    j <- match(names(coefp), names(coef),0)
    coef[j] <- coefp
    coef <- as.data.frame(t(coef))
    
    if (coef$triple1!=0)
    {
        prob0 <- coef$`(Intercept)`+coef$x*x+coef$y*y+coef$`x:y`*x*y+coef$`I(x^2)`*x^2+coef$`I(y^2)`*y^2
        probtr <- triple*(coef$triple1+coef$`x:triple1`*x+coef$`y:triple1`*y+coef$`x:y:triple1`*x*y+coef$`I(x^2):triple1`*x^2+coef$`I(y^2):triple1`*y^2)
        if(coef$zone11!=0){probz1 <- zone1*(coef$zone11+coef$`x:zone11`*x+coef$`y:zone11`*y+coef$`x:y:zone11`*x*y+coef$`I(x^2):zone11`*x^2+coef$`I(y^2):zone11`*y^2)}else{probz1 <- rep(0,length(prob0))}
        if(coef$zone21!=0){probz2 <- zone2*(coef$zone21+coef$`x:zone21`*x+coef$`y:zone21`*y+coef$`x:y:zone21`*x*y+coef$`I(x^2):zone21`*x^2+coef$`I(y^2):zone21`*y^2)}else{probz2 <- rep(0,length(prob0))}
        if(coef$zone31!=0){probz3 <- zone3*(coef$zone31+coef$`x:zone31`*x+coef$`y:zone31`*y+coef$`x:y:zone31`*x*y+coef$`I(x^2):zone31`*x^2+coef$`I(y^2):zone31`*y^2)}else{probz3 <- rep(0,length(prob0))}
        if(coef$zone41!=0){probz4 <- zone4*(coef$zone41+coef$`x:zone41`*x+coef$`y:zone41`*y+coef$`x:y:zone41`*x*y+coef$`I(x^2):zone41`*x^2+coef$`I(y^2):zone41`*y^2)}else{probz4 <- rep(0,length(prob0))}
        if(coef$zone51!=0){probz5 <- zone5*(coef$zone51+coef$`x:zone51`*x+coef$`y:zone51`*y+coef$`x:y:zone51`*x*y+coef$`I(x^2):zone51`*x^2+coef$`I(y^2):zone51`*y^2)}else{probz5 <- rep(0,length(prob0))}
        prob <- prob0+probtr+probz1+probz2+probz3+probz4+probz5
        A$p <- exp(prob)/(1+exp(prob))
        A <- A[-which(A$y>sqrt(32^2-(A$x-25)^2)+5.25),]
    }
    else
    {
        prob0 <- coef$`(Intercept)`+coef$x*x+coef$y*y+coef$`x:y`*x*y+coef$`I(x^2)`*x^2+coef$`I(y^2)`*y^2
        if(coef$zone11!=0){probz1 <- zone1*(coef$zone11+coef$`x:zone11`*x+coef$`y:zone11`*y+coef$`x:y:zone11`*x*y+coef$`I(x^2):zone11`*x^2+coef$`I(y^2):zone11`*y^2)}else{probz1 <- rep(0,length(prob0))}
        if(coef$zone21!=0){probz2 <- zone2*(coef$zone21+coef$`x:zone21`*x+coef$`y:zone21`*y+coef$`x:y:zone21`*x*y+coef$`I(x^2):zone21`*x^2+coef$`I(y^2):zone21`*y^2)}else{probz2 <- rep(0,length(prob0))}
        if(coef$zone31!=0){probz3 <- zone3*(coef$zone31+coef$`x:zone31`*x+coef$`y:zone31`*y+coef$`x:y:zone31`*x*y+coef$`I(x^2):zone31`*x^2+coef$`I(y^2):zone31`*y^2)}else{probz3 <- rep(0,length(prob0))}
        if(coef$zone41!=0){probz4 <- zone4*(coef$zone41+coef$`x:zone41`*x+coef$`y:zone41`*y+coef$`x:y:zone41`*x*y+coef$`I(x^2):zone41`*x^2+coef$`I(y^2):zone41`*y^2)}else{probz4 <- rep(0,length(prob0))}
        if(coef$zone51!=0){probz5 <- zone5*(coef$zone51+coef$`x:zone51`*x+coef$`y:zone51`*y+coef$`x:y:zone51`*x*y+coef$`I(x^2):zone51`*x^2+coef$`I(y^2):zone51`*y^2)}else{probz5 <- rep(0,length(prob0))}
        prob <- prob0+probz1+probz2+probz3+probz4+probz5
        A$p <- exp(prob)/(1+exp(prob))
        A <- A[-which((A$y>sqrt(23.71^2-(A$x-25)^2)+5.25)|(A$x<3)|(A$x>47)),]
    }
    A
}