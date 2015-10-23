Pxy2 <- function(player,season)
{
    SQL <- paste0("select * from nba.BETA where season='",season,"' and player='",player,"'",sep='')
    coef <- dbGetQuery(con,SQL)
    coef <- coef[,2:13]
    
    #Define x and y
    x <- seq(0,50,0.1)
    y <- seq(4.25,40,0.1)
    
    #Grid xy
    A <- expand.grid(x,y)
    names(A) <- c('x','y')
    
    x <- A$x
    y <- A$y
    A$triple <- rep(0,length(x))
    triple <- A$triple
    
    for (i in 1:length(triple)) 
    {
        if(x[i]<3)
        {
            triple[i] <- 1
        }
        else
        {
            if (x[i]>47)
            {
                triple[i] <- 1
            }
            else
            {
                if (y[i]>sqrt((23.71^2)-(x[i]-25)^2)+5.25)
                {
                    triple[i] <- 1
                }
            }
        }
    }

    A$triple <- triple

        #Redifine the grid (over 3pt line)
    if (coef$"triple1"==0)
    {
        A <- A[-which((A$y>sqrt(23.71^2-(A$x-25)^2)+5.25)|(A$x<3)|(A$x>47)),]
    }
    else
    {
        A <- A[-which(A$y>sqrt(32^2-(A$x-25)^2)+5.25),]
    }

    x <- A$x
    y <- A$y
    triple <- A$triple
    
    #Probability
    Prob <- coef$"(Intercept)" + coef$"x"*x + coef$"y"*y + coef$"x:y"*x*y + coef$"I(x^2)"*x^2 + coef$"I(y^2)"*y^2 + triple*(coef$"triple1" + coef$"triple1:x"*x + coef$"triple1:y"*y + coef$"triple1:x:y"*x*y + coef$"triple1:I(x^2)"*x^2 + coef$"triple1:I(y^2)"*y^2)
    p <- exp(Prob)/(1 + exp(Prob))
    
    C <- cbind.data.frame(A,p)
}