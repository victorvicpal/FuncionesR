DynamicBeta <- function(player,season,pv=0.01)
{
	#Required packages
    require(logistf)
    require(aod)
    require(dplyr)
    require(cluster)
    
    #SQL query selecting player and season
    SQL <- paste("select player, x ,y , if(result='made',1,0) as result, if(x<3 or x>47,1,if(y>(sqrt((23.71*23.71)-(x-25)*(x-25))+5.25),1,0)) as triple from nba.play_by_play where etype='shot' and y>3 and game_id in (select id from nba.game where season='",season,"') and player='",player,"';", sep="")
    M <- dbGetQuery(con,SQL)
    M2 <- subset.data.frame(M, M$triple==0)
    M3 <- subset.data.frame(M, M$triple==1)

    #Zone selection
    d <- dist(M2[,2:3], method = "euclidean")
    fit <- hclust(d, method="ward.D")
    groups <- cutree(fit, k=5)

    M2$g1 <- ifelse(groups==1,1,0)
    M2$g2 <- ifelse(groups==2,1,0)
    M2$g3 <- ifelse(groups==3,1,0)
    M2$g4 <- ifelse(groups==4,1,0)
    M2$g5 <- ifelse(groups==5,1,0)
    M3$g1 <- rep(0,length(M3$player))
    M3$g2 <- rep(0,length(M3$player))
    M3$g3 <- rep(0,length(M3$player))
    M3$g4 <- rep(0,length(M3$player))
    M3$g5 <- rep(0,length(M3$player))
    M <- rbind.data.frame(M2,M3)
    
    #Matrix exploration
    Means <- apply(M[,5:10],2,mean)
    Means1 <- Means
    ZonePerc <- apply(M[,5:10],2,function(x,y) length(x[x==1 & y==1])/length(x[x==1]), y=M$result)
    
    #Triple or not
    if (Means[1]<0.1) {Means <- Means[-1]}
    
    Means <- Means[Means!=0]
    
    xnom <- names(Means)
    minm <- names(Means[which.min(Means)])
    
    
    #Initial formula
    form <- paste0('(1+',paste0(xnom,collapse = '+'),')')
    formula <- as.formula(paste0('result~(x+y+x*y+I(x^2)+I(y^2))*',form))
    
    #All zone model (logistic regression)
    M[,5:10] <- apply(M[,5:10],2,factor)
    output <- logistf(formula,data=M,pl=FALSE)
    print(coef(output))
    cat("LR first= ", (output$loglik[2]-output$loglik[1])*2, "\n")
    #First Wald Test
    t <- try(wald.test(b= coef(output), Sigma= vcov(output), Terms = 1:output$df[1]))
    op <- 0
    
    while ("try-error" %in% class(t))
    {
        op <- op+1
        minm <- names(Means[which.min(Means)])
        Means <- Means[-which.min(Means)]
        form <- gsub(paste0('[+]',minm),'',form)
        formula <- as.formula(paste0('result~(x+y+x*y+I(x^2)+I(y^2))*',form))
        output <- logistf(formula,data=M,pl=FALSE)
        t <- try(wald.test(b=coef(output), Sigma=vcov(output), Terms = 1:output$df[1]))
        cat("iteration1 nº= ", op, "\n")
        if (op==5) {break}
    }
    cat("iteration1 = ", op, "\n")
    cat("LR second= ", (output$loglik[2]-output$loglik[1])*2, "\n")
    print(output)
    print(t)
    
    cont <- 0
    t2 <- t
    
    #Searching the minimun p-value Loop
    while (t$result$chi2[3]>pv & t2$result$chi2[3]>=t$result$chi2[3])
    {
        cont <- cont+1
        t2 <- t
        output2 <- output
        minm <- names(Means[which.min(Means)])
        Means <- Means[-which.min(Means)]
        form <- gsub(paste0('[+]',minm),'',form)
        formula <- as.formula(paste0('result~(x+y+x*y+I(x^2)+I(y^2))*',form))
        output <- logistf(formula,data=M,pl=FALSE)
        t <- try(wald.test(b=coef(output), Sigma=vcov(output), Terms = 1:output$df[1]))
        if (t2$result$chi2[3]<t$result$chi2[3]) 
        {
            output <- output2
            break
        }
        cat("iteration2 nº = ", cont, "\n")
        print(output)
        print(t)
        if (cont==6-op) {break}
    }
    
    
    cat('---------------------------------------------------------', "\n")
    LR <- (output$loglik[2]-output$loglik[1])*2
    
    cat('SUMMARY','\n')
    cat('---------------------------------------------------------', "\n")
    cat("FG% zones ", "\n")
    print(ZonePerc)
    cat("shots by zone ", "\n")
    print(Means1)
    cat("iteration1 = ", op, "\n")
    cat("iteration2 = ", cont, "\n")
    cat("LRfinal = ", LR, "\n")
    output
}