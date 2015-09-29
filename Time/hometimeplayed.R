hometimeplayed <- function(x,y)
{
    E <- subset(x[c("h1","h2","h3","h4","h5","period","time")])
    D <- subset.data.frame(E,E[,6]==1)
    C <- unclass(strptime(D[,7], format="%M:%S"))
    sec <- C$sec
    min <- C$min
    t <- rep(0,nrow(y))
    matcht <- data.frame(y,t)
    for (i in 2:nrow(D))
        {
        away <- c(which((as.character(y[,1]))==D[i,1]),which((as.character(y[,1]))==D[i,2]),which((as.character(y[,1]))==D[i,3]),which((as.character(y[,1]))==D[i,4]),which((as.character(y[,1]))==D[i,5]))
        time <- (sec[i-1]+ min[i-1]%*%60) - (sec[i]+ min[i]%*%60)
        for (j in 1:length(t))
            if (j%in%away)
            {
            t[j]=t[j]+time
            matcht[j,3]=t[j]
            }
        }
matcht
}