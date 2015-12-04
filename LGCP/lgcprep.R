lgcptest <- function(player,season)
{
#LGCP XY shot distribution
require(lgcp)
require(NMF)
require(spatstat)

#connexion mysql
m <- dbDriver("MySQL")
con <- dbConnect(m,user='root',host='localhost',dbname='nba')

#We need the shot matrix
SQL <- paste("select a.player, a.x, a.y, ifnull(a.MI,0) as MI, ifnull(b.MA,0) as MA, ifnull(b.MA/(b.MA+a.MI),0) as P from (select player, x,y, count(result) as MI from nba.play_by_play where etype='shot' and result='missed' and game_id in (select id from nba.game where season='",season,"') and player='",player,"' group by x,y,result) a left join (select player,x,y, count(result) as MA from nba.play_by_play where etype='shot' and result='made' and game_id in (select id from nba.game where season='",season,"') and player='",player,"' group by x,y,result) b on (a.x=b.x and a.y=b.y)", sep="")
M <- dbGetQuery(con,SQL)

#Define de grid ppp
mypattern <- ppp(M$x,M$y,c(0,50),c(4,35))

#Density (lambda)
den <- density.ppp(mypattern)
sar <- spatialAtRisk(den)
n <- dim(sar$Zm)[2]

#NMF sarnmf=W*B
sarnmf <- nmf(sar$Zm,n,'Frobenious')
W <- basis(sarnmf)
B <- coef(sarnmf)

#Reconstruct the matrix (similar to Pxy from logistic regression)
nx <- 1:n
ny <- 1:n
A <- expand.grid(nx,ny)
names(A) <- c('nx','ny')
A$X <- sar$X[A$nx]
A$Y <- sar$X[A$ny]

for (i in 1:length(A$X))
{
	A$lambda[i] <- sar$Zm[A$nx[i],A$ny[i]]
	A$lambdanmf[i] <- B[A$nx[i],A$ny[i]]
}
}