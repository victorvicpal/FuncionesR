BetaSeason <- function(season)
{
	load('Beta3.Rdata')
	SQL <- paste("select distinct player from nba.play_by_play where game_id in (select id from nba.game where season='",season,"') and player<>'' and etype='shot';",sep='')
	M <- dbGetQuery(con,SQL)
	N <- matrix(0,length(M$player),14)
	N <- as.data.frame(N)
	colnames(N) <- c("player","(Intercept)","triple1","x","y","I(x^2)","I(y^2)","triple1:x","triple1:y","x:y","triple1:I(x^2)","triple1:I(y^2)","triple1:x:y","LR")
	N$player <- M$player
	for (i in 1:length(M))
	{
		N[i,2:14] <- Beta3(N[i,],season)
	}
N
}