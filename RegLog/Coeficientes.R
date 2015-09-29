Coeficientes <- function(player,season)
{
	SQL <- paste0("select * from nba.BETA where season='",season,"' and player='",player,"'",sep='')
	coef <- dbGetQuery(con,SQL)
	coef[,2:13]
}