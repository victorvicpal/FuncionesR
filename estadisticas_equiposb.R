estadisticas_equipos_0607 <- function()
{
	m <- dbDriver("MySQL")
	con <- dbConnect(m,user='root',host='localhost',dbname='nba')
	sql <- paste("select distinct hometeam from nba.game where hometeam not in ('ALL','TAR') and season='06-07'",sep="")
	teams <- dbGetQuery(con,sql)
	sql2 <- paste("select * from nba.play_by_play a, nba.game b where a.game_id=b.id and b.season='06-07'")
	jugadas <- dbGetQuery(con,sql2)

	shota <- rep(0,nrow(teams))
	shotm <- rep(0,nrow(teams))
	threepa <- rep(0,nrow(teams))
	threepm <- rep(0,nrow(teams))
	fta <- rep(0,nrow(teams))
	ftm <- rep(0,nrow(teams))
	rebo <- rep(0,nrow(teams))
	rebd <- rep(0,nrow(teams))
	assist <- rep(0,nrow(teams))
	foulc <- rep(0,nrow(teams))
	foulr <- rep(0,nrow(teams))
	steal <- rep(0,nrow(teams))
	to <- rep(0,nrow(teams))
	blockm <- rep(0,nrow(teams))
	blocka <- rep(0,nrow(teams))
	points <- rep(0,nrow(teams))
	possessions <- rep(0,nrow(teams))
	stats <- data.frame(teams,shota,shotm,threepa,threepm,fta,ftm,rebo,rebd,assist,foulc,foulr,steal,to,blockm,blocka,points,possessions)

	for (i in 1:max(jugadas[,36]))
	{
		stats_i <- data.frame(teams,shota,shotm,threepa,threepm,fta,ftm,rebo,rebd,assist,foulc,foulr,steal,to,blockm,blocka,points,possessions)

		match <- subset(jugadas,jugadas[,36]==i)

		for (l in 1:nrow(match))
		{
			j <- which(as.character(teams[,1])==as.character(match[l,3]))
				#Tiros de 2 puntos#
				if (match[l,30]=="made")
				{
					shotm[j]=shotm[j]+1
					shota[j]=shota[j]+1
					stats_i[j,2]=shota[j]
					stats_i[j,3]=shotm[j]
				}
				else
				{
					shota[j]=shota[j]+1
					stats_i[j,2]=shota[j]
				}
		}
		stats[j,2:ncol(stats)]=stats[j,2:ncol(stats)]+stats_i[j,2:ncol(stats)]
	}
stats
}
