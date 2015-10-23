biplotmatrix <- function(id)
{
	sql1 <- paste("select distinct player, team from nba.play_by_play where game_id=",id," and player<>'';",sep='')
	players <- dbGetQuery(con,sql1)

	sql2 <- paste("select distinct concat_ws('-',x,y) as coord,x,y from nba.play_by_play where game_id=",id,"  and etype='shot' order by x asc;",sep='')
	coord <- dbGetQuery(con,sql2)

	sql3 <- paste("select distinct a.team, a.player, a.distance, concat_ws('-',a.x,a.y) as coord, a.x, a.y, ifnull(b.smi,0) as smi, ifnull(c.sma,0) as sma, ifnull(c.sma,0)/(ifnull(b.smi,0)+ifnull(c.sma,0)) as pxy from (select game_id, player, team, result, replace(distance, 'ft', '') as distance, x, y from nba.play_by_play where game_id=",id," and etype='shot') a left join (select game_id, player, team, x, y, count(result) as smi from nba.play_by_play where result='missed' and game_id=",id," group by x,y, player) b on (a.x=b.x and a.y=b.y and a.player=b.player) left join (select game_id, player, team, x, y, count(result) as sma from nba.play_by_play where result='made' and game_id=",id," group by x,y, player) c on (a.x=c.x and a.y=c.y and a.player=c.player)",sep='')
	pbm <- dbGetQuery(con,sql3)

	m <- matrix(NA,nr=nrow(players), nc=nrow(coord))

	col <- c(coord[,1])
	colnames(m) <- col
	pla <- players[,1]
	rownames(m) <- pla

	pl <- match(pbm[,2],rownames(m))
	co <- match(pbm[,4],colnames(m))

	for (i in 1:nrow(pbm))
	{
		m[pl[i],co[i]] <- as.numeric(pbm[i,ncol(pbm)])
	}
m
}