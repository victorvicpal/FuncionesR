Beta <- function(player)
{
	SQL <- paste("select player, x ,y , if(result='made',1,0) as result, if(type='3pt',1,0) as triple from nba.play_by_play where etype='shot' and y>3 and game_id in (select id from nba.game where season='07-08') and player='",player,"';", sep="")
	M <- dbGetQuery(con,SQL)
	M$triple <- as.factor(M$triple)
	SQL2 <- paste("select a.num, ifnull(b.triple,0) as triple, ifnull(b.triple,0)/a.num as Q from (select player, count(player) as num from nba.play_by_play where etype='shot' and player='",player,"' and game_id in (select id from nba.game where season='07-08')) A left join (select player, count(player) as triple from nba.play_by_play where etype='shot' and player='",player,"' and type='3pt' and game_id in (select id from nba.game where season='07-08')) b on a.player=b.player;", sep="")
	tr <- dbGetQuery(con,SQL2)
	tr$Q <- as.numeric(tr$Q)
	if (tr$num>40)
	{
		if (tr$Q>0.01 & tr$triple>3)
			{
			output <- logistf(result ~ triple*x + triple*y + triple + triple*x*y + triple*I(x^2) + triple*I(y^2), data = M, family = binomial)
			Estimates <- output$coefficients
			LR <- -2*output$loglik[1]+2*output$loglik[2]
			print(Estimates)
			print(drop1(output))
			print(LR)
			}
		else
			{
			output <- logistf(result ~ x + y + x*y + I(x^2) + I(y^2), data = M, family = binomial)
			drop1(output)
			Estimates <- output$coefficients
			LR <- -2*output$loglik[1]+2*output$loglik[2]
			print(Estimates)
			print(drop1(output))
			print(LR)
			}
	}
	else {print('no hay suficientes datos')}
}
