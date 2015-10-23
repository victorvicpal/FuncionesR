TypeShootAway <- function(x,y)
{
	drivinglut <- rep(0,nrow(y))
	drivinglum <- rep(0,nrow(y))
	dunkt <- rep(0,nrow(y))
	dunkm <- rep(0,nrow(y))
	hookt <- rep(0,nrow(y))
	hookm <- rep(0,nrow(y))
	jumpt <- rep(0,nrow(y))
	jumpm <- rep(0,nrow(y))
	layupt <- rep(0,nrow(y))
	layupm <- rep(0,nrow(y))
	pullupt <- rep(0,nrow(y))
	pullupm <- rep(0,nrow(y))
	runningt <- rep(0,nrow(y))
	runningm <- rep(0,nrow(y))
	shott <- rep(0,nrow(y))
	shotm <- rep(0,nrow(y))
	tipt <- rep(0,nrow(y))
	tipm <- rep(0,nrow(y))

	match <- data.frame(y,drivinglut,drivinglum,dunkt,dunkm,hookt,hookm,jumpt,jumpm,layupt,layupm,pullupt,pullupm,runningt,runningm,shott,shotm,tipt,tipm)

	D <- subset(x[c("a1","a2","a3","a4","a5","period","etype","player","result","type")])

	for (i in 1:nrow(D))
	{
		if (D[i,7]=="shot")
		{
			j <- which(as.character(match[,1])==D[i,8])

			if (D[i,10]=="driving layup")
			{
				drivinglut[j]=drivinglut[j]+1
				match[j,3]=drivinglut[j]
				if (D[i,9]=="made")
				{
					drivinglum[j]=drivinglum[j]+1
					match[j,4]=drivinglum[j]
				}
			}

			if (D[i,10]=="dunk")
			{
				dunkt[j]=dunkt[j]+1
				match[j,5]=dunkt[j]
				if (D[i,9]=="made")
				{
					dunkm[j]=dunkm[j]+1
					match[j,6]=dunkm[j]
				}
			}

			if (D[i,10]=="hook")
			{
				hookt[j]=hookt[j]+1
				match[j,7]=hookt[j]
				if (D[i,9]=="made")
				{
					hookm[j]=hookm[j]+1
					match[j,8]=hookm[j]
				}
			}

			if (D[i,10]=="jump")
			{
				jumpt[j]=jumpt[j]+1
				match[j,9]=jumpt[j]
				if (D[i,9]=="made")
				{
					jumpm[j]=jumpm[j]+1
					match[j,10]=jumpm[j]
				}
			}

			if (D[i,10]=="layup")
			{
				layupt[j]=layupt[j]+1
				match[j,11]=layupt[j]
				if (D[i,9]=="made")
				{
					layupm[j]=layupm[j]+1
					match[j,12]=layupm[j]
				}
			}

			if (D[i,10]=="pullup jump")
			{
				pullupt[j]=pullupt[j]+1
				match[j,13]=pullupt[j]
				if (D[i,9]=="made")
				{
					pullupm[j]=pullupm[j]+1
					match[j,14]=pullupm[j]
				}
			}

			if (D[i,10]=="running")
			{
				runningt[j]=runningt[j]+1
				match[j,15]=runningt[j]
				if (D[i,9]=="made")
				{
					runningm[j]=runningm[j]+1
					match[j,16]=runningm[j]
				}
			}

			if (D[i,10]=="shot")
			{
				shott[j]=shott[j]+1
				match[j,17]=shott[j]
				if (D[i,9]=="made")
				{
					shotm[j]=shotm[j]+1
					match[j,18]=shotm[j]
				}
			}

			if (D[i,10]=="tip")
			{
				tipt[j]=tipt[j]+1
				match[j,19]=tipt[j]
				if (D[i,9]=="made")
				{
					tipm[j]=tipm[j]+1
					match[j,20]=tipm[j]
				}
			}
		}
	}
match
}