awayteam <- function(x) {
teams <- toString(x[1,13])

for (i in 1:nrow(x))
if (x[i,13]!="OFF")
teams <- unique(c(teams,toString(x[i,13])))

players <- c(toString(x[1,1]),toString(x[1,2]),toString(x[1,3]),toString(x[1,4]),toString(x[1,5]))

for (i in 1:nrow(x))
if ((x[i,18]!="")&&(x[i,13]==teams[1]))
players <- unique(c(players,toString(x[i,18])))

AWAY <- data.frame(players, teams[1])

AWAY
}