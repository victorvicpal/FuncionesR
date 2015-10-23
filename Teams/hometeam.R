hometeam <- function(x) {
teams <- toString(x[1,13])

for (i in 1:nrow(x))
if (x[i,13]!="OFF")
teams <- unique(c(teams,toString(x[i,13])))

players <- c(toString(x[1,6]),toString(x[1,7]),toString(x[1,8]),toString(x[1,9]),toString(x[1,10]))

for (i in 1:nrow(x))
if ((x[i,18]!="")&&(x[i,13]==teams[2]))
players <- unique(c(players,toString(x[i,18])))

HOME <- data.frame(players, teams[2])

HOME
}