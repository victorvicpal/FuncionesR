players <- function(x) {

teams <- toString(x[1,13])

for (i in 1:nrow(x))
if (x[i,13]!="OFF")
teams <- unique(c(teams,toString(x[i,13])))

away <- c(toString(x[1,1]),toString(x[1,2]),toString(x[1,3]),toString(x[1,4]),toString(x[1,5]))
home <- c(toString(x[1,6]),toString(x[1,7]),toString(x[1,8]),toString(x[1,9]),toString(x[1,10]))

for (i in 1:nrow(x))
if ((x[i,18]!="")&&(x[i,13]==teams[1]))
away <- unique(c(away,toString(x[i,18])))

for (i in 1:nrow(x))
if ((x[i,18]!="")&&(x[i,13]==teams[2]))
home <- unique(c(home,toString(x[i,18])))

HOME <- matrix(home, length(home), 2)
HOME[,2]=teams[2]

AWAY <- matrix(away, length(away), 2)
AWAY[,2]=teams[1]

}