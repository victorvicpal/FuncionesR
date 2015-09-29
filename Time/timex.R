timex <- function(x)
{
	C <- unclass(strptime(x[,12], format="%M:%S"))
	sec <- C$sec
	min <- C$min
	TIMER <- data.frame(x[,11],min, sec)
	if (TIMER[1,]==1)

}


unclass(strptime(PHOLAL[i,12], format="%M:%S")) 
(difftime(strptime(x[i,12], format="%M:%S"),strptime(x[i+1,12], format="%M:%S"))>=120)

C <- rbind(C,subset(PHOLAL, PHOLAL[, 11] == 2))