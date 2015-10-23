conexion_base_datos <- function()
{
	m <- dbDriver("MySQL")
	con <- dbConnect(m,user='root',host='localhost',dbname='nba')
}