#Conexión con MySQL (RMySQL)

library(RMySQL)

con <- dbConnect(MySQL(),user = 'root',host = 'localhost',dbname= 'nba_data_base')

dbReadTable(con,"Players")