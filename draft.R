library(RMySQL)
library(tidyverse)
library(ggplot2)


credentials <- read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character","character","character","character"))

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


mydb = dbConnect(MySQL(),
                 user=credentials[1, "username"],
                 password=credentials[1, "password"],
                 dbname=credentials[1, "dbname"],
                 host=credentials[1, "host"])

RetreiveDataSet <- function(tablename) {
  queryString = "SELECT *"
  queryString = paste(queryString, "FROM",tablename, sep = " ")
  print(queryString)
  res = dbSendQuery(mydb, queryString)
  df = fetch(res, n=-1) 
  dbClearResult(dbListResults(mydb)[[1]])
  return(df)
}

D <- RetreiveDataSet(credentials[1, "table"])
