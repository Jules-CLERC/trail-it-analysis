library(RMySQL)
library(tidyverse)
library(plotly)

#############
# Load
#############

options("digits.secs"=6)
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

#############
# Preprocess
#############

D = D %>% mutate(Timestamp = paste(date,time),
                 Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
          arrange(Timestamp)

#############
# Visualize
#############

#How many players are there in total?
nbPlayers = D %>% 
  summarise(player_name = unique(playerName)) %>% 
  count()

#On which dates did the players play?
datesPlayers = D %>% 
  select(playerName, date) %>%
  distinct(playerName, date) %>%
  arrange(playerName, date)

#For how much time did each player play? (number of days, minutes)
nbDaysGamePlayers = datesPlayers %>%
  count(playerName)
names(nbDaysGamePlayers)[2] <- 'nbDays'

nbMinutesGamePlayers = D %>% 
  select(playerName, sessionLength) %>%
  group_by(playerName) %>%
  summarise(sum(sessionLength))
names(nbMinutesGamePlayers)[2] <- 'nbMinutes'

nbTimeGamePlayers = merge(nbDaysGamePlayers, nbMinutesGamePlayers, "playerName")

#How many of the players are stroke patients?
nbStrokePlayers = D %>% 
  select(playerName, trainingReason) %>%
  distinct(playerName, trainingReason) %>%
  filter(!(trainingReason == "OtherReason")) %>%
  distinct(playerName) %>%
  count()

#Did the players improve their overall reaction time?
avgReactionTime = D %>%
  select(playerName, date, sessionMedianReactionTime) %>%
  group_by(playerName) %>%
  summarise(avgReactionTime = mean(sessionMedianReactionTime))

lastReactionTime = D %>%
  select(playerName, date, time, sessionMedianReactionTime) %>%
  group_by(playerName) %>%
  mutate(Timestamp = paste(date,time),
         Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  arrange(Timestamp) %>%
  filter(time == max(time)) %>%
  summarise(lastReactionTime = min(sessionMedianReactionTime))

improveReactionTime = merge(avgReactionTime, lastReactionTime, "playerName") %>%
  mutate(IsImprove = ifelse(avgReactionTime > lastReactionTime, TRUE, FALSE))

