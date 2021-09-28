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

listPlayers = D %>% 
  distinct(profileID, playerName) %>%
  mutate(playerNameID = paste(playerName, "(", profileID, ")"))

#How many players are there in total?

nbPlayers = listPlayers %>% count()

#On which dates did the players play?
datesPlayers = D %>% 
  select(profileID, date) %>%
  distinct(profileID, date) %>%
  arrange(profileID, date)
datesPlayers = merge(datesPlayers, listPlayers, "profileID")

#For how much time did each player play? (number of days, minutes)
nbDaysGamePlayers = datesPlayers %>%
  group_by(profileID) %>%
  count()
names(nbDaysGamePlayers)[2] <- 'nbDays'

nbMinutesGamePlayers = D %>% 
  select(profileID, sessionLength) %>%
  group_by(profileID) %>%
  summarise(sum(sessionLength))
names(nbMinutesGamePlayers)[2] <- 'nbMinutes'

nbTimeGamePlayers = merge(nbDaysGamePlayers, nbMinutesGamePlayers, "profileID")
nbTimeGamePlayers = merge(nbTimeGamePlayers, listPlayers, "profileID")

#How many of the players are stroke patients?
strokePlayers = D %>% 
  select(profileID, trainingReason) %>%
  filter(!(trainingReason == "OtherReason")) %>%
  distinct(profileID)

#Did the players improve their overall reaction time?
#TODO : change the calculation of the improve
avgReactionTime = D %>%
  select(profileID, date, sessionMedianReactionTime) %>%
  group_by(profileID) %>%
  summarise(avgReactionTime = mean(sessionMedianReactionTime))

lastReactionTime = D %>%
  select(profileID, date, time, sessionMedianReactionTime) %>%
  group_by(profileID) %>%
  mutate(Timestamp = paste(date,time),
         Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  arrange(Timestamp) %>%
  filter(time == max(time)) %>%
  summarise(lastReactionTime = min(sessionMedianReactionTime))

improveReactionTime = merge(avgReactionTime, lastReactionTime, "profileID") %>%
  mutate(IsImprove = ifelse(avgReactionTime > lastReactionTime, TRUE, FALSE))
improveReactionTime = merge(improveReactionTime, listPlayers, "profileID")

