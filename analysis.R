#############
# Load
#############

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

#For how much time did each player play? (number of years, months, days, minutes)
#years
nbYearsGamePlayers = datesPlayers %>%
  mutate(year = substr(date, 0, 4)) %>%
  distinct(profileID, year) %>%
  group_by(profileID) %>%
  count()
names(nbYearsGamePlayers)[2] <- 'nbYears'
#months
nbMonthsGamePlayers = datesPlayers %>%
  mutate(month = substr(date, 0, 7)) %>%
  distinct(profileID, month) %>%
  group_by(profileID) %>%
  count()
names(nbMonthsGamePlayers)[2] <- 'nbMonths'
#days
nbDaysGamePlayers = datesPlayers %>%
  group_by(profileID) %>%
  count()
names(nbDaysGamePlayers)[2] <- 'nbDays'
#minutes
nbMinutesGamePlayers = D %>% 
  select(profileID, sessionLength) %>%
  group_by(profileID) %>%
  summarise(sum(sessionLength))
names(nbMinutesGamePlayers)[2] <- 'nbMinutes'
#merge all in nbTimeGamePlayers
nbTimeGamePlayers = merge(nbDaysGamePlayers, nbMinutesGamePlayers, "profileID")
nbTimeGamePlayers = merge(nbTimeGamePlayers, nbYearsGamePlayers, "profileID")
nbTimeGamePlayers = merge(nbTimeGamePlayers, nbMonthsGamePlayers, "profileID")
nbTimeGamePlayers = merge(nbTimeGamePlayers, listPlayers, "profileID")

#How many of the players are stroke patients?
strokePlayers = D %>% 
  select(profileID, trainingReason) %>%
  filter(!(trainingReason == "OtherReason")) %>%
  distinct(profileID) %>%
  mutate(stroke = "stroke")

strokePlayers = merge(strokePlayers, listPlayers, "profileID", all.y = TRUE)
strokePlayers = strokePlayers[c(1,2)]
strokePlayers[is.na(strokePlayers)] <- "no stroke"
nbTimeGamePlayers = merge(nbTimeGamePlayers, strokePlayers, "profileID")

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
  mutate(isImprove = ifelse(avgReactionTime > lastReactionTime, "Improve", "Not improve"))
improveReactionTime = improveReactionTime[c(1,4)]
nbTimeGamePlayers = merge(nbTimeGamePlayers, improveReactionTime, "profileID")
improveReactionTime = merge(improveReactionTime, listPlayers, "profileID")


