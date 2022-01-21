#server function
data_import <- function(input, output, session) {
  
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
  
  #Add column Timestamp
  D = D %>% mutate(Timestamp = paste(date,time),
                   Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    arrange(Timestamp)
  
  #Delete duplicate rows
  D = D %>%
    subset(select = -c(id)) %>%
    distinct()
  
  #Remove negative reaction time
  D = D %>%
    filter(levelReactionTime > 0)
  
  #Add sessionID
  D = D %>%
    group_by(profileID) %>%
    mutate(
      sessionID = ifelse(
        levelNumber > lag(levelNumber, default = 0), 0, 1
      ),
      sessionID = cumsum(sessionID)
    ) %>%
    ungroup()
  
  return(D)
}