data_import_UI <- function(id) {
  # ns is short for "namespace". The function call below will initialize the namespace used
  # for this module. Always have this line of code as the first thing in your UI.
  ns = NS(id)
  
  # UI code
  list(fluidRow(
    # When we create UI inside a module, we need to encapsulate
    # the ID with ns() (fx. ns("file") will result in "{modulename}-file"). 
    # This is necessary, in order for each ID to become unique.
    actionButton(ns("addSQLData"), "Add SQL Data")
  )
  )
}

#server function
data_import <- function(input, output, session) {
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  observeEvent(input$addSQLData, {
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

    toReturn$df <- D
    toReturn$trigger <- toReturn$trigger + 1
    
  })
  
  return(toReturn)
}