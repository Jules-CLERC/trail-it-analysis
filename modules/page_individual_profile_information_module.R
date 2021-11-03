page_individual_profile_information_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              column(6,
                     uiOutput(ns("player_header"))
              )
            ),
            fluidRow(
              column(6,
                     uiOutput(ns("player_performance_header")),
                     uiOutput(ns("player_characteristics"))
              ),
              column(6,
                     uiOutput(ns("player_progress_treatment")),
                     uiOutput(ns("player_past_sessions"))
              )
            )
  )
}

#server function
page_individual_profile_information <- function(input, output, session, D, currentPlayer) {
  
  playerData <- reactiveValues(df = NULL)
  
  observeEvent(currentPlayer(), {
    playerData$df <- D() %>% 
      filter(profileID == currentPlayer())
  })
  
  output$player_header <- renderUI({
    validate(need(playerData$df, "No current player."))
    
    dataLastDatePlayer = playerData$df %>%
      filter(Timestamp == max(Timestamp))
    
    playerName = as.character(playerData$df[1, "playerName"])
    
    lastDatePlay = as.character(paste(
      "Played",
      as.numeric(
        as.duration(today() - ymd(dataLastDatePlayer[1, "date"])),"days"
      ),
      "day(s) ago"
    ))
    
    toReturn <- fluidRow(
      img(src='user.png', align = "left", id="user-image"),
      div(
        h1(playerName),
        p(lastDatePlay)
      )
    )
    return(toReturn)
  })
  
  output$player_performance_header <- renderUI({
    validate(need(playerData$df, "No current player."))
    
    #get last session reaction time
    lastSession <- playerData$df %>%
      filter(sessionID == max(sessionID))
    
    lastSessionReactionTime <- paste0(
      round(lastSession[1,"sessionMedianReactionTime"], 2),
      "s"
    )
    
    #Get last percentage hits
    lastDataPointsTouched <- lastSession %>%
      summarise(
        sumHits = sum(levelHitsTotal),
        sumErrors = sum(levelErrorsTotal)
      )
    lastPercentageHits = round(((lastDataPointsTouched[1,"sumHits"] - lastDataPointsTouched[1,"sumErrors"])/lastDataPointsTouched[1,"sumHits"]) * 100)
    
    #TODO : I don't remember with what kind of data I need to compare the last percentage hits
    #Is it the first session reaction time ?
    #Get first percentage hits to do the comparison
    firstSession <- playerData$df %>%
      filter(gameType == toString(lastSession[1,"gameType"])) %>%
      filter(circleAmount == toString(lastSession[1,"circleAmount"])) %>%
      filter(sessionLength == toString(lastSession[1,"sessionLength"])) %>%
      filter(sessionID == min(sessionID))
    
    firstDataPointsTouched <- firstSession %>%
      summarise(
        sumHits = sum(levelHitsTotal),
        sumErrors = sum(levelErrorsTotal)
      )
    firstPercentageHits = round(((firstDataPointsTouched[1,"sumHits"] - firstDataPointsTouched[1,"sumErrors"])/firstDataPointsTouched[1,"sumHits"]) * 100)
    
    #determine the difference between two percentages
    resultPercentageHits = lastPercentageHits - firstPercentageHits
    color = "red"
    if(resultPercentageHits >= 0) {
      resultPercentageHits = paste0("+",resultPercentageHits)
      color="rgb(40,176,36)"
    }

    #UI to return
    toReturn <- fluidRow(
      column(6,
             class="div-player-performance",
             h2(lastSessionReactionTime),
             p("Reaction time")
      ),
      column(6,
             class="div-player-performance",
             div(
               id="label-percentage-hits",
               style= paste("background-color:", color),
               p(paste0(resultPercentageHits,"%"))
             ),
             h2(paste0(lastPercentageHits, "%")),
             p("Hits")
      )
    )
    return(toReturn)
  })
  
  output$player_characteristics <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    toReturn <- div(
      h2("Player characteristics"),
      p(paste("Age:",
              as.character(playerData$df[1, "ageGroup"]))),
      p(paste("Training reason:", 
              as.character(playerData$df[1, "trainingReason"])))
    )
    
    return(toReturn)
  })
  
  output$player_progress_treatment <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    #Get the last reaction time of the player
    lastSession <- playerData$df %>%
      filter(sessionID == max(sessionID))
    lastSessionReactionTime <- lastSession[1,"sessionMedianReactionTime"]
    
    #Get the min value
    #Progress bar: 0 = 2*sd(strokes_players_reaction_time_array)
    listMedianReactionTimeStrokesPlayers <- D() %>%
      filter(trainingReason != "OtherReason") %>%
      group_by(profileID) %>%
      filter(gameType == toString(lastSession[1,"gameType"])) %>%
      filter(circleAmount == toString(lastSession[1,"circleAmount"])) %>%
      filter(sessionLength == toString(lastSession[1,"sessionLength"])) %>%
      filter(sessionID == min(sessionID)) %>%
      distinct(sessionMedianReactionTime) %>%
      ungroup() %>%
      summarise(value = mean(sessionMedianReactionTime))
      #summarise(value = 2 * sd(sessionMedianReactionTime))
    stkValue = listMedianReactionTimeStrokesPlayers$value
    
    #Get the max value
    #Progress bar: 100 = 2*sd(mpc_players_reaction_time_array)
    listMedianReactionTimeMpcPlayers <- D() %>%
      filter(substring(playerName, 1, 4) == "MPC-") %>%
      filter(date == "2021-10-15") %>%
      group_by(profileID) %>%
      filter(gameType == toString(lastSession[1,"gameType"])) %>%
      filter(circleAmount == toString(lastSession[1,"circleAmount"])) %>%
      filter(sessionLength == toString(lastSession[1,"sessionLength"])) %>%
      filter(sessionID == min(sessionID)) %>%
      distinct(sessionMedianReactionTime) %>%
      ungroup() %>%
      summarise(value = mean(sessionMedianReactionTime))
      # summarise(value = 2 * sd(sessionMedianReactionTime))
    mpcValue = listMedianReactionTimeMpcPlayers$value
    
    d1 = stkValue - mpcValue
    d2 = stkValue - lastSessionReactionTime$sessionMedianReactionTime
    d3 = lastSessionReactionTime$sessionMedianReactionTime - mpcValue
    if(d3 < 0) {
      result = 100
    }
    else {
      result = (d2 / d1) * 100
    }
    
    #UI to return
    toReturn <- div(
      h2("Current treatment progress"),
      p(paste(round(d3,2), "secondes from normal reaction time")),
      progressBar(
        id = "pbReactionTime",
        value = result,
        total = 100,
        title = "",
        display_pct = TRUE,
        status = "custom"
      )
    )
    
    return(toReturn)
  })
  
  output$player_past_sessions <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    datesPlayer <- playerData$df %>%
      filter(levelNumber == 1) %>%
      select(Timestamp) %>%
      arrange(desc(Timestamp))
    
    strDates = ""
    maxDates = 3
    for (i in 1:nrow(datesPlayer)) {
      if (i <= maxDates) {
        fullDate = datesPlayer[i, "Timestamp"][[1]]
        ymd = as.Date(fullDate, format = "%Y-%m-%d %H:%M:%OS")
        strDates = paste(strDates,
                         paste0("<p>",
                           wday(ymd, label = TRUE), ", ", 
                           ymd(ymd), " at ", 
                           format(strptime(fullDate, format = "%Y-%m-%d %H:%M:%OS"), '%H:%M'), "</p>")
                         )
      }
    }
    if(nrow(datesPlayer) > maxDates) {
      moreDates = nrow(datesPlayer) - maxDates
      strDates = paste(strDates,
                       paste0("<p>(",
                              moreDates,
                              " other sessions)</p>")
                       
      )
    }
    
    toReturn <- div(
      h2("Past play sessions"),
      HTML(strDates)
    )
    
    return(toReturn)
  })
}
