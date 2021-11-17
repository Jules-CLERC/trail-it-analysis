page_individual_profile_information_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              column(6,
                     div(
                       id = 'div-player-header',
                       img(src='user.png', align = "left", id="user-image"),
                       uiOutput(ns("player_header"))
                     )
              )
            ),
            br(),
            fluidRow(
              column(6,
                     uiOutput(ns("player_performance_header"))
              ),
              column(6,
                     h5("Current treatment progress", class="title-div"),
                     uiOutput(ns("player_progress_treatment"))
              )
            ),
            fluidRow(
              column(6,
                     h5("Player characteristics", class="title-div"),
                     uiOutput(ns("player_characteristics"))
              ),
              column(6,
                     h5("Past play sessions", class="title-div"),
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
    
    dur = as.duration(today() - ymd(dataLastDatePlayer[1, "date"]))
    datePlayer = dataLastDatePlayer[1, "date"]
    nbDays = as.numeric(dur, "days")
    #if duration < 30 days reports in days
    if(nbDays <= 30) {
      lastDatePlay = as.character(paste("Played", nbDays, "day(s) ago (", datePlayer[[1]], ")"))
    } 
    #else if duration < 365 days reports in months
    else if (nbDays <= 365) {
      lastDatePlay = as.character(paste("Played", round(as.numeric(dur, "months")), "month(s) ago (", datePlayer[[1]], ")"))
    }
    #else report in years
    else {
      lastDatePlay = as.character(paste("Played", round(as.numeric(dur, "years")), "year(s) ago (", datePlayer[[1]], ")"))
    }
    
    toReturn <- div(
        h1(playerName),
        p(lastDatePlay)
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
    
    #Get previous percentage hits to do the comparison
    previousSession <- playerData$df %>%
      filter(gameType == toString(lastSession[1,"gameType"])) %>%
      filter(circleAmount == toString(lastSession[1,"circleAmount"])) %>%
      filter(sessionLength == toString(lastSession[1,"sessionLength"])) %>%
      filter(sessionID != max(sessionID)) %>%
      filter(sessionID == max(sessionID))
    
    resultPercentageHits = 0
    #If a previous session exists
    if(nrow(previousSession) > 0) {
      previousDataPointsTouched <- previousSession %>%
        summarise(
          sumHits = sum(levelHitsTotal),
          sumErrors = sum(levelErrorsTotal)
        )
      previousPercentageHits = round(((previousDataPointsTouched[1,"sumHits"] - previousDataPointsTouched[1,"sumErrors"])/previousDataPointsTouched[1,"sumHits"]) * 100)
      
      #determine the difference between two percentages
      resultPercentageHits = lastPercentageHits - previousPercentageHits
    }
    
    #determine color of the background label
    color = "red"
    if(resultPercentageHits >= 0) {
      resultPercentageHits = paste0("+",resultPercentageHits)
      color="rgb(40,176,36)"
    }

    #UI to return
    toReturn <- fluidRow(
      column(6,
             class="div-player-performance",
             h1(lastSessionReactionTime),
             p("Reaction time")
      ),
      column(6,
             class="div-player-performance",
             div(
               id="label-percentage-hits",
               style= paste("background-color:", color),
               p(paste0(resultPercentageHits,"%"))
             ),
             h1(paste0(lastPercentageHits, "%")),
             p("Hits")
      )
    )
    return(toReturn)
  })
  
  output$player_characteristics <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    toReturn <- div(
      p(paste("Age:",
              as.character(playerData$df[1, "ageGroup"]))),
      p(paste("Training reason:", 
              as.character(playerData$df[1, "trainingReason"]))),
      p(paste("Email:", 
              as.character(playerData$df[1, "email"]))),
      p(paste("Play context:", 
              as.character(playerData$df[1, "playContext"])))
    )
    
    return(toReturn)
  })
  
  output$player_progress_treatment <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    #Get the last reaction time of the player
    lastSession <- playerData$df %>%
      filter(sessionID == max(sessionID))
    lastSessionReactionTime <- lastSession[1,"sessionMedianReactionTime"]
    
    #Get MPC datas
    mpcD <- D() %>%
      filter(substring(playerName, 1, 4) == "MPC-") %>%
      filter(date == "2021-10-15")
    
    #Check if the last session can be compared with the MPC datas
    settingsLastSession <- lastSession %>%
      distinct(gameType, circleAmount, sessionLength)
    settingsMPC <- mpcD %>%
      distinct(gameType, circleAmount, sessionLength)
    
    #if the datas can be compared
    if(nrow(merge(settingsLastSession,settingsMPC)) > 0) {
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
        summarise(value = 2 * sd(sessionMedianReactionTime))
      stkValue = listMedianReactionTimeStrokesPlayers$value
      
      #Get the max value
      #Progress bar: 100 = 2*sd(mpc_players_reaction_time_array)
      listMedianReactionTimeMpcPlayers <- mpcD %>%
        group_by(profileID) %>%
        filter(gameType == toString(lastSession[1,"gameType"])) %>%
        filter(circleAmount == toString(lastSession[1,"circleAmount"])) %>%
        filter(sessionLength == toString(lastSession[1,"sessionLength"])) %>%
        filter(sessionID == min(sessionID)) %>%
        distinct(sessionMedianReactionTime) %>%
        ungroup() %>%
        summarise(value = mean(sessionMedianReactionTime))
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
      result = round(result)
      #UI to return
      toReturn <- div(
        p(paste(round(d3,2), "seconds from normal reaction time")),
        HTML(paste0('
          <div class="progress" style="height: 20px;">
            <div class="progress-bar" role="progressbar" style="width: ', result ,'%;" aria-valuenow="', result ,'" aria-valuemin="0" aria-valuemax="100">', result ,'%</div>
          </div>
        '))
      )
      return(toReturn)
    }
    else {
      #Check if a previous session with the same settings exists
      similarPreviousSession <- playerData$df %>%
        filter(sessionID != max(sessionID)) %>%
        merge(settingsLastSession) %>%
        filter(sessionID == max(sessionID))
      if(nrow(similarPreviousSession) == 0) {
        #UI to return
        toReturn <- div(
          p("No MPC data to compared"),
          p("No previous session with the same settings to compared")
        )
        return(toReturn)
      }
      else {
        #Compared the two sessions
        result <- similarPreviousSession[1,"sessionMedianReactionTime"] - lastSessionReactionTime 
        if (result > 0) {
          #UI to return
          toReturn <- div(
            p(paste("reaction time improved by", round(result, 2), "s"))
          )
          return(toReturn)
        }
        else if (result < 0) {
          #UI to return
          toReturn <- div(
            p(paste("reaction time decreased by", abs(round(result, 2)), "s"))
          )
          return(toReturn)
        }
        else {
          #UI to return
          toReturn <- div(
            p("reaction time unchanged")
          )
          return(toReturn)
        }
      }
    }
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
      ns <- session$ns
      callModule(modal_individual_profile_dates, "modal_individual_profile_dates", datesPlayer)
      toReturn <- div(
        HTML(strDates),
        modal_individual_profile_dates_UI(ns("modal_individual_profile_dates"))
      )
    }
    else {
      toReturn <- div(
        HTML(strDates)
      )
    }
    return(toReturn)
  })
}
