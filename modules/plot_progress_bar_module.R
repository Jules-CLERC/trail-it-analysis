plot_progress_bar_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    h5("Current treatment progress", class="title-div"),
    uiOutput(ns("player_progress_treatment"))
  )
}

plot_progress_bar <- function(input, output, session, D, currentPlayer, playerData) {
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
      ui <- div(
        p(paste(round(d3,2), "seconds from normal reaction time")),
        HTML(paste0('
          <div class="progress" style="height: 20px;">
            <div class="progress-bar" role="progressbar" style="width: ', result ,'%;" aria-valuenow="', result ,'" aria-valuemin="0" aria-valuemax="100">', result ,'%</div>
          </div>
        '))
      )
      return(ui)
    }
    else {
      #Check if a previous session with the same settings exists
      similarPreviousSession <- playerData$df %>%
        filter(sessionID != max(sessionID)) %>%
        merge(settingsLastSession) %>%
        filter(sessionID == max(sessionID))
      if(nrow(similarPreviousSession) == 0) {
        #UI to return
        ui <- div(
          p("No MPC data to compared"),
          p("No previous session with the same settings to compared")
        )
        return(ui)
      }
      else {
        #Compared the two sessions
        result <- similarPreviousSession[1,"sessionMedianReactionTime"] - lastSessionReactionTime 
        if (result > 0) {
          #UI to return
          ui <- div(
            p(paste("reaction time improved by", round(result, 2), "s"))
          )
          return(ui)
        }
        else if (result < 0) {
          #UI to return
          ui <- div(
            p(paste("reaction time decreased by", abs(round(result, 2)), "s"))
          )
          return(ui)
        }
        else {
          #UI to return
          ui <- div(
            p("reaction time unchanged")
          )
          return(ui)
        }
      }
    }
  })
}