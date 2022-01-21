page_individual_profile_information_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              column(6,
                     style="padding-left: 0px !important;",
                     column(2, style="padding-left: 0px !important;",
                            img(src='user.png', align = "left", id="user-image")),
                     column(8, uiOutput(ns("player_header")))
              )
            ),
            br(),
            fluidRow(
              column(6,
                     uiOutput(ns("player_performance_summary"))
              ),
              column(6,
                     plot_progress_bar_UI(ns("plot_progress_bar"))
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
    
    ui <- div(
        h1(playerName),
        p(lastDatePlay)
    )
    return(ui)
  })
  
  output$player_performance_summary <- renderUI({
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
    ui <- fluidRow(
      column(3, style="padding-left: 0px !important;",
             column(12, h1(lastSessionReactionTime)),
             column(12, p("Reaction time"))
      ),
      column(3,
             column(12, 
                    div(
                      id="label-percentage-hits",
                      style= paste("background-color:", color),
                      p(paste0(resultPercentageHits,"%"))
                    )),
             column(12, h1(paste0(lastPercentageHits, "%"))),
             column(12, p("Hits"))
      )
    )
    return(ui)
  })
  
  output$player_characteristics <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    ui <- div(
      p(paste("Age:",
              as.character(playerData$df[1, "ageGroup"]))),
      p(paste("Training reason:", 
              as.character(playerData$df[1, "trainingReason"]))),
      p(paste("Email:", 
              as.character(playerData$df[1, "email"]))),
      p(paste("Play context:", 
              as.character(playerData$df[1, "playContext"])))
    )
    
    return(ui)
  })
  
  callModule(plot_progress_bar, "plot_progress_bar", D, currentPlayer, playerData)
  
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
      ui <- div(
        HTML(strDates),
        modal_individual_profile_dates_UI(ns("modal_individual_profile_dates"))
      )
    }
    else {
      ui <- div(
        HTML(strDates)
      )
    }
    return(ui)
  })
}
