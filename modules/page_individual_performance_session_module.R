page_individual_performance_session_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            selectInput(ns("gameplay_player_select"),
                        label = "Select gameplay",
                        choices = NULL,
                        selected = NULL),
            plot_reaction_time_player_over_time_UI(ns("plot_reaction_time_player_over_time")),
            h3("Type game"),
            tableOutput(ns("infos_type_game")),
            h3("Reaction time"),
            tableOutput(ns("infos_reaction_time")),
            uiOutput(ns("nb_points_touched"))
  )
}

#server function
page_individual_performance_session <- function(input, output, session, currentPlayer, D) {
  
  currentGameplay <- reactiveValues(df = NULL)
  
  observeEvent(currentPlayer(), {
    #Check if a current player is selected
    if(!is.null(currentPlayer())) {
      #create list of all gamePlays dates
      #--> Get the first level of each gameplay
      listGameplays = D() %>%
        filter(profileID == currentPlayer()) %>%
        filter(eventLabel == "Level 1 Completed!") %>%
        pull(Timestamp)
      updateSelectInput(session, "gameplay_player_select",
                        choices = listGameplays)
    }
  })
  
  observeEvent(input$gameplay_player_select, {
    if(input$gameplay_player_select != "") {
      #get the row of the first level
      firstLevel <- D() %>%
        filter(profileID == currentPlayer()) %>%
        mutate(TimestampString = paste0(Timestamp,"")) %>%
        filter(TimestampString == input$gameplay_player_select)
      #get the session results
      currentGameplay$df <- D() %>%
        filter(profileID == currentPlayer()) %>%
        filter(Timestamp >= as.POSIXct(input$gameplay_player_select, format = "%Y-%m-%d %H:%M:%OS")) %>%
        arrange(Timestamp) %>%
        mutate(
          sessionID = ifelse(
            levelNumber > lag(levelNumber, default = 0), 0, 1
          ),
          sessionID = cumsum(sessionID)
        ) %>%
        filter(sessionID == 0)
    }
  })
  
  callModule(plot_reaction_time_player_over_time, "plot_reaction_time_player_over_time", reactive(currentGameplay$df))
  
  #type of game
  output$infos_type_game <- renderTable(colnames = FALSE, {
    validate(need(currentGameplay$df, "No current gameplay"))
    gameType = currentGameplay$df[1, "gameType"]
    circleAmount = currentGameplay$df[1, "circleAmount"]
    sessionLength = currentGameplay$df[1, "sessionLength"]
    
    table <- tibble(
      x = "Game type:", 
      y = gameType) %>%
      add_row(
        x = "Circle amount:", 
        y = toString(circleAmount)) %>%
      add_row(
        x = "Session length:", 
        y = paste(sessionLength, "min"))
    return(table)
  })
  
  #List reaction time
  output$infos_reaction_time <- renderTable(colnames = FALSE, {
    validate(need(currentGameplay$df, "No current gameplay"))
    bestReactionTime = round(currentGameplay$df[1, "sessionBestReactionTime"], 2)
    medianReactionTime = round(currentGameplay$df[1, "sessionMedianReactionTime"], 2)
    worstReactionTime = round(currentGameplay$df[1, "sessionWorstReactionTime"], 2)

    table <- tibble(
      x = "Best:", 
      y = paste(bestReactionTime, "s")) %>%
      add_row(
        x = "Median:", 
        y = paste(medianReactionTime, "s")) %>%
      add_row(
        x = "Worst:", 
        y = paste(worstReactionTime, "s"))
    return(table)
  })
  
  #Percentage points touched
  output$nb_points_touched <- renderUI({
    validate(need(currentGameplay$df, "No current gameplay"))
    
    dataPointsTouched <- currentGameplay$df %>%
      summarise(
        sumHits = sum(levelHitsTotal),
        sumErrors = sum(levelErrorsTotal)
      )
    result = ((dataPointsTouched[1,"sumHits"] - dataPointsTouched[1,"sumErrors"])/dataPointsTouched[1,"sumHits"]) * 100
    
    progress_text = paste("<strong>Number of points touched:</strong>",
                      sprintf("%.2f", result),
                      "%")
    progress_bar = tags$span("",class="mini-progress-bar-fill",style=paste0("width:", result, "%;"))
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      progress_text,
      "</p>",
      "<div class='mini-progress-bar' style='width:100px;'>",
      progress_bar,
      "</div><br>"
    ))
    return(ui)
  })  
}
