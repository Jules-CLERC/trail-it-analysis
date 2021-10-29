plot_compare_performance_players_UI <- function(id) {
  ns = NS(id)
  
  list(
    h1("Compare performances of players"),
    p("Select player(s)"),
    selectInput(ns("compare_player_select"),
                label = "Compare with players",
                choices = NULL,
                selected = NULL,
                multiple = TRUE),
    hr(),
    p("Select game type"),
    selectInput(ns("compare_player_gameType_select"),
                label = "Select game type",
                choices = NULL,
                selected = NULL),
    p("Select circle amount"),
    selectInput(ns("compare_player_circleAmount_select"),
                label = "Select circle amount",
                choices = NULL,
                selected = NULL),
    plotlyOutput(ns("plot_compare_performance_players_graph"))
  )
}

plot_compare_performance_players <- function(input, output, session, currentPlayer, D, listPlayers) {
  
  #Wait for data
  toListen <- reactive({
    list(listPlayers(), currentPlayer())
  })
  observeEvent(toListen(), {
    if(!is.null(listPlayers()) && !is.null(currentPlayer())) {
      
      #Get all the combination (gameType, CircleAmount) of the current player
      combinationsGameplays <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(gameType, circleAmount)
      
      #get players with a combination similar to the currentPlayer
      similarCombinationGameplays <- D() %>%
        filter(profileID != currentPlayer()) %>%
        distinct(profileID, gameType, circleAmount) %>%
        merge(combinationsGameplays) %>%
        distinct(profileID) %>%
        merge(listPlayers())
      
      unsimilarCombinationGameplays <- D() %>%
        filter(profileID != currentPlayer()) %>%
        distinct(profileID) %>%
        anti_join(similarCombinationGameplays, by.x="profileID") %>%
        merge(listPlayers())
      
      updateSelectInput(session, "compare_player_select",
                        choices = list(
                          'Similar gameplays' = similarCombinationGameplays[,"playerNameID"],
                          'Unsimilar gameplays' = unsimilarCombinationGameplays[,"playerNameID"]
                        ))
      
      #Get types of games played by the currentPlayer
      gameTypePlayer <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(gameType)

      updateSelectInput(session, "compare_player_gameType_select",
                        choices = gameTypePlayer[,"gameType"])
      
      #Get types of circle amount played by the currentPlayer
      circleAmountPlayer <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(circleAmount)

      updateSelectInput(session, "compare_player_circleAmount_select",
                        choices = circleAmountPlayer[,"circleAmount"])
    }
  })
  
  output$plot_compare_performance_players_graph<- renderPlotly({
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."), errorClass = "vis")
    
    GameplaysPlayer <- D() %>%
      filter(profileID == currentPlayer()) %>%
      filter(gameType == input$compare_player_gameType_select) %>%
      filter(circleAmount == input$compare_player_circleAmount_select)
    validate(need(nrow(GameplaysPlayer) > 0, "No data available for these parameters."), errorClass = "vis")
    
    #Check the players selected to the comparison
    listToCompare <- listPlayers() %>% 
      filter(!is.na(match(playerNameID, input$compare_player_select)) | profileID == currentPlayer()) %>%
      select(profileID)

    #Get the results of players selected
    dateComparePlayers = merge(D(), listToCompare, "profileID")
    dateComparePlayers = dateComparePlayers %>%
      filter(eventLabel == "Level 1 Completed!") %>%
      filter(gameType == input$compare_player_gameType_select) %>%
      filter(circleAmount == input$compare_player_circleAmount_select) %>%
      select(profileID, Timestamp, sessionMedianReactionTime, gameType, circleAmount, sessionLength) %>%
      arrange(Timestamp) %>%
      mutate(startTimestamp = first(Timestamp)) %>%
      group_by(profileID) %>%
      mutate(newTimestamp = Timestamp + difftime(startTimestamp, first(Timestamp))) %>%
      mutate(newTimestamp = difftime(newTimestamp, startTimestamp, units = "days")) %>%
      arrange(newTimestamp)
    
    fig <- plot_ly() %>% layout(
      xaxis = list(title = 'Number of days'),
      yaxis = list(title = 'Median reaction Time (s)'),
      legend = list(title=list(text='<b> Players </b>'))
    )
    for (i in 1:nrow(listToCompare)) {
      tmpProfile = listToCompare[i, "profileID"]
      tmpPlayerName = listPlayers() %>% filter(profileID == tmpProfile)
      fig <- fig %>% 
        add_trace(data = dateComparePlayers %>% filter(profileID == tmpProfile), x=~newTimestamp, y=~sessionMedianReactionTime, type = 'scatter', mode = 'lines+markers', name = tmpPlayerName["playerName"])
    }
    
    return(fig)
  }) 
}