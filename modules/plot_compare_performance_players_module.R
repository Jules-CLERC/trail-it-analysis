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
      #create list of all players except the current player
      listComparePlayers <- listPlayers() %>%
        filter(profileID != currentPlayer())
      updateSelectInput(session, "compare_player_select",
                        choices = listComparePlayers["playerNameID"])
      
      #Get types of games played by the currentPlayer
      gameTypePlayer <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(gameType)
      #I need to do a special case when there is only one choice, because otherwise the choice is not detected
      if(nrow(gameTypePlayer["gameType"]) == 1) {
        updateSelectInput(session, "compare_player_gameType_select",
                          choices = gameTypePlayer[1,"gameType"])
      }
      else {
        updateSelectInput(session, "compare_player_gameType_select",
                          choices = gameTypePlayer["gameType"])
      }
      
      #Get types of circle amount played by the currentPlayer
      circleAmountPlayer <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(circleAmount)
      #I need to do a special case when there is only one choice, because otherwise the choice is not detected
      if(nrow(circleAmountPlayer["circleAmount"]) == 1) {
        updateSelectInput(session, "compare_player_circleAmount_select",
                          choices = circleAmountPlayer[1,"circleAmount"])
      }
      else {
        updateSelectInput(session, "compare_player_circleAmount_select",
                          choices = circleAmountPlayer["circleAmount"])
      }
    }
  })
  
  output$plot_compare_performance_players_graph<- renderPlotly({
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."), errorClass = "vis")
    
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