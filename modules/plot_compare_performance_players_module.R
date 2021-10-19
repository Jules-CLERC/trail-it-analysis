plot_compare_performance_players_UI <- function(id) {
  ns = NS(id)
  
  list(
    selectInput(ns("compare_player_select"),
                label = "Compare with players",
                choices = NULL,
                selected = NULL,
                multiple = TRUE),
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
    }
  })
  
  output$plot_compare_performance_players_graph<- renderPlotly({
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."), errorClass = "vis")
    
    #Check the players selected to the comparison
    listToCompare <- listPlayers() %>% 
      filter(!is.na(match(playerNameID, input$compare_player_select))) %>%
      select(profileID)
    
    #Get the results of players selected and the current player
    dateComparePlayers = D() %>%
      filter(!is.na(match(profileID, listToCompare)) | profileID == currentPlayer()) %>%
      filter(eventLabel == "Level 1 Completed!") %>%
      group_by(profileID) %>%
      distinct(Timestamp, sessionMedianReactionTime)
    
    fig <- plot_ly(data = dateComparePlayers %>% filter(profileID == currentPlayer()), x=~Timestamp, y=~sessionMedianReactionTime, type = 'scatter', mode = 'lines')
    
    if(nrow(listToCompare) > 0) {
      for (profile in listToCompare) {
        fig <- fig %>% 
          add_trace(data = dateComparePlayers %>% filter(profileID == profile), x=~Timestamp, y=~sessionMedianReactionTime, type = 'scatter', mode = 'lines')
      }
    }
    
    return(fig)
  }) 
}