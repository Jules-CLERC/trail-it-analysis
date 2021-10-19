page_trends_statistics_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            tableOutput(ns("training_time_popularity")),
            tableOutput(ns("difficulty_popularity")),
            tableOutput(ns("gamemode_popularity"))
  )
}

#server function
page_trends_statistics_players <- function(input, output, session, D, strokesPlayers) {
  #TODO : put the plots modules
  output$training_time_popularity<- renderTable({
    validate(need(D(), "No data."))
    validate(need(strokesPlayers(), "No data."))
    
    onlyStrokePlayer = strokesPlayers() %>%
      filter(stroke == "stroke")

    DStrokesPlayers = merge(D(), onlyStrokePlayer, "profileID")

    trainingTime = DStrokesPlayers %>%
      distinct(profileID, sessionLength) %>%
      group_by(sessionLength) %>%
      count() %>%
      ungroup() %>%
      mutate(popularityPercent = (n / sum(n)) * 100)

    return(trainingTime)
  })
  
  output$difficulty_popularity<- renderTable({
    validate(need(D(), "No data."))
    validate(need(strokesPlayers(), "No data."))
    
    onlyStrokePlayer = strokesPlayers() %>%
      filter(stroke == "stroke")
    
    DStrokesPlayers = merge(D(), onlyStrokePlayer, "profileID")
    
    difficulties = DStrokesPlayers %>%
      distinct(profileID, circleAmount) %>%
      group_by(circleAmount) %>%
      count() %>%
      ungroup() %>%
      mutate(popularityPercent = (n / sum(n)) * 100)
    
    return(difficulties)
  })
  
  output$gamemode_popularity<- renderTable({
    validate(need(D(), "No data."))
    validate(need(strokesPlayers(), "No data."))
    
    onlyStrokePlayer = strokesPlayers() %>%
      filter(stroke == "stroke")
    
    DStrokesPlayers = merge(D(), onlyStrokePlayer, "profileID")
    
    gamemodes = DStrokesPlayers %>%
      distinct(profileID, gameType) %>%
      group_by(gameType) %>%
      count() %>%
      ungroup() %>%
      mutate(popularityPercent = (n / sum(n)) * 100)
    
    return(gamemodes)
  })
}
