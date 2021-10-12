plot_days_by_player_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      plotlyOutput(ns("time_days_players_graph"))
    )
  )
}

plot_days_by_player <- function(input, output, session, timesGamesPlayers) {
  
  output$time_days_players_graph<- renderPlotly({
    validate(need(timesGamesPlayers(), "Waiting for data."), errorClass = "vis")
    plot_ly() %>%
      add_trace(data = timesGamesPlayers(), x=~playerNameID, y=~nbDays, type = 'bar', name=~stroke)
  })
  
}