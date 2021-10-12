plot_minutes_by_player_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      plotlyOutput(ns("time_minutes_players_graph"))
    )
  )
}

plot_minutes_by_player <- function(input, output, session, timesGamesPlayers) {
  
  output$time_minutes_players_graph<- renderPlotly({
    validate(need(timesGamesPlayers(), "Waiting for data."), errorClass = "vis")
    plot_ly() %>%
      add_trace(data = timesGamesPlayers(), x=~playerNameID, y=~nbMinutes, type = 'bar', name=~stroke)
  })
  
}