plot_frequency_year_by_month_player_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      plotlyOutput(ns("time_year_by_month_players_graph"))
    )
  )
}

plot_frequency_year_by_month_player <- function(input, output, session, timesGamesPlayers) {
  
  output$time_year_by_month_players_graph<- renderPlotly({
    validate(need(timesGamesPlayers(), "Waiting for data."))
    plot_ly() %>%
      add_trace(data = timesGamesPlayers(), x=~nbYears / nbMonths, y=~playerNameID, type = 'bar', name=~isImprove)
  })
  
}