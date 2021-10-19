page_trends_performance_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            plot_compare_performance_players_UI(ns("plot_compare_performance_players"))
  )
}

#server function
page_trends_performance_players <- function(input, output, session, currentPlayer, D, listPlayers) {
  callModule(plot_compare_performance_players, "plot_compare_performance_players", currentPlayer, D, listPlayers)
}
