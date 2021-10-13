page_individual_performance_over_time_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            plot_time_player_UI(ns("plot_time_player"))
  )
}

#server function
page_individual_performance_over_time <- function(input, output, session, currentPlayer, D) {
  callModule(plot_time_player, "plot_time_player", currentPlayer, D)
}
