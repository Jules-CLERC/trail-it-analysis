page_individual_performance_session_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            plot_reaction_time_player_UI(ns("plot_reaction_time_player"))
  )
}

#server function
page_individual_performance_session <- function(input, output, session, currentPlayer, D) {
  callModule(plot_reaction_time_player, "plot_reaction_time_player", currentPlayer, D)
}
