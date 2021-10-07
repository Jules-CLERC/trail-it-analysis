page_times_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              h1("For how much time did each player play ?"),
              plot_minutes_by_player_UI(ns("minutes_by_player")),
              plot_days_by_player_UI(ns("days_by_player")),
              plot_frequency_year_by_month_player_UI(ns("frequency_year_by_month_player"))
            )
  )
}

#server function
page_times_players <- function(input, output, session, timesGamesPlayers) {
  callModule(plot_minutes_by_player, "minutes_by_player", timesGamesPlayers)
  callModule(plot_days_by_player, "days_by_player", timesGamesPlayers)
  callModule(plot_frequency_year_by_month_player, "frequency_year_by_month_player", timesGamesPlayers)
}
