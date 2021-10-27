page_individual_performance_over_time_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            h1("Animation : Reaction time by section for each session"),
            plot_reaction_time_player_over_time_UI(ns("plot_reaction_time_player_over_time")),
            hr(),
            h1("Play frequency"),
            plot_time_player_UI(ns("plot_time_player"))
  )
}

#server function
page_individual_performance_over_time <- function(input, output, session, currentPlayer, D) {
  
  gameplaysPlayer <- reactiveValues(df = NULL)
  
  observeEvent(currentPlayer(), {
    #Check if a current player is selected
    if(!is.null(currentPlayer())) {
      gameplaysPlayer$df <- D() %>%
        filter(profileID == currentPlayer()) %>%
        arrange(Timestamp) %>%
        mutate(
          sessionID = ifelse(
            levelNumber > lag(levelNumber, default = 0), 0, 1
          ),
          sessionID = cumsum(sessionID)
        )
    }
  })
  
  callModule(plot_reaction_time_player_over_time, "plot_reaction_time_player_over_time", reactive(gameplaysPlayer$df))
  callModule(plot_time_player, "plot_time_player", currentPlayer, D)
}
