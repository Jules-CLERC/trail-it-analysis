#UI function
calc_nb_players_UI <- function(id) {
  ns <- NS(id)
  
  list(fluidRow(
    h2("How many players are there in total?"),
    textOutput(ns("nb_players"))
  ))
}

#server function
calc_nb_players <- function(input, output, session, listPlayers) {
  nbPlayers = listPlayers %>% count()
  output$nb_players <- renderText({
    paste("Number of players : ", nbPlayers)
  })
}