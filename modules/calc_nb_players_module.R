#UI function
calc_nb_players_UI <- function(id) {
  ns <- NS(id)
  valueBox(
    "Players", 
    h2(textOutput(ns("nb_players"))), 
    icon = icon("user")
  )
}

#server function
calc_nb_players <- function(input, output, session, listPlayers) {
  nbPlayers = listPlayers %>% count()
  
  #Show the result
  output$nb_players <- renderText({
    paste(nbPlayers)
  })
}