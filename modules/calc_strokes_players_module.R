#UI function
calc_strokes_players_UI <- function(id) {
  ns <- NS(id)
  
  list(fluidRow(
    h2("How many of the players are stroke patients?"),
    textOutput(ns("nb_players_stroke"))
  ))
}

#server function
calc_strokes_players <- function(input, output, session, D, listPlayers) {
  #How many of the players are stroke patients?
  strokesPlayers = D %>% 
    select(profileID, trainingReason) %>%
    filter(!(trainingReason == "OtherReason")) %>%
    distinct(profileID) %>%
    mutate(stroke = "stroke")
  
  strokesPlayers = merge(strokesPlayers, listPlayers, "profileID", all.y = TRUE)
  strokesPlayers = strokesPlayers[c(1,2)]
  strokesPlayers[is.na(strokesPlayers)] <- "no stroke"
  
  output$nb_players_stroke <- renderText({
    paste("Number of players stroke : ", strokesPlayers %>% count())
  })
  
  toReturn <- reactiveValues(
    variable = strokesPlayers,
    variable_name = "strokesPlayers",
    trigger = 0
  )
  return(toReturn)
}