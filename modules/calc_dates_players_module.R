#UI function
calc_dates_players_UI <- function(id) {
  ns <- NS(id)
  
  list(fluidRow(
    h2("On which dates did the players play?"),
    selectInput(ns("dates_player_select"),
                label = "Choose a player",
                choices = NULL,
                selected = NULL),
    dataTableOutput(ns('dates_player_table'))
  ))
}

#server function
calc_dates_players <- function(input, output, session, D, listPlayers) {
  datesPlayers = D %>%
    select(profileID, date) %>%
    distinct(profileID, date) %>%
    arrange(profileID, date)
  datesPlayers = merge(datesPlayers, listPlayers, "profileID")
  
  #Add data to the select input
  updateSelectInput(session, "dates_player_select",
                    choices = listPlayers["playerNameID"])

  #show the dates of a playerNameID
  output$dates_player_table <- renderDataTable(
      datesPlayers %>% filter(playerNameID == input$dates_player_select) %>% select(date)
      )
  
  toReturn <- reactiveValues(
    variable = datesPlayers,
    variable_name = "datesPlayers",
    trigger = 0
  )
  return(toReturn)
}