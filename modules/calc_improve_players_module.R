#UI function
calc_improve_players_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title="Did the players improve their overall reaction time?",
      status="primary",
      solidHeader = TRUE,
      width = "100%",
      dataTableOutput(ns('improve_players_table'))
    )
  )
}

#server function
calc_improve_players <- function(input, output, session, D, listPlayers) {
  #Did the players improve their overall reaction time?
  #TODO : change the calculation of the improve
  avgReactionTime = D %>%
    select(profileID, date, sessionMedianReactionTime) %>%
    group_by(profileID) %>%
    summarise(avgReactionTime = mean(sessionMedianReactionTime))
  
  lastReactionTime = D %>%
    select(profileID, date, time, sessionMedianReactionTime) %>%
    group_by(profileID) %>%
    mutate(Timestamp = paste(date,time),
           Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    arrange(Timestamp) %>%
    filter(time == max(time)) %>%
    summarise(lastReactionTime = min(sessionMedianReactionTime))
  
  improveReactionTime = merge(avgReactionTime, lastReactionTime, "profileID") %>%
    mutate(isImprove = ifelse(avgReactionTime > lastReactionTime, "Improve", "Not improve"))
  improveReactionTime = improveReactionTime[c(1,4)]
  
  #Show the data
  output$improve_players_table <- renderDataTable(
    merge(improveReactionTime, listPlayers, "profileID")
  )
  
  toReturn <- reactiveValues(
    variable = improveReactionTime,
    variable_name = "improveReactionTime",
    trigger = 0
  )
  return(toReturn)
}