page_improves_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              title="Did the players improve their overall reaction time?",
              status="primary",
              solidHeader = TRUE,
              width = "100%",
              dataTableOutput(ns('improve_players_table'))
            )
  )
}

#server function
page_improves_players <- function(input, output, session, listPlayers, improvesPlayers) {
  
  output$improve_players_table <- renderDataTable({
    validate(need(listPlayers(), "No data yet."))
    merge(improvesPlayers(), listPlayers(), "profileID")
  })
  
}
