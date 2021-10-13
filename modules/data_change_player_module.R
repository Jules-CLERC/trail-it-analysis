#UI function
data_change_player_UI <- function(id) {
  ns = NS(id)
  list(
    fluidPage(
    HTML("<h3>Select Data</h3>"),
    uiOutput(ns("sessionList"))
    )
  )
}

#server function
data_change_player <- function(input, output, session, listPlayers) {
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  ns <- session$ns
  
  observeEvent(listPlayers(), {
    output$sessionList <- renderUI({
      validate(need(listPlayers(), "No data yet."))
      lapply(1:nrow(listPlayers()), function(i) {
        data_select_row_player_UI(ns(listPlayers()[i, "profileID"]))
      })
    })
    
    lapply(1:nrow(listPlayers()), function(i) {
      validate(need(listPlayers(), "No data yet."))
      selectPlayer <- callModule(data_select_row_player,
                 listPlayers()[i, "profileID"],
                 listPlayers()[i, "playerNameID"],
                 listPlayers()[i, "profileID"])
      observeEvent(selectPlayer$trigger, {
        req(selectPlayer$trigger > 0)
        toReturn$df <-  selectPlayer$df
        toReturn$trigger <- toReturn$trigger + 1
      })
    })
  })
  return(toReturn)
}