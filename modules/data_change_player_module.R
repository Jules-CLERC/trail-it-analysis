#UI function
data_change_player_UI <- function(id) {
  ns = NS(id)
  list(
    fluidPage(
    HTML("<h3>Select and Edit Data</h3>",
         "<p>Here you can switch which data record is being used,
         remove records and upload new records.</p>"),
    uiOutput(ns("sessionList"))
    )
  )
}

#server function
data_change_player <- function(input, output, session, listPlayers, currentPlayer) {
  ns <- session$ns
  
  observeEvent(listPlayers(), {
    validate(need(listPlayers(), "No data yet."))
    
    output$sessionList <- renderUI({
      lapply(1:nrow(listPlayers()), function(i) {
        data_select_row_player_UI(ns(listPlayers()[i, "profileID"]))
      })
    })
    
    lapply(1:nrow(listPlayers()), function(i) {
      callModule(data_select_row_player, 
                 listPlayers()[i, "profileID"], 
                 listPlayers()[i, "playerNameID"],
                 listPlayers()[i, "profileID"],
                 currentPlayer)
    })
  })
}