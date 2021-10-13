#UI function
data_select_player_UI <- function(id) {
  ns = NS(id)
  list(
    actionButton(ns("changeCurrentPlayer"), "Change current player"),
    textOutput(ns("currentPlayerOutput"))
  )
}

#server function
data_select_player <- function(input, output, session, listPlayers, currentPlayer) {
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  output$currentPlayerOutput <- renderText({
    validate(need(currentPlayer(), "No data yet."))
    paste("Data from",
          listPlayers() %>%
            filter(profileID == currentPlayer()) %>%
            select(playerNameID))
  })
  
  ns <- session$ns
  observeEvent(input$changeCurrentPlayer, {
    insertUI(selector = "#changeCurrentPlayer", where = "afterEnd",
             ui = showModal(modalDialog(data_change_player_UI(ns("data_change_player")), easyClose = TRUE)))
  })
  
  changePlayer <- callModule(data_change_player, "data_change_player", listPlayers)
  observeEvent(changePlayer$trigger, {
    req(changePlayer$trigger > 0)
    toReturn$df <-  changePlayer$df
    toReturn$trigger <- toReturn$trigger + 1
  })
  
  return(toReturn)
}