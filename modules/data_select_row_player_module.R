#UI function
data_select_row_player_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(id=ns("row"),
             column(9, uiOutput(ns("sessionText"))),
             column(3, actionButton(ns("actionChoose"), "Choose"))
    )
  )
}

#server function
data_select_row_player <- function(input, output, session, playerNameID, profileID, currentPlayer) {
  output$sessionText <- renderUI({
    paste(playerNameID)
  })
  observeEvent(input$actionChoose, {
    currentPlayer$df <- profileID
    currentPlayer$trigger <- currentPlayer$trigger + 1
  })
}
