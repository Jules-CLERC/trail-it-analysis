#UI function
data_select_player_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    column(2, actionButton(ns("changeCurrentPlayer"), "Change current player")),
    column(4, textOutput(ns("currentPlayerOutput")))
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
    validate(need(listPlayers(), "No data yet."))
    
    dataCurrentPlayer <- listPlayers() %>%
      filter(profileID == currentPlayer())
    
    ymd = as.Date(dataCurrentPlayer$Timestamp, format = "%Y-%m-%d %H:%M:%OS")
    strDate = paste0(ymd(ymd), ", ", format(strptime(dataCurrentPlayer$Timestamp, format = "%Y-%m-%d %H:%M:%OS"), '%H:%M'))
    
    text = paste0(dataCurrentPlayer$playerName, " (", strDate, ")")
    return(text)
  })
  
  ns <- session$ns
  observeEvent(input$changeCurrentPlayer, {
    insertUI(selector = "#changeCurrentPlayer", where = "afterEnd",
             ui = showModal(modalDialog(data_change_player_UI(ns("data_change_player")), easyClose = TRUE)))
  })
  
  changePlayer <- callModule(data_change_player, "data_change_player", listPlayers)
  observeEvent(changePlayer$trigger, {
    req(changePlayer$trigger > 0)
    toReturn$df <-  changePlayer$df$profileID
    toReturn$trigger <- toReturn$trigger + 1
  })
  
  return(toReturn)
}