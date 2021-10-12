page_dates_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              h1("On which dates did the players play?"),
              selectInput(ns("dates_player_select"),
                                        label = "Choose a player",
                                        choices = NULL,
                                        selected = NULL),
              dataTableOutput(ns('dates_player_table'))
            )
  )
}

#server function
page_dates_players <- function(input, output, session, listPlayers, datesPlayers) {

  #Add data to the select input
  toListen <- reactive({
    list(listPlayers(),datesPlayers())
  })
  observeEvent(toListen(), {
    if(!is.null(listPlayers()) && !is.null(datesPlayers())) {
      updateSelectInput(session, "dates_player_select",
                        choices = listPlayers()["playerNameID"])
    }
  })
  
  #show the dates of a playerNameID
  output$dates_player_table <- renderDataTable({
    validate(need(datesPlayers(), "No data yet."))
    datesPlayers() %>% filter(playerNameID == input$dates_player_select) %>% select(date)
  })
}
