page_profile_information_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              h1(textOutput(ns("nb_players"))),
              h2("players")
            ),
            fluidRow(
              h1(textOutput(ns("nb_players_stroke"))),
              h2("players stroke")
            )
  )
}

#server function
page_profile_information <- function(input, output, session, listPlayers, strokesPlayers) {
  
  output$nb_players <- renderText({
    validate(need(listPlayers(), "No data yet."))
    listPlayers_data <- listPlayers()
    paste(listPlayers_data %>% count())
  })
  
  output$nb_players_stroke <- renderText({
    validate(need(strokesPlayers(), "No data yet."))
    listStrokesPlayers_data <- strokesPlayers()
    paste(listStrokesPlayers_data %>% filter(stroke == "stroke") %>% count())
  })
  
}
