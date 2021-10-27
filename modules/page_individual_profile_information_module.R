page_individual_profile_information_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              column(6,
                     h1(uiOutput(ns("player_header")))
              )
            ),
            fluidRow(
              column(7,
                     tags$h3("Player Characteristics"),
                     tableOutput(ns("profile_info"))
              ),
              column(5,
                     tags$h3("Play History"),
                     tableOutput(ns("play_history"))
              ),
            )
  )
}

#server function
page_individual_profile_information <- function(input, output, session, D, currentPlayer, listPlayers, timesGamesPlayers, datesPlayers) {
  
  output$player_header <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    listPlayers() %>%
      filter(profileID == currentPlayer()) %>%
      select(playerName)
  })
  
  output$profile_info <- renderTable(colnames = FALSE, {
    validate(need(currentPlayer(), "No current player."))
    
    dataPlayer = D() %>% 
      filter(profileID == currentPlayer()) %>%
      filter(Timestamp == max(Timestamp))
    
    minutesPlayer = timesGamesPlayers() %>%
      filter(profileID == currentPlayer()) %>%
      select(nbMinutes)
    
    table <- tibble(
        x = "ProfileID:", 
        y = as.character(dataPlayer[1, "profileID"])) %>%
      add_row(
        x = "Email:", 
        y = as.character(dataPlayer[1, "email"])) %>%
      add_row(
        x = "Last date play:", 
        y = as.character(paste0(
          dataPlayer[1, "date"],
          " ~",
          as.numeric(
            as.duration(today() - ymd(dataPlayer[1, "date"])),"days"
          ),
          " day(s)"
        ))) %>%
      add_row(
        x = "Play context:", 
        y = as.character(dataPlayer[1, "playContext"])) %>%
      add_row(
        x = "Training reason:", 
        y = as.character(dataPlayer[1, "trainingReason"])) %>%
      add_row(
        x = "Age group:", 
        y = as.character(dataPlayer[1, "ageGroup"])) %>%
      add_row(
        x = "Time play:", 
        y = as.character(minutes(as.integer(minutesPlayer[1, "nbMinutes"]))))
    return(table)
  })
  
  output$play_history <- renderTable(colnames = FALSE, {
    validate(need(currentPlayer(), "No current player."))
    
    datesPlayers() %>% 
      filter(profileID == currentPlayer()) %>%
      mutate(p_wday = wday(as.character(date), abbr = F, label=T),
             p_date = format(date(as.character(date)), "%d %b %Y"),
             p_timestamp = paste(p_wday, p_date)) %>%
      select(date, p_timestamp)
  })
}
