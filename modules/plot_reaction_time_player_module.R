plot_reaction_time_player_UI <- function(id) {
  ns = NS(id)
  
  list(
    textOutput(ns("result")),
    selectInput(ns("gameplay_player_select"),
                label = "Select gameplay",
                choices = NULL,
                selected = NULL)
  )
}

plot_reaction_time_player <- function(input, output, session, currentPlayer, D) {
  observeEvent(currentPlayer(), {
    if(!is.null(currentPlayer())) {
      #create list of all players except the current player
      listGameplays = D() %>%
        filter(profileID == currentPlayer()) %>%
        filter(eventLabel == "Level 1 Completed!") %>%
        subset(select = -c(id)) %>%
        distinct() %>%
        select(Timestamp, gameType)
      view(listGameplays)
      
      my_list2 = c("Cylinders" = "cyl",
        "Transmission" = "am",
        "Gears" = "gear")

      choicesGameplays <- lapply(listGameplays["Timestamp"], as.character)
      print(typeof(choicesGameplays))
      
      names(choicesGameplays) <- lapply(listGameplays["gameType"], as.character)
      
      updateSelectInput(session, "gameplay_player_select",
                        choices = choicesGameplays)
    }
  })
  
  output$result <- renderText({
    paste("You chose", input$gameplay_player_select)
  })
}